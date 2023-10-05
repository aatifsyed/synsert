use std::{cmp::Reverse, fmt, ops::RangeInclusive};

use proc_macro2::{LineColumn, Span};
use rangemap::RangeInclusiveMap;
use ropey::{Rope, RopeSlice};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Operation {
    Append(String),
    Prepend(String),
    Replace(String),
    Remove,
}

/// Keeps track of edits so you can apply them later.
pub struct Synsert {
    source_code: Rope,
    /// Detect edit collisions.
    ///
    /// char-indexed.
    edits: RangeInclusiveMap<usize, Operation>,
}

impl Synsert {
    pub fn new(source_code: &str) -> Self {
        Self {
            source_code: Rope::from_str(source_code),
            edits: RangeInclusiveMap::new(),
        }
    }
    /// # Panics
    /// - if `span` contains an out-of-bounds line or column for `source_code`.
    /// - if [`Span::source_text`] returns a different string to the one indexed into ours.
    /// - if there is already an edit that overlaps with this `span`.
    pub fn queue_edit_at(&mut self, span: Span, operation: Operation) {
        // proc-macro2::Span's line's are 1-indexed
        // ropey::Rope's lines are 0-indexed

        let num_lines = self.source_code.len_lines();
        assert!(
            num_lines >= span.end().line,
            "span exceeds end of file. span is {} but there are {} lines",
            FmtSpan(span),
            num_lines
        );

        let start = span.start();
        self.assert_column(start, span);
        let start = self.source_code.line_to_char(start.line - 1) + start.column;

        let end = span.end();
        self.assert_column(end, span);
        let end = self.source_code.line_to_char(end.line - 1) + end.column - 1; // to inclusive

        assert!(
            !self.edits.overlaps(&(start..=end)),
            "an edit has already been made in the range {}",
            FmtSpan(span)
        );

        #[cfg(never)] // see bug below
        if let Some(reported) = span.source_text() {
            let internal = self.source_code.slice(start..=end);
            assert_eq!(
                internal, reported,
                "the span's reported source text does not match our calculated source text"
            )
        }

        self.edits.insert(start..=end, operation)
    }

    fn assert_column(&self, coord: LineColumn, span: Span) {
        let num_cols = self.source_code.line(coord.line - 1).len_chars();
        assert!(
            num_cols > coord.column,
            "span exceeds end of line. span is {} but there are {} columns",
            FmtSpan(span),
            num_cols
        );
    }

    /// It is a logic error to apply edits in an arbitrary order - early edits may interfere with later edits.
    ///
    /// # Panics
    /// - if `range` is out-of-bounds. This is true for any edit retained by this struct.
    pub fn apply(&mut self, range: &RangeInclusive<usize>, operation: &Operation) {
        match operation {
            Operation::Prepend(it) => self.source_code.insert(*range.start(), it),
            Operation::Append(it) => self.source_code.insert(range.end() + 1, it),
            Operation::Replace(it) => {
                self.source_code.remove(range.clone());
                self.source_code.insert(*range.start(), it)
            }
            Operation::Remove => self.source_code.remove(range.clone()),
        }
    }
    /// Get the current, possibly edited state of the source code.
    pub fn source_code(&self) -> RopeSlice {
        self.source_code.slice(..)
    }
    /// Latest edit in the file first.
    pub fn edits_ordered(&self) -> Vec<(RangeInclusive<usize>, Operation)> {
        let mut edits = self.edits.clone().into_iter().collect::<Vec<_>>();
        edits.sort_by_key(|(range, _op)| Reverse(*range.start()));
        edits
    }
    /// Apply all the edits, returning the result.
    pub fn apply_all(mut self) -> String {
        for (range, operation) in self.edits_ordered() {
            self.apply(&range, &operation)
        }
        self.source_code.into()
    }
    /// Convenience method for [`Self::queue_edit_at`] with [`Operation::Replace`].
    pub fn replace(&mut self, span: Span, new: impl Into<String>) {
        self.queue_edit_at(span, Operation::Replace(new.into()))
    }
    /// Convenience method for [`Self::queue_edit_at`] with [`Operation::Prepend`].
    pub fn prepend(&mut self, span: Span, new: impl Into<String>) {
        self.queue_edit_at(span, Operation::Prepend(new.into()))
    }
    /// Convenience method for [`Self::queue_edit_at`] with [`Operation::Append`].
    pub fn append(&mut self, span: Span, new: impl Into<String>) {
        self.queue_edit_at(span, Operation::Append(new.into()))
    }
    /// Convenience method for [`Self::queue_edit_at`] with [`Operation::Remove`].
    pub fn remove(&mut self, span: Span) {
        self.queue_edit_at(span, Operation::Remove)
    }
}

struct FmtSpan(Span);

impl fmt::Display for FmtSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let start = self.0.start();
        let end = self.0.end();
        f.write_fmt(format_args!(
            "{}:{}..{}:{}",
            start.line, start.column, end.line, end.column
        ))
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use pretty_assertions::assert_str_eq;
    use syn::spanned::Spanned as _;

    #[test]
    fn proc_macro2_source_text_is_correct_for_single_byte() {
        let source_code = "const FOO: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        assert_eq!("FOO", ast.ident.span().source_text().unwrap())
    }

    /// https://github.com/dtolnay/proc-macro2/issues/408
    #[test]
    #[should_panic = "is not a char boundary"]
    fn proc_macro2_source_text_is_incorrect_for_multibyte() {
        let source_code = "const 𓀕: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        let reported = ast.ident.span().source_text(); // boom
        assert_eq!("𓀕", reported.unwrap())
    }

    #[test]
    fn single_line_single_edit() {
        let source_code = "const FOO: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Replace(String::from("BAR")));
        assert_str_eq!(synsert.apply_all(), "const BAR: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("TO")));
        assert_str_eq!(synsert.apply_all(), "const TOFOO: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("BAR")));
        assert_str_eq!(synsert.apply_all(), "const FOOBAR: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Remove);
        assert_str_eq!(synsert.apply_all(), "const : () = ();");
    }

    #[test]
    fn single_line_single_edit_multibyte() {
        let source_code = "const 𓀕: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.expr.span(), Operation::Replace(String::from("𓀠")));
        assert_str_eq!(synsert.apply_all(), "const 𓀕: () = 𓀠;");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("𓀠")));
        assert_str_eq!(synsert.apply_all(), "const 𓀠𓀕: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("𓀠")));
        assert_str_eq!(synsert.apply_all(), "const 𓀕𓀠: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ty.span(), Operation::Remove);
        assert_str_eq!(synsert.apply_all(), "const 𓀕:  = ();");
    }
}
