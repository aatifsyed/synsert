//! A primitive for programatically editing files using [`syn`].
//!
//! `syn` is the de-facto standard for parsing Rust. Its syntax tree is easy to
//! use, but it is lossy - if you parse a file, edit it with syn, and unparse
//! it, you'll lose all your comments and spacing (for example).
//!
//! [Rust Analyzer's syntax crate](https://docs.rs/ra_ap_syntax) has a lossless
//! syntax tree, which powers IDE assists, but it's far more difficult to use.
//!
//! [`Editor`] allows you to use `syn`'s syntax tree to write your Structured
//! Search and Replace tools, or IDE assists.
//!
//! ```
//! let source_code = "const NUM: usize = 1;"; // get the source text
//!
//! // create an AST and a helper struct from the same source code
//! let (mut editor, ast) = synsert::Editor::new_with_ast::<syn::ItemConst>(source_code).unwrap();
//!
//! let edited = editor
//!     .append(ast.ident, "_YAKS")
//!     .replace(ast.expr, "9001")
//!     .finish();
//!
//! assert_eq!(edited, "const NUM_YAKS: usize = 9001;");
//! ```
//!
//! See the examples for a more in-depth case using a [`syn::visit::Visit`](https://docs.rs/syn/latest/syn/visit/index.html)or

use std::{cmp::Reverse, fmt, ops::RangeInclusive};

use proc_macro2::{LineColumn, Span};
use rangemap::RangeInclusiveMap;
use ropey::Rope;
use syn::{parse::Parse, spanned::Spanned};

/// Keeps track of edits so you can apply them correctly all-at-once with [`finish`](Editor::finish)
///
/// See [module documentation](mod@self) for more.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Editor {
    source_code: Rope,
    /// Detect edit collisions.
    ///
    /// char-indexed.
    edits: RangeInclusiveMap<usize, Operation>,
}

impl Editor {
    /// Parse `source_code` into the given type, and create a new editor over that source code.
    pub fn new_with_ast<T: Parse>(source_code: &str) -> syn::Result<(Self, T)> {
        syn::parse_str::<T>(source_code).map(|ast| (Self::new(source_code), ast))
    }
    /// Create a new editor.
    ///
    /// Syntax tree types used with this editor must be from the same source.
    pub fn new(source_code: &str) -> Self {
        Self {
            source_code: Rope::from_str(source_code),
            edits: RangeInclusiveMap::new(),
        }
    }
    /// Replace `node` with `text`.
    ///
    /// ```
    /// # use synsert::Editor;
    /// let (mut editor, ast) = Editor::new_with_ast::<syn::TraitItemFn>("fn shave_yaks();").unwrap();
    /// assert_eq!(editor.replace(ast.sig.ident, "write_code").finish(), "fn write_code();")
    /// ```
    ///
    /// # Panics
    /// - may panic if `node` is from a difference source text than this `Editor`.
    /// - if there is a conflict (i.e this node, or a parent of it has already been edited).
    pub fn replace(&mut self, node: impl Spanned, text: impl Into<String>) -> &mut Self {
        self.queue_edit_at(node.span(), Operation::Replace(text.into()))
    }
    /// Put `text` before `node`.
    ///
    /// ```
    /// # use synsert::Editor;
    /// let (mut editor, ast) = Editor::new_with_ast::<syn::ExprCast>("ate as u8").unwrap();
    /// assert_eq!(editor.prepend(ast.expr, "i_").finish(), "i_ate as u8")
    /// ```
    ///
    /// # Panics
    /// - may panic if `node` is from a difference source text than this `Editor`.
    /// - if there is a conflict (i.e this node, or a parent of it has already been edited).
    pub fn prepend(&mut self, node: impl Spanned, text: impl Into<String>) -> &mut Self {
        self.queue_edit_at(node.span(), Operation::Prepend(text.into()))
    }
    /// Put `text` after `node`.
    ///
    /// ```
    /// # use synsert::Editor;
    /// let (mut editor, ast) = Editor::new_with_ast::<syn::Expr>("maybe").unwrap();
    /// assert_eq!(editor.append(ast, "?").finish(), "maybe?")
    /// ```
    ///
    /// # Panics
    /// - may panic if `node` is from a difference source text than this `Editor`.
    /// - if there is a conflict (i.e this node, or a parent of it has already been edited).
    pub fn append(&mut self, node: impl Spanned, text: impl Into<String>) -> &mut Self {
        self.queue_edit_at(node.span(), Operation::Append(text.into()))
    }
    /// Remove the text for `node`.
    ///
    /// ```
    /// # use synsert::Editor;
    /// let (mut editor, ast) = Editor::new_with_ast::<syn::ExprArray>("[going, going, gone]").unwrap();
    /// assert_eq!(editor.remove(&ast.elems[2]).finish(), "[going, going, ]")
    /// ```
    ///
    /// # Panics
    /// - may panic if `node` is from a difference source text than this `Editor`.
    /// - if there is a conflict (i.e this node, or a parent of it has already been edited).
    pub fn remove(&mut self, node: impl Spanned) -> &mut Self {
        self.queue_edit_at(node.span(), Operation::Remove)
    }
    /// # Panics
    /// - if `span` contains an out-of-bounds line or column for `source_code`.
    /// - if there is already an edit with a `span` that overlaps with this one.
    fn queue_edit_at(&mut self, span: Span, operation: Operation) -> &mut Self {
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

        self.edits.insert(start..=end, operation);
        self
    }
    fn assert_column(&self, coord: LineColumn, span: Span) {
        let num_cols = self.source_code.line(coord.line - 1).len_chars();
        assert!(
            num_cols >= coord.column,
            "span exceeds end of line. span is {} but there are {} columns",
            FmtSpan(span),
            num_cols
        );
    }
    /// Apply all the edits, returning the final text.
    pub fn finish(&self) -> String {
        let mut edits = self.edits.iter().collect::<Vec<_>>();
        edits.sort_by_key(|(range, _op)| Reverse(*range.start()));

        let mut source_code = self.source_code.clone();

        for (range, operation) in edits {
            apply(&mut source_code, range, operation)
        }

        source_code.into()
    }
    /// See if this editor has accumulated any edits.
    pub fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }
}

/// An operation on the source text.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
enum Operation {
    Append(String),
    Prepend(String),
    Replace(String),
    Remove,
}

fn apply(rope: &mut Rope, range: &RangeInclusive<usize>, operation: &Operation) {
    match operation {
        Operation::Prepend(it) => rope.insert(*range.start(), it),
        Operation::Append(it) => rope.insert(range.end() + 1, it),
        Operation::Replace(it) => {
            rope.remove(range.clone());
            rope.insert(*range.start(), it)
        }
        Operation::Remove => rope.remove(range.clone()),
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
    use indoc::indoc;
    use pretty_assertions::assert_str_eq;

    #[test]
    fn readme() {
        assert!(
            std::process::Command::new("cargo")
                .args(["rdme", "--check"])
                .output()
                .expect("couldn't run `cargo rdme`")
                .status
                .success(),
            "README.md is out of date - bless the new version by running `cargo rdme`"
        )
    }

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

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Replace(String::from("BAR")));
        assert_str_eq!(synsert.finish(), "const BAR: () = ();");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("TO")));
        assert_str_eq!(synsert.finish(), "const TOFOO: () = ();");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("BAR")));
        assert_str_eq!(synsert.finish(), "const FOOBAR: () = ();");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Remove);
        assert_str_eq!(synsert.finish(), "const : () = ();");
    }

    #[test]
    fn single_line_single_edit_multibyte() {
        let source_code = "const 𓀕: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.expr.span(), Operation::Replace(String::from("𓀠")));
        assert_str_eq!(synsert.finish(), "const 𓀕: () = 𓀠;");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("𓀠")));
        assert_str_eq!(synsert.finish(), "const 𓀠𓀕: () = ();");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("𓀠")));
        assert_str_eq!(synsert.finish(), "const 𓀕𓀠: () = ();");

        let mut synsert = Editor::new(source_code);
        synsert.queue_edit_at(ast.ty.span(), Operation::Remove);
        assert_str_eq!(synsert.finish(), "const 𓀕:  = ();");
    }

    #[test]
    fn single_line_multi_edit() {
        let source_code = "const FOO: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();

        let mut synsert = Editor::new(source_code);
        synsert.replace(ast.expr.span(), "make_bar()");
        synsert.replace(ast.ident.span(), "BAR");
        assert_str_eq!(synsert.finish(), "const BAR: () = make_bar();");

        let mut synsert = Editor::new(source_code);
        synsert.prepend(ast.expr.span(), "make_foo_bar");
        synsert.append(ast.ident.span(), "_BAR");
        assert_str_eq!(synsert.finish(), "const FOO_BAR: () = make_foo_bar();");
    }

    #[test]
    fn multi_line_single_edit() {
        let source_code = indoc! {"
            const FOO: () = {
                do_stuff();
                do_more_stuff();
            };"
        };
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();

        let mut synsert = Editor::new(source_code);
        synsert.replace(ast.expr.span(), "make_foo()");
        assert_str_eq!(synsert.finish(), "const FOO: () = make_foo();");

        let source_code = indoc! {"
            const FOOD: () = ();
            const FOO: () = {
                do_stuff();
                do_more_stuff();
            };
            const FOO_FIGHTERS: () = ();"
        };
        let ast = syn::parse_str::<syn::File>(source_code).unwrap();

        let mut synsert = Editor::new(source_code);
        synsert.replace(ast.items[1].span(), "const BAR: () = ();");
        assert_str_eq!(
            synsert.finish(),
            indoc! {"
                const FOOD: () = ();
                const BAR: () = ();
                const FOO_FIGHTERS: () = ();"
            }
        );
    }
}
