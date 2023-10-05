use std::{cmp::Reverse, ops::RangeInclusive};

use miette::{SourceCode, SourceOffset, SourceSpan};
use proc_macro2::{LineColumn, Span};
use rangemap::RangeInclusiveMap;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Operation {
    Append(String),
    Prepend(String),
    Replace(String),
    Remove,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Synsert<'a> {
    source_code: &'a str,
    edits: RangeInclusiveMap<usize, Operation>,
}

impl<'a> Synsert<'a> {
    pub fn new(source_code: &'a str) -> Self {
        Self {
            source_code,
            edits: RangeInclusiveMap::new(),
        }
    }
    /// # Panics
    /// - if `span` doesn't correspond to the `source_code` that this was created with.
    /// - if there is already an edit that overlaps with this `span`
    pub fn queue_edit_at(&mut self, span: Span, operation: Operation) {
        // proc-macro2 columns: 0-indexed, UTF-8-chars
        //      miette columns: 1-indexed, byte offset
        //   proc-macro2 lines: 1-indexed
        //        miette lines: 1-indexed
        let start = span.start();
        let start = SourceOffset::from_location(
            self.source_code,
            start.line,
            self.column_byte_offset(start),
        );
        let end = span.end();
        let end =
            SourceOffset::from_location(self.source_code, end.line, self.column_byte_offset(end));
        let len = SourceOffset::from(end.offset() - start.offset());
        let miette_span = SourceSpan::new(start, len);
        let actual = std::str::from_utf8(
            self.source_code
                .read_span(&miette_span, 0, 0)
                .unwrap()
                .data(),
        )
        .unwrap();
        let expected = span.source_text().unwrap();
        assert_eq!(expected, actual);
        let start = start.offset();
        let end = end.offset();
        if self.edits.overlaps(&(start..=end)) {
            panic!("duplicate edit for range")
        }
        self.edits.insert(start..=end, operation)
    }

    fn column_byte_offset(&self, position: LineColumn) -> usize {
        let (column, _char) = self
            .source_code
            .lines()
            .nth(position.line - 1)
            .unwrap()
            .char_indices()
            .nth(position.column + 1)
            .unwrap();
        column
    }

    /// Sorted, later spans first, non-overlapping.
    /// Spans are in UTF-8 chars.
    pub fn edits(self) -> Vec<(RangeInclusive<usize>, Operation)> {
        let mut v = self.edits.into_iter().collect::<Vec<_>>();
        v.sort_by_key(|(range, _op)| Reverse(*range.start()));
        v
    }

    pub fn applied(self) -> String {
        let mut edited = String::from(self.source_code);
        for (span, operation) in self.edits() {
            edited = do_edit(edited, span, operation)
        }
        edited
    }
}

pub fn do_edit(
    mut source_code: String,
    span: RangeInclusive<usize>,
    operation: Operation,
) -> String {
    let (beginning_and_middle, end) = source_code.split_at(*span.end());
    let (beginning, middle) = beginning_and_middle.split_at(*span.start());

    let end = end.to_owned();
    let middle = middle.to_owned();
    source_code.truncate(beginning.len());
    let mut beginning = source_code;

    match operation {
        Operation::Append(it) => {
            beginning.push_str(&middle);
            beginning.push_str(&it);
        }
        Operation::Prepend(it) => {
            beginning.push_str(&it);
            beginning.push_str(&middle);
        }
        Operation::Replace(it) => beginning.push_str(&it),
        Operation::Remove => {}
    }
    beginning.push_str(&end);
    beginning
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;
    use pretty_assertions::assert_str_eq;
    use syn::spanned::Spanned as _;

    #[test]
    fn proc_macro2_spans_are_correct() {
        let source_code = "const FOO: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        assert_eq!(
            "FOO",
            ast.ident.span().source_text().unwrap(),
            "ident from span doesn't match ident from source"
        )
    }

    #[test]
    fn single_line_single_edit() {
        let source_code = "const FOO: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Replace(String::from("BAR")));
        assert_str_eq!(synsert.applied(), "const BAR: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("TO")));
        assert_str_eq!(synsert.applied(), "const TOFOO: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("BAR")));
        assert_str_eq!(synsert.applied(), "const FOOBAR: () = ();");

        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.ident.span(), Operation::Remove);
        assert_str_eq!(synsert.applied(), "const : () = ();");
    }

    #[test]
    fn debug_str() {
        let source_code = "const 𓀕: () = ();";
        let ix2char = source_code.char_indices().collect::<HashMap<_, _>>();
        let mut ix_char = 0;
        for (ix, byte) in source_code.bytes().enumerate() {
            match ix2char.get(&ix) {
                Some(char) => {
                    println!("{ix:>2} │ {byte:<3} │ {char} │ {ix_char:<2}");
                    ix_char += 1
                }
                None => println!("{ix:>2} │ {byte:<3} ┊   ┊"),
            }
        }

        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        let syn::ItemConst {
            attrs: _,
            vis,
            const_token,
            ident,
            generics,
            colon_token,
            ty,
            eq_token,
            expr,
            semi_token,
        } = &ast;

        macro_rules! print_spanned {
            ($($ident:ident)*) => {
                $(
                    let span = $ident.span();
                    let start = span.start();
                    let end = span.end();
                    println!(
                        "{}..{} {}",
                        start.column, end.column, stringify!($ident)
                    );
                )*
            };
        }

        println!();
        print_spanned!(
            vis const_token ident generics colon_token ty eq_token expr semi_token
        );
    }

    #[test]
    fn single_line_single_edit_multibyte() {
        let source_code = "const 𓀕: () = ();";
        let ast = syn::parse_str::<syn::ItemConst>(source_code).unwrap();
        let mut synsert = Synsert::new(source_code);
        synsert.queue_edit_at(ast.expr.span(), Operation::Replace(String::from("𓀠")));
        assert_str_eq!(synsert.applied(), "const 𓀕: () = 𓀠;");

        // let mut synsert = Synsert::new(source_code);
        // synsert.queue_edit_at(ast.ident.span(), Operation::Prepend(String::from("𓀠")));
        // assert_str_eq!(synsert.applied(), "const 𓀠𓀕: () = ();");

        // let mut synsert = Synsert::new(source_code);
        // synsert.queue_edit_at(ast.ident.span(), Operation::Append(String::from("𓀠")));
        // assert_str_eq!(synsert.applied(), "const 𓀕𓀠: () = ();");

        // let mut synsert = Synsert::new(source_code);
        // synsert.queue_edit_at(ast.ty.span(), Operation::Remove);
        // assert_str_eq!(synsert.applied(), "const 𓀕:  = ();");
    }
}
