//! A primitive for programatically editing files using [`syn`].
//!
//! `syn` is the de-facto standard for parsing Rust in user code.
//! Its syntax tree is easy to use, but it is lossy - if you parse a file,
//! edit it with syn, and unparse it,
//! you'll lose all your comments and spacing (for example).
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
    /// Kept for exposing to users
    source_code_string: String,
    /// Kept for line-column assertions
    source_code_rope: Rope,
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
            source_code_string: String::from(source_code),
            source_code_rope: Rope::from_str(source_code),
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
    /// Get a copy of the _unedited_ text.
    pub fn source(&self) -> &str {
        &self.source_code_string
    }
    /// Get a copy of the _unedited_ text at the given location.
    ///
    /// # Panics
    /// - if `span` is out of bounds.
    pub fn source_at(&self, span: impl Spanned) -> &str {
        let span = span2range(&self.source_code_rope, span.span());
        let start = self.source_code_rope.char_to_byte(*span.start());
        let end = self.source_code_rope.char_to_byte(*span.end());
        &self.source_code_string[start..end]
    }
    /// Apply all the edits, returning the final text.
    pub fn finish(&self) -> String {
        let mut edits = self.edits.iter().collect::<Vec<_>>();
        edits.sort_by_key(|(range, _op)| Reverse(*range.start()));

        let mut source_code = self.source_code_rope.clone();

        for (range, operation) in edits {
            apply(&mut source_code, range, operation)
        }

        source_code.into()
    }
    /// Get the number of accumulated edits.
    pub fn len(&self) -> usize {
        self.edits.len()
    }
    /// See if this editor has accumulated any edits.
    pub fn is_empty(&self) -> bool {
        self.edits.is_empty()
    }
    /// Remove all edits from this editor.
    pub fn clear(&mut self) {
        self.edits.clear()
    }
    /// See if this editor has any edits at the given location.
    ///
    /// # Panics
    /// - if `span` is out of bounds
    pub fn is_empty_at(&self, span: impl Spanned) -> bool {
        let span = span2range(&self.source_code_rope, span.span());
        self.edits.overlaps(&span)
    }

    /// # Panics
    /// - if `span` contains an out-of-bounds line or column for `source_code`.
    /// - if there is already an edit with a `span` that overlaps with this one.
    fn queue_edit_at(&mut self, span: Span, operation: Operation) -> &mut Self {
        // proc-macro2::Span's line's are 1-indexed
        // ropey::Rope's lines are 0-indexed

        let num_lines = self.source_code_rope.len_lines();
        assert!(
            num_lines >= span.end().line,
            "span exceeds end of file. span is {} but there are {} lines",
            FmtSpan(span),
            num_lines
        );

        let range = span2range(&self.source_code_rope, span);

        assert!(
            !self.edits.overlaps(&range),
            "an edit has already been made in the range {}",
            FmtSpan(span)
        );

        if let Some(reported) = span.source_text() {
            let internal = self.source_code_rope.slice(range.clone());
            assert_eq!(
                internal, reported,
                "the span's reported source text does not match our calculated source text"
            )
        }

        self.edits.insert(range, operation);
        self
    }
}

fn span2range(txt: &Rope, span: Span) -> RangeInclusive<usize> {
    let start = span.start();
    assert_column(txt, start, span);
    let start = txt.line_to_char(start.line - 1) + start.column;

    let end = span.end();
    assert_column(txt, end, span);
    let end = txt.line_to_char(end.line - 1) + end.column - 1; // to inclusive

    start..=end
}

fn assert_column(txt: &Rope, coord: LineColumn, span: Span) {
    let num_cols = txt.line(coord.line - 1).len_chars();
    assert!(
        num_cols >= coord.column,
        "span exceeds end of line. span is {} but there are {} columns",
        FmtSpan(span),
        num_cols
    );
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

/// Batteries-included framework for writing structural search and replace programs
#[cfg(feature = "harness")]
pub mod harness {
    use crate::Editor;
    use console::{Style, Term, TermFamily, TermTarget};
    use similar::{ChangeTag, TextDiff};
    use std::{
        fmt, fs,
        io::{self, Write as _},
        path::Path,
        process,
    };
    use syn::visit::Visit;

    // https://github.com/mitsuhiko/similar/blob/de455873dab514082bf6e7bb5f0029837fe280d5/examples/terminal-inline.rs
    pub fn print_diff_on(term: &Term, old: &str, new: &str) -> io::Result<()> {
        let diff = TextDiff::from_lines(old, new);

        for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
            if idx > 0 {
                core::writeln!(&mut &*term, "{:-^1$}", "-", 80)?;
            }
            for op in group {
                for change in diff.iter_inline_changes(op) {
                    let (sign, s) = match change.tag() {
                        ChangeTag::Delete => ("-", Style::new().red()),
                        ChangeTag::Insert => ("+", Style::new().green()),
                        ChangeTag::Equal => (" ", Style::new().dim()),
                    };
                    write!(
                        term,
                        "{}{} |{}",
                        ~ Style::new().dim() => Line(change.old_index()),
                        ~ Style::new().dim() => Line(change.new_index()),
                        ~ Style::new().bold() => sign
                    )?;
                    for (emphasized, value) in change.iter_strings_lossy() {
                        if emphasized {
                            write!(term, "{}", ~ s.clone().underlined().on_black() => value)?;
                        } else {
                            write!(term, "{}", ~ &s => value)?;
                        }
                    }
                    if change.missing_newline() {
                        writeln!(term, "")?;
                    }
                }
            }
        }

        struct Line(Option<usize>);
        impl fmt::Display for Line {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match self.0 {
                    None => f.write_str("    "),
                    Some(idx) => f.write_fmt(format_args!("{:<4}", idx + 1)),
                }
            }
        }

        Ok(())
    }

    pub fn run<P, V>(
        files: impl IntoIterator<Item = P>,
        start: impl FnMut(&Path, Editor) -> Option<V>,
        end: impl FnMut(V) -> Option<Editor>,
    ) -> !
    where
        P: AsRef<Path>,
        V: for<'ast> Visit<'ast>,
    {
        let term = Term::stdout();
        match run_on(files, start, end, &term, true) {
            Ok(report) => report.exit_on(&term),
            Err(e) => {
                let _ = writeln!(&term, "io error: {e}");
                process::exit(1)
            }
        }
    }

    pub fn run_on<P, V>(
        files: impl IntoIterator<Item = P>,
        mut start: impl FnMut(&Path, Editor) -> Option<V>,
        mut end: impl FnMut(V) -> Option<Editor>,
        term: &Term,
        diff_and_confirm: bool,
    ) -> io::Result<Report>
    where
        P: AsRef<Path>,
        V: for<'ast> Visit<'ast>,
    {
        let mut report = Report::default();

        let yellow = Style::new().yellow();
        let red = Style::new().red();
        let green = Style::new().green();

        for file in files {
            let file = file.as_ref();
            write!(term, "file {}...", file.display())?;
            match fs::read_to_string(file) {
                Ok(orig) => {
                    match Editor::new_with_ast(&orig) {
                        Ok((editor, ast)) => match start(file, editor) {
                            Some(mut visitor) => {
                                // flush in case the user wants to do IO
                                term.flush()?;
                                visitor.visit_file(&ast);
                                match end(visitor) {
                                    Some(editor) => match editor.len() {
                                        0 => {
                                            writeln!(term, "{} (no edits)", ~ &yellow => "skipped")?
                                        }
                                        n => match diff_and_confirm {
                                            true => {
                                                writeln!(term, "{n} edits!")?;
                                                let new = editor.finish();
                                                print_diff_on(term, &orig, &new)?;
                                                match dialoguer::Confirm::new()
                                                    .with_prompt("save the edited file? ")
                                                    .default(true)
                                                    .interact_on(term)
                                                    .map_err(|dialoguer::Error::IO(it)| it)?
                                                {
                                                    true => match fs::write(file, new) {
                                                        Ok(()) => {
                                                            report.files += 1;
                                                            report.edits += n
                                                        }
                                                        Err(e) => {
                                                            report.failed += 1;
                                                            writeln!(term, "{}: failed to write: {e}", ~ &red => "error")?;
                                                        }
                                                    },
                                                    false => {}
                                                }
                                            }
                                            false => match fs::write(file, editor.finish()) {
                                                Ok(()) => {
                                                    report.files += 1;
                                                    report.edits += n;
                                                    writeln!(term, "{} ({n} edits)", ~ &green => "ok")?
                                                }
                                                Err(e) => {
                                                    report.failed += 1;
                                                    writeln!(term, "{} (failed to write: {e})", ~ &red => "error")?;
                                                }
                                            },
                                        },
                                    },
                                    None => writeln!(term, "{}", ~ &yellow => "skipped" )?,
                                }
                            }
                            None => writeln!(term, "{}", ~ &yellow => "skipped" )?,
                        },
                        Err(e) => {
                            report.failed += 1;
                            writeln!(term, "{} (failed to parse: {e})", ~ &red => "error")?;
                        }
                    };
                }
                Err(e) => {
                    report.failed += 1;
                    writeln!(&term, "{} (failed to read: {e})", ~ &red => "error" )?;
                }
            }
        }

        Ok(report)
    }

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct Report {
        pub failed: usize,
        pub files: usize,
        pub edits: usize,
    }

    impl Report {
        pub fn exit_on(&self, term: &Term) -> ! {
            let Self {
                failed,
                files,
                edits,
            } = self;
            let _ = writeln!(term, "edited {} sites across {} files", edits, files);
            match failed {
                0 => process::exit(0),
                n => {
                    let _ = writeln!(term, "failed to edit {n} files");
                    process::exit(1)
                }
            }
        }
    }

    fn should_color(term: &Term) -> bool {
        let env = match term.target() {
            TermTarget::Stdout => console::colors_enabled(),
            TermTarget::Stderr => console::colors_enabled_stderr(),
            TermTarget::ReadWritePair(_) => false,
        };
        term.is_term()
            && term.features().colors_supported()
            && term.features().is_attended()
            && env
            && matches!(
                term.features().family(),
                TermFamily::UnixTerm | TermFamily::WindowsConsole
            )
    }

    macro_rules! write {
        ($target:expr, $fmt:literal $(, $(~ $style:expr => )? $arg:expr)* $(,)?) => {{
            let mut _target: &console::Term = $target;
            match $crate::harness::should_color(&_target) {
                true => _target.write_fmt(format_args!($fmt, $({
                    let _style = $crate::harness::Style::new();
                    $(
                        let _style = $style;
                    )?
                    _style.apply_to($arg)
                }),*)),
                false => _target.write_fmt(format_args!($fmt, $($arg),*)),
            }
        }};
    }
    pub(crate) use write;
    macro_rules! writeln {
        ($target:expr, $($tt:tt)*) => {{
            let mut _target: &console::Term = $target;
            match $crate::harness::write!(&mut _target, $($tt)*) {
                Ok(()) => _target.write_fmt(format_args!("\n")),
                Err(e) => Err(e)
            }
        }};
    }
    pub(crate) use writeln;
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
