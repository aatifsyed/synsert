mod common;

use clap::Parser;
use common::print_diff;
use console::Style;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use std::{error::Error, fs, path::PathBuf};
use syn::{punctuated::Punctuated, visit::Visit, Expr, Ident, LitStr, Token};
use synsert::Editor;

use crate::{
    format_args::{FormatArgs, OwnedPiece, OwnedPosition},
    tracing_fields::{Field, FieldKey, Path, Sigil},
};

#[derive(clap::Parser)]
struct Args {
    #[arg(num_args(1..), required = true)]
    file: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let Args { file } = Args::parse();

    let mut count = 0;
    for path in file.iter() {
        print!("{}...", Style::new().apply_to(path.display()).dim());
        let before = fs::read_to_string(path)?;
        let mut visitor = Visitor {
            editor: Editor::new(&before),
        };
        let Ok(ast) = syn::parse_file(&before) else {
            println!("skipped (failed to parse)");
            continue;
        };
        visitor.visit_file(&ast);
        match visitor.editor.is_empty() {
            true => println!("no edits."),
            false => {
                let after = visitor.editor.finish();

                println!("edits to apply!");
                print_diff(&before, &after);

                if dialoguer::Confirm::new()
                    .with_prompt("save the edited file? ")
                    .interact()?
                {
                    fs::write(path, after)?;
                    count += 1
                }
            }
        }
    }
    println!("edited {} files", count);
    Ok(())
}

struct Visitor {
    editor: Editor,
}

impl<'ast> Visit<'ast> for Visitor {
    fn visit_macro(&mut self, node: &'ast syn::Macro) {
        if !node.path.segments.last().is_some_and(|it| {
            matches!(
                it.ident.to_string().as_str(),
                "error" | "warn" | "info" | "debug" | "trace"
            )
        }) {
            return; // not the right macro
        }

        let Ok(body) = node.parse_body::<FormatArgs>() else {
            return; // don't understand the args
        };

        let slug = body
            .pieces
            .iter()
            .filter_map(|it| match it {
                OwnedPiece::String(s) => Some(s),
                OwnedPiece::NextArgument(_) => None,
            })
            .fold(String::new(), |mut acc, el| {
                acc += el;
                acc
            });

        let mut fields = Punctuated::<_, Token![,]>::new();

        for arg in body.pieces.iter().filter_map(|it| match it {
            OwnedPiece::String(_) => None,
            OwnedPiece::NextArgument(it) => Some(&**it),
        }) {
            let sigil = match arg.format.ty.as_str() {
                "" => Some(Sigil::Percent),
                "?" => Some(Sigil::QuestionMark),
                _ => None,
            };
            match &arg.position {
                OwnedPosition::ArgumentImplicitlyIs(ix) | OwnedPosition::ArgumentIs(ix) => {
                    match body.positional_args.get(*ix) {
                        Some(Expr::Path(it)) if it.path.segments.len() == 1 => fields.push(
                            Field::Shorthand(sigil, syn::parse2(it.to_token_stream()).unwrap()),
                        ),
                        Some(Expr::Field(it)) => fields.push(Field::Shorthand(
                            sigil,
                            syn::parse2(it.to_token_stream()).unwrap(),
                        )),
                        Some(it) => fields.push(Field::KV(
                            FieldKey::Quoted(LitStr::new(
                                it.to_token_stream().to_string().as_str(),
                                Span::call_site(),
                            )),
                            Token![=](Span::call_site()),
                            sigil,
                            it.clone(),
                        )),
                        None => return, // no such arg
                    }
                }
                OwnedPosition::ArgumentNamed(it) => {
                    let ident = Ident::new(it, Span::call_site());
                    match body.named_args.get(&ident) {
                        Some(e) => fields.push(Field::KV(
                            FieldKey::Path(Path::from(ident)),
                            Token![=](Span::call_site()),
                            sigil,
                            e.clone(),
                        )),
                        None => fields.push(Field::Shorthand(sigil, Path::from(ident))),
                    }
                }
            }
        }

        if fields.is_empty() {
            return; // do nothing
        }

        self.editor.replace(
            &node.tokens,
            quote!(#fields, #slug).into_token_stream().to_string(),
        );
    }
}

mod tracing_fields {
    use derive_quote_to_tokens::ToTokens;
    use quote::{quote, ToTokens};
    use syn::{
        ext::IdentExt as _,
        parse::{Parse, ParseStream},
        punctuated::Punctuated,
        Expr, Ident, LitStr, Token,
    };

    #[derive(Debug, Clone)]
    pub enum Sigil {
        Percent,
        QuestionMark,
    }
    impl ToTokens for Sigil {
        fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
            tokens.extend(match self {
                Sigil::Percent => quote!(%),
                Sigil::QuestionMark => quote!(?),
            })
        }
    }
    #[derive(ToTokens, Debug, Clone)]
    pub struct Path {
        /// Non-empty, no trailing dots
        inner: Punctuated<Ident, Token![.]>,
    }
    impl Parse for Path {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(Self {
                inner: Punctuated::parse_separated_nonempty_with(input, Ident::parse_any)?,
            })
        }
    }
    impl From<Ident> for Path {
        fn from(value: Ident) -> Self {
            Self {
                inner: Punctuated::from_iter([value]),
            }
        }
    }
    #[derive(ToTokens, Debug, Clone)]
    pub enum FieldKey {
        Path(Path),
        Quoted(LitStr),
    }

    #[derive(ToTokens, Debug, Clone)]
    pub enum Field {
        Shorthand(Option<Sigil>, Path),
        KV(FieldKey, Token![=], Option<Sigil>, Expr),
    }
}

mod format_args {
    use rustc_parse_format::{
        Alignment, Argument, Count, DebugHex, FormatSpec, InnerSpan, ParseMode, Parser, Piece,
        Position, Sign,
    };
    use std::collections::HashMap;
    use syn::{
        parse::{Parse, ParseStream},
        Expr, Ident, LitStr, Token,
    };

    #[derive(PartialEq, Debug)]
    pub struct FormatArgs {
        pub pieces: Vec<OwnedPiece>,
        pub positional_args: Vec<Expr>,
        pub named_args: HashMap<Ident, Expr>,
    }

    #[test]
    fn test2() {
        let _: FormatArgs = dbg!(syn::parse_quote!("hello {:}"));
    }

    #[test]
    fn test() {
        use syn::parse_quote;
        assert_eq!(
            FormatArgs {
                pieces: vec![
                    OwnedPiece::String(String::from("hello ")),
                    OwnedPiece::NextArgument(Box::new(OwnedArgument {
                        position: OwnedPosition::ArgumentImplicitlyIs(0),
                        position_span: InnerSpan { start: 8, end: 8 },
                        format: OwnedFormatSpec::default(),
                    })),
                    OwnedPiece::String(String::from("!")),
                ],
                positional_args: vec![parse_quote!(42)],
                named_args: HashMap::new()
            },
            parse_quote!("hello {}!", 42)
        );
    }

    impl Parse for FormatArgs {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            let fmt = input.parse::<LitStr>()?.value();
            let pieces = Parser::new(&fmt, None, None, false, ParseMode::Format)
                .map(Into::into)
                .collect();
            let mut positional_args = Vec::new();
            let mut named_args = HashMap::new();
            if input.is_empty() {
                return Ok(Self {
                    pieces,
                    positional_args,
                    named_args,
                });
            }
            input.parse::<Token![,]>()?;
            for arg in input.parse_terminated(Arg::parse, Token![,])? {
                match arg {
                    Arg::Positional(it) => match named_args.is_empty() {
                        true => positional_args.push(it),
                        false => {
                            return Err(
                                input.error("positional arguments may not follow named arguments")
                            )
                        }
                    },
                    Arg::Named(name, _, val) => match named_args.get_key_value(&name) {
                        None => {
                            named_args.insert(name, val);
                        }
                        Some((already, _)) => {
                            let mut error = input.error("duplicate named argument");
                            error.combine(syn::Error::new(
                                already.span(),
                                "previous definition here",
                            ));
                            return Err(error);
                        }
                    },
                }
            }

            Ok(Self {
                pieces,
                positional_args,
                named_args,
            })
        }
    }

    enum Arg {
        Positional(Expr),
        Named(Ident, Token![=], Expr),
    }
    impl Parse for Arg {
        fn parse(input: ParseStream) -> syn::Result<Self> {
            Ok(match input.peek2(Token![=]) {
                true => Self::Named(input.parse()?, input.parse()?, input.parse()?),
                false => Self::Positional(input.parse()?),
            })
        }
    }

    /// A piece is a portion of the format string which represents the next part
    /// to emit. These are converted from [`Piece`]s emitted as a stream by the [`Parser`] class.
    #[derive(Clone, PartialEq, Debug)]
    pub enum OwnedPiece {
        /// A literal string which should directly be emitted
        String(String),
        /// This describes that formatting should process the next argument (as
        /// specified inside) for emission.
        NextArgument(Box<OwnedArgument>),
    }

    impl From<Piece<'_>> for OwnedPiece {
        fn from(value: Piece<'_>) -> Self {
            match value {
                Piece::String(it) => Self::String(it.into()),
                Piece::NextArgument(it) => Self::NextArgument(Box::new((*it).into())),
            }
        }
    }

    /// Representation of an argument specification.
    #[derive(Clone, PartialEq, Debug)]
    pub struct OwnedArgument {
        /// Where to find this argument
        pub position: OwnedPosition,
        /// The span of the position indicator. Includes any whitespace in implicit
        /// positions (`{  }`).
        pub position_span: InnerSpan,
        /// How to format the argument
        pub format: OwnedFormatSpec,
    }

    impl From<Argument<'_>> for OwnedArgument {
        fn from(value: Argument<'_>) -> Self {
            let Argument {
                position,
                position_span,
                format,
            } = value;
            Self {
                position: position.into(),
                position_span,
                format: format.into(),
            }
        }
    }

    /// Enum describing where an argument for a format can be located.
    #[derive(Clone, PartialEq, Debug)]
    #[allow(clippy::enum_variant_names)]
    pub enum OwnedPosition {
        /// The argument is implied to be located at an index
        ArgumentImplicitlyIs(usize),
        /// The argument is located at a specific index given in the format,
        ArgumentIs(usize),
        /// The argument has a name.
        ArgumentNamed(String),
    }
    impl From<Position<'_>> for OwnedPosition {
        fn from(value: Position<'_>) -> Self {
            match value {
                Position::ArgumentImplicitlyIs(it) => Self::ArgumentImplicitlyIs(it),
                Position::ArgumentIs(it) => Self::ArgumentIs(it),
                Position::ArgumentNamed(it) => Self::ArgumentNamed(it.into()),
            }
        }
    }

    /// Specification for the formatting of an argument in the format string.
    #[derive(Clone, PartialEq, Debug)]
    pub struct OwnedFormatSpec {
        /// Optionally specified character to fill alignment with.
        pub fill: Option<char>,
        /// Span of the optionally specified fill character.
        pub fill_span: Option<InnerSpan>,
        /// Optionally specified alignment.
        pub align: Alignment,
        /// The `+` or `-` flag.
        pub sign: Option<Sign>,
        /// The `#` flag.
        pub alternate: bool,
        /// The `0` flag.
        pub zero_pad: bool,
        /// The `x` or `X` flag. (Only for `Debug`.)
        pub debug_hex: Option<DebugHex>,
        /// The integer precision to use.
        pub precision: OwnedCount,
        /// The span of the precision formatting flag (for diagnostics).
        pub precision_span: Option<InnerSpan>,
        /// The string width requested for the resulting format.
        pub width: OwnedCount,
        /// The span of the width formatting flag (for diagnostics).
        pub width_span: Option<InnerSpan>,
        /// The descriptor string representing the name of the format desired for
        /// this argument, this can be empty or any number of characters, although
        /// it is required to be one word.
        pub ty: String,
        /// The span of the descriptor string (for diagnostics).
        pub ty_span: Option<InnerSpan>,
    }
    impl From<FormatSpec<'_>> for OwnedFormatSpec {
        fn from(value: FormatSpec<'_>) -> Self {
            let FormatSpec {
                fill,
                fill_span,
                align,
                sign,
                alternate,
                zero_pad,
                debug_hex,
                precision,
                precision_span,
                width,
                width_span,
                ty,
                ty_span,
            } = value;
            Self {
                fill,
                fill_span,
                align,
                sign,
                alternate,
                zero_pad,
                debug_hex,
                precision: precision.into(),
                precision_span,
                width: width.into(),
                width_span,
                ty: ty.into(),
                ty_span,
            }
        }
    }

    impl Default for OwnedFormatSpec {
        fn default() -> Self {
            Self {
                fill: Default::default(),
                fill_span: Default::default(),
                align: Alignment::AlignUnknown,
                sign: Default::default(),
                alternate: Default::default(),
                zero_pad: Default::default(),
                debug_hex: Default::default(),
                precision: Default::default(),
                precision_span: Default::default(),
                width: Default::default(),
                width_span: Default::default(),
                ty: Default::default(),
                ty_span: Default::default(),
            }
        }
    }

    /// A count is used for the precision and width parameters of an integer, and
    /// can reference either an argument or a literal integer.
    #[derive(Clone, PartialEq, Debug, Default)]
    #[allow(clippy::enum_variant_names)]
    pub enum OwnedCount {
        /// The count is specified explicitly.
        CountIs(usize),
        /// The count is specified by the argument with the given name.
        CountIsName(String, InnerSpan),
        /// The count is specified by the argument at the given index.
        CountIsParam(usize),
        /// The count is specified by a star (like in `{:.*}`) that refers to the argument at the given index.
        CountIsStar(usize),
        /// The count is implied and cannot be explicitly specified.
        #[default]
        CountImplied,
    }
    impl From<Count<'_>> for OwnedCount {
        fn from(value: Count<'_>) -> Self {
            match value {
                Count::CountIs(it) => Self::CountIs(it),
                Count::CountIsName(l, r) => Self::CountIsName(l.into(), r),
                Count::CountIsParam(it) => Self::CountIsParam(it),
                Count::CountIsStar(it) => Self::CountIsStar(it),
                Count::CountImplied => Self::CountImplied,
            }
        }
    }
}
