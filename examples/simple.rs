use clap::Parser;
use std::{error::Error, path::PathBuf};
use syn::{
    parse_quote,
    spanned::Spanned as _,
    visit::{self, Visit},
    Expr, ExprTry, ItemFn, Local, ReturnType, Stmt,
};
use synsert::Editor;

/// A simple program that converts `#[test]` functions that return `Result`s to not return anything.
#[derive(Parser)]
struct Args {
    #[arg(num_args(1..), required = true)]
    file: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let Args { file } = Args::parse();

    synsert::harness::run(
        file,
        |_, editor| Some(Visitor { editor }),
        |Visitor { editor }| Some(editor),
    )
}

struct Visitor {
    editor: Editor,
}

impl<'ast> Visit<'ast> for Visitor {
    fn visit_item_fn(&mut self, i: &'ast ItemFn) {
        if i.attrs.iter().any(|attr| attr == &parse_quote!(#[test])) {
            if let ReturnType::Type(..) = i.sig.output {
                self.editor.remove(i.sig.output.span());
                for stmt in &i.block.stmts {
                    match stmt {
                        // let foo = bar?;
                        Stmt::Local(Local {
                            init: Some(init), ..
                        }) => {
                            if let Expr::Try(ExprTry { question_token, .. }) = &*init.expr {
                                self.editor.replace(question_token.span(), ".unwrap()");
                            }
                        }
                        // top level expression
                        Stmt::Expr(Expr::Try(ExprTry { question_token, .. }), _) => {
                            self.editor.replace(question_token.span(), ".unwrap()");
                        }
                        _ => {}
                    }
                }
            }
        }
        visit::visit_item_fn(self, i)
    }
}
