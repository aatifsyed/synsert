mod common;

use clap::Parser;
use common::print_diff;
use console::Style;
use std::{error::Error, fs, path::PathBuf};
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
