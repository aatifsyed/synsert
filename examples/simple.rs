use clap::Parser;
use console::{style, Style};
use similar::{ChangeTag, TextDiff};
use std::{error::Error, fmt, fs, path::PathBuf};
use syn::{
    parse_quote,
    spanned::Spanned as _,
    visit::{self, Visit},
    Expr, ExprTry, ItemFn, Local, ReturnType, Stmt,
};
use synsert::Synsert;

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
        let mut editor = Editor {
            inner: Synsert::new(&before),
        };
        let Ok(ast) = syn::parse_file(&before) else {
            println!("skipped (failed to parse)");
            continue;
        };
        editor.visit_file(&ast);
        match editor.inner.edits().is_empty() {
            true => println!("no edits."),
            false => {
                let after = editor.inner.apply_all();

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

struct Editor {
    inner: Synsert,
}

impl<'ast> Visit<'ast> for Editor {
    fn visit_item_fn(&mut self, i: &'ast ItemFn) {
        if i.attrs.iter().any(|attr| attr == &parse_quote!(#[test])) {
            if let ReturnType::Type(..) = i.sig.output {
                self.inner.remove(i.sig.output.span());
                for stmt in &i.block.stmts {
                    match stmt {
                        // let foo = bar?;
                        Stmt::Local(Local {
                            init: Some(init), ..
                        }) => {
                            if let Expr::Try(ExprTry { question_token, .. }) = &*init.expr {
                                self.inner.replace(question_token.span(), ".unwrap()")
                            }
                        }
                        // top level expression
                        Stmt::Expr(Expr::Try(ExprTry { question_token, .. }), _) => {
                            self.inner.replace(question_token.span(), ".unwrap()")
                        }
                        _ => {}
                    }
                }
            }
        }
        visit::visit_item_fn(self, i)
    }
}

// https://github.com/mitsuhiko/similar/blob/de455873dab514082bf6e7bb5f0029837fe280d5/examples/terminal-inline.rs
fn print_diff(old: &str, new: &str) {
    let diff = TextDiff::from_lines(old, new);

    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            println!("{:-^1$}", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                print!(
                    "{}{} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        print!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        print!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    println!();
                }
            }
        }
    }

    struct Line(Option<usize>);
    impl fmt::Display for Line {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self.0 {
                None => write!(f, "    "),
                Some(idx) => write!(f, "{:<4}", idx + 1),
            }
        }
    }
}
