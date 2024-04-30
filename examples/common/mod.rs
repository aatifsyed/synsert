use console::{style, Style, Term};
use similar::{ChangeTag, TextDiff};
use std::{error::Error, fmt, fs, path::Path};
use synsert::Editor;

pub fn harness<'path, V>(
    files: impl IntoIterator<Item = &'path Path>,
    mut begin: impl FnMut(Editor) -> V,
    mut end: impl FnMut(V) -> Editor,
) -> Result<(), Box<dyn Error>>
where
    for<'ast> V: syn::visit::Visit<'ast>,
{
    let mut edited_files = 0;
    let mut edited_sites = 0;
    for path in files {
        print!("{}... ", Style::new().apply_to(path.display()).dim());
        let before = fs::read_to_string(path)?;
        let Ok(ast) = syn::parse_file(&before) else {
            println!("skipped (failed to parse file)");
            continue;
        };
        let mut visitor = begin(Editor::new(&before));
        visitor.visit_file(&ast);
        let editor = end(visitor);
        match editor.len() {
            0 => println!("no edits."),
            n => {
                println!("{} edits to apply!", n);

                let after = editor.finish();
                print_diff(&before, &after);

                if dialoguer::Confirm::new()
                    .with_prompt("save the edited file? ")
                    .default(true)
                    .interact()?
                {
                    fs::write(path, after)?;
                    edited_files += 1;
                    edited_sites += n;
                }
            }
        }
    }
    println!("edited {} sites in {} files", edited_sites, edited_files);
    Term::stderr().show_cursor()?; // clean up after rustyline on Ctrl+C
    Ok(())
}

// https://github.com/mitsuhiko/similar/blob/de455873dab514082bf6e7bb5f0029837fe280d5/examples/terminal-inline.rs
pub fn print_diff(old: &str, new: &str) {
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
