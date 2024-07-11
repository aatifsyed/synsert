mod common;

use clap::Parser;
use std::{collections::HashSet, error::Error, fs, path::PathBuf};
use syn::visit::Visit;
use synsert::Editor;

#[derive(Parser)]
struct Args {
    v0: PathBuf,
    v1: PathBuf,
    #[arg(num_args(1..), required = true)]
    file: Vec<PathBuf>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let Args { file, v0, v1 } = Args::parse();
    let v0 = fs::read_to_string(v0)?;
    let v0 = &v0.lines().collect();
    let v1 = fs::read_to_string(v1)?;
    let v1 = &v1.lines().collect();
    common::harness(
        file.iter().map(PathBuf::as_path),
        |editor| Visitor { editor, v0, v1 },
        |Visitor { editor, .. }| editor,
    )
}

struct Visitor<'a> {
    editor: Editor,
    v0: &'a HashSet<&'a str>,
    v1: &'a HashSet<&'a str>,
}

impl Visit<'_> for Visitor<'_> {
    fn visit_item_impl(&mut self, i: &'_ syn::ItemImpl) {
        if let Some((_, it, _)) = i.trait_.as_ref() {
            if it.segments.last().is_some_and(|it| it.ident == "RpcMethod") {
                let Some(full_name) = i.items.iter().find_map(|it| match it {
                    syn::ImplItem::Const(it) if it.ident == "NAME" => match &it.expr {
                        syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(it),
                            ..
                        }) => Some(it.value()),
                        _ => None,
                    },
                    _ => None,
                }) else {
                    return;
                };
                let Some(replaceme) = i.items.iter().find_map(|it| match it {
                    syn::ImplItem::Const(it) if it.ident == "API_PATHS" => Some(&it.expr),
                    _ => None,
                }) else {
                    return;
                };

                let Some(name) = full_name.strip_prefix("Filecoin.") else {
                    return;
                };

                let new = match (self.v0.contains(name), self.v1.contains(name)) {
                    (true, true) => "ApiPaths::Both",
                    (true, false) => "ApiPaths::V0",
                    (false, true) => "ApiPaths::V1",
                    (false, false) => return,
                };

                self.editor.replace(replaceme, new);
            }
        }
    }
}
