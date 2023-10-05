<!-- cargo-rdme start -->

A primitive for programatically editing files using [`syn`].

`syn` is the de-facto standard for parsing Rust. Its syntax tree is easy to
use, but it is lossy - if you parse a file, edit it with syn, and unparse
it, you'll lose all your comments and spacing (for example).

[Rust Analyzer's syntax crate](https://docs.rs/ra_ap_syntax) has a lossless
syntax tree, which powers IDE assists, but it's far more difficult to use.

[`Editor`] allows you to use `syn`'s syntax tree to write your Structured
Search and Replace tools, or IDE assists.

```rust
let source_code = "const NUM: usize = 1;"; // get the source text

// create an AST and a helper struct from the same source code
let (mut editor, ast) = synsert::Editor::new_with_ast::<syn::ItemConst>(source_code).unwrap();

let edited = editor
    .append(ast.ident, "_YAKS")
    .replace(ast.expr, "9001")
    .finish();

assert_eq!(edited, "const NUM_YAKS: usize = 9001;");
```

See the examples for a more in-depth case using a [`syn::visit::Visit`](https://docs.rs/syn/latest/syn/visit/index.html)or

<!-- cargo-rdme end -->
