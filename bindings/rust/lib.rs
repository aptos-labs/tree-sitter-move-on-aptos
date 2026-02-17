//! This crate provides MoveOnAptos language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [language][language func] function to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse some code:
//!
//! ```
//! let code = r#"
//! module 0x1::example {
//!     public fun hello(): u64 { 42 }
//! }
//! "#;
//! let mut parser = tree_sitter::Parser::new();
//! parser.set_language(&tree_sitter_move_on_aptos::language()).expect("Error loading MoveOnAptos grammar");
//! let tree = parser.parse(code, None).unwrap();
//! assert!(!tree.root_node().has_error());
//! ```
//!
//! [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
//! [language func]: fn.language.html
//! [Parser]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

use tree_sitter::Language;

extern "C" {
    fn tree_sitter_move_on_aptos() -> Language;
}

/// Get the tree-sitter [Language][] for this grammar.
///
/// [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
pub fn language() -> Language {
    unsafe { tree_sitter_move_on_aptos() }
}

/// The content of the [`node-types.json`][] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

// Uncomment these to include any queries that this grammar contains

// pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");
// pub const INJECTIONS_QUERY: &str = include_str!("../../queries/injections.scm");
// pub const LOCALS_QUERY: &str = include_str!("../../queries/locals.scm");
// pub const TAGS_QUERY: &str = include_str!("../../queries/tags.scm");

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(source: &str) -> tree_sitter::Tree {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("Error loading MoveOnAptos grammar");
        parser.parse(source, None).unwrap()
    }

    #[test]
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&language())
            .expect("Error loading MoveOnAptos grammar");
    }

    #[test]
    fn test_parse_module_declaration() {
        let code = r#"
module 0x1::coin {
    struct Coin<phantom CoinType> has key, store {
        value: u64,
    }
}
"#;
        let tree = parse(code);
        let root = tree.root_node();

        assert!(!root.has_error());
        assert_eq!(root.kind(), "source_file");

        let module = root.named_child(0).unwrap();
        assert_eq!(module.kind(), "module_declaration");

        let body = module.child_by_field_name("body").unwrap();
        let struct_decl = body
            .named_children(&mut body.walk())
            .find(|c| c.kind() == "struct_declaration")
            .unwrap();

        let name = struct_decl.child_by_field_name("name").unwrap();
        assert_eq!(name.utf8_text(code.as_bytes()).unwrap(), "Coin");
    }

    #[test]
    fn test_parse_function_with_control_flow() {
        let code = r#"
module 0x1::example {
    public fun transfer(from: &signer, to: address, amount: u64): bool {
        if (amount > 0) {
            true
        } else {
            false
        }
    }
}
"#;
        let tree = parse(code);
        let root = tree.root_node();
        assert!(!root.has_error());

        let module = root.named_child(0).unwrap();
        let body = module.child_by_field_name("body").unwrap();
        let func = body
            .named_children(&mut body.walk())
            .find(|c| c.kind() == "function_declaration")
            .unwrap();

        let name = func.child_by_field_name("name").unwrap();
        assert_eq!(name.utf8_text(code.as_bytes()).unwrap(), "transfer");
    }

    #[test]
    fn test_parse_enum_and_match() {
        let code = r#"
module 0x1::color {
    enum Color {
        Red,
        Green,
        Blue,
    }

    public fun is_primary(c: &Color): bool {
        match (c) {
            Color::Red | Color::Blue => true,
            _ => false,
        }
    }
}
"#;
        let tree = parse(code);
        let root = tree.root_node();
        assert!(!root.has_error());

        let module = root.named_child(0).unwrap();
        let body = module.child_by_field_name("body").unwrap();
        let children: Vec<_> = body.named_children(&mut body.walk()).collect();

        let enum_decl = children.iter().find(|c| c.kind() == "enum_declaration");
        assert!(enum_decl.is_some(), "should contain an enum_declaration");

        let func = children
            .iter()
            .find(|c| c.kind() == "function_declaration");
        assert!(func.is_some(), "should contain a function_declaration");
    }

    #[test]
    fn test_parse_use_declarations() {
        let code = r#"
module 0x1::my_module {
    use std::vector;
    use 0x1::coin::{Self, Coin};
    use 0x1::account::Account as MyAccount;
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());

        let body = tree
            .root_node()
            .named_child(0)
            .unwrap()
            .child_by_field_name("body")
            .unwrap();
        let uses: Vec<_> = body
            .named_children(&mut body.walk())
            .filter(|c| c.kind() == "use_declaration")
            .collect();
        assert_eq!(uses.len(), 3);
    }

    #[test]
    fn test_parse_spec_block() {
        let code = r#"
module 0x1::verified {
    fun add(a: u64, b: u64): u64 {
        a + b
    }

    spec add {
        ensures result == a + b;
    }

    spec module {
        pragma verify = true;
    }
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());

        let body = tree
            .root_node()
            .named_child(0)
            .unwrap()
            .child_by_field_name("body")
            .unwrap();
        let specs: Vec<_> = body
            .named_children(&mut body.walk())
            .filter(|c| c.kind() == "spec_block")
            .collect();
        assert_eq!(specs.len(), 2);
    }

    #[test]
    fn test_parse_attributes() {
        let code = r#"
module 0x1::tests {
    #[test]
    fun test_basic() {
        assert!(1 + 1 == 2, 0);
    }

    #[test, expected_failure(abort_code = 1)]
    fun test_failure() {
        abort 1
    }
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());
    }

    #[test]
    fn test_parse_generics_and_abilities() {
        let code = r#"
module 0x1::container {
    struct Box<T: store + drop> has key {
        value: T,
    }

    public fun unbox<T: store + drop>(box: Box<T>): T {
        let Box { value } = box;
        value
    }
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());
    }

    #[test]
    fn test_parse_lambda_expression() {
        let code = r#"
module 0x1::functional {
    use std::vector;

    public fun sum(v: &vector<u64>): u64 {
        let result = 0u64;
        vector::for_each_ref(v, |e| {
            result = result + *e;
        });
        result
    }
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());
    }

    #[test]
    fn test_parse_address_block() {
        let code = r#"
address 0x42 {
    module first {
        public fun one(): u64 { 1 }
    }
    module second {
        public fun two(): u64 { 2 }
    }
}
"#;
        let tree = parse(code);
        assert!(!tree.root_node().has_error());

        let root = tree.root_node();
        let addr_block = root.named_child(0).unwrap();
        assert_eq!(addr_block.kind(), "address_block");
    }

    #[test]
    fn test_node_types_json_is_valid() {
        assert!(!NODE_TYPES.is_empty());
        assert!(NODE_TYPES.starts_with('['));
        assert!(NODE_TYPES.contains("\"type\""));
        assert!(NODE_TYPES.contains("module_declaration"));
        assert!(NODE_TYPES.contains("function_declaration"));
        assert!(NODE_TYPES.contains("struct_declaration"));
    }
}
