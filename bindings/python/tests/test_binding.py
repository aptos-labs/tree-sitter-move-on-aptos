from unittest import TestCase

import tree_sitter
import tree_sitter_move_on_aptos


class TestLanguage(TestCase):
    def setUp(self):
        self.language = tree_sitter.Language(tree_sitter_move_on_aptos.language())
        self.parser = tree_sitter.Parser(self.language)

    def test_can_load_grammar(self):
        """Test that the grammar can be loaded without errors."""
        try:
            tree_sitter.Language(tree_sitter_move_on_aptos.language())
        except Exception:
            self.fail("Error loading Move on Aptos grammar")

    def test_parse_module_declaration(self):
        """Test parsing a module with a struct declaration."""
        code = b"""
module 0x1::coin {
    struct Coin<phantom CoinType> has key, store {
        value: u64,
    }
}
"""
        tree = self.parser.parse(code)
        root = tree.root_node

        self.assertFalse(root.has_error)
        self.assertEqual(root.type, "source_file")

        module = root.named_children[0]
        self.assertEqual(module.type, "module_declaration")

        body = module.child_by_field_name("body")
        self.assertIsNotNone(body)

        struct_decl = next(
            (c for c in body.named_children if c.type == "struct_declaration"),
            None,
        )
        self.assertIsNotNone(struct_decl)
        name_node = struct_decl.child_by_field_name("name")
        self.assertEqual(name_node.text, b"Coin")

    def test_parse_function_with_control_flow(self):
        """Test parsing a function with if/else."""
        code = b"""
module 0x1::example {
    public fun transfer(from: &signer, to: address, amount: u64): bool {
        if (amount > 0) {
            true
        } else {
            false
        }
    }
}
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

        body = tree.root_node.named_children[0].child_by_field_name("body")
        func = next(
            (c for c in body.named_children if c.type == "function_declaration"),
            None,
        )
        self.assertIsNotNone(func)
        self.assertEqual(func.child_by_field_name("name").text, b"transfer")

    def test_parse_enum_and_match(self):
        """Test parsing enum declarations and match expressions."""
        code = b"""
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
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

        body = tree.root_node.named_children[0].child_by_field_name("body")
        members = body.named_children

        enum_decl = next((c for c in members if c.type == "enum_declaration"), None)
        self.assertIsNotNone(enum_decl)
        self.assertEqual(enum_decl.child_by_field_name("name").text, b"Color")

        func = next((c for c in members if c.type == "function_declaration"), None)
        self.assertIsNotNone(func)

    def test_parse_use_declarations(self):
        """Test parsing various use declaration forms."""
        code = b"""
module 0x1::my_module {
    use std::vector;
    use 0x1::coin::{Self, Coin};
    use 0x1::account::Account as MyAccount;
}
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

        body = tree.root_node.named_children[0].child_by_field_name("body")
        uses = [c for c in body.named_children if c.type == "use_declaration"]
        self.assertEqual(len(uses), 3)

    def test_parse_spec_block(self):
        """Test parsing specification blocks."""
        code = b"""
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
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

        body = tree.root_node.named_children[0].child_by_field_name("body")
        specs = [c for c in body.named_children if c.type == "spec_block"]
        self.assertEqual(len(specs), 2)

    def test_parse_attributes(self):
        """Test parsing attribute annotations."""
        code = b"""
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
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

    def test_parse_generics_and_abilities(self):
        """Test parsing generic types with ability constraints."""
        code = b"""
module 0x1::container {
    struct Box<T: store + drop> has key {
        value: T,
    }

    public fun unbox<T: store + drop>(box: Box<T>): T {
        let Box { value } = box;
        value
    }
}
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

    def test_parse_lambda_expression(self):
        """Test parsing lambda/closure expressions."""
        code = b"""
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
"""
        tree = self.parser.parse(code)
        self.assertFalse(tree.root_node.has_error)

    def test_parse_address_block(self):
        """Test parsing address blocks containing modules."""
        code = b"""
address 0x42 {
    module first {
        public fun one(): u64 { 1 }
    }
    module second {
        public fun two(): u64 { 2 }
    }
}
"""
        tree = self.parser.parse(code)
        root = tree.root_node

        self.assertFalse(root.has_error)
        addr_block = root.named_children[0]
        self.assertEqual(addr_block.type, "address_block")

    def test_tree_walk(self):
        """Test walking the parse tree with a cursor."""
        code = b"""
module 0x1::example {
    public fun hello(): u64 { 42 }
}
"""
        tree = self.parser.parse(code)
        cursor = tree.walk()

        # Root should be source_file
        self.assertEqual(cursor.node.type, "source_file")

        # Walk down to module
        self.assertTrue(cursor.goto_first_child())
        self.assertEqual(cursor.node.type, "module_declaration")

    def test_query(self):
        """Test running a tree-sitter query to find function declarations."""
        code = b"""
module 0x1::example {
    public fun alpha(): u64 { 1 }
    public fun beta(): u64 { 2 }
    fun gamma(): u64 { 3 }
}
"""
        tree = self.parser.parse(code)

        query = tree_sitter.Query(
            self.language,
            "(function_declaration name: (identifier) @name)",
        )
        cursor = tree_sitter.QueryCursor(query)
        matches = cursor.matches(tree.root_node)

        names = []
        for _, captures_dict in matches:
            for node_list in captures_dict.values():
                for node in node_list:
                    names.append(node.text.decode())

        self.assertIn("alpha", names)
        self.assertIn("beta", names)
        self.assertIn("gamma", names)
        self.assertEqual(len(names), 3)
