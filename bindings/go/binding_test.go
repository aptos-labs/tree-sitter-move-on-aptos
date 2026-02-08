package tree_sitter_move_on_aptos_test

import (
	"testing"

	tree_sitter_move_on_aptos "github.com/aptos-labs/tree-sitter-move-on-aptos/bindings/go"
	tree_sitter "github.com/tree-sitter/go-tree-sitter"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_move_on_aptos.Language())
	if language == nil {
		t.Errorf("Error loading MoveOnAptos grammar")
	}
}

func newParser(t *testing.T) *tree_sitter.Parser {
	t.Helper()
	parser := tree_sitter.NewParser()
	lang := tree_sitter.NewLanguage(tree_sitter_move_on_aptos.Language())
	if lang == nil {
		t.Fatal("Error loading MoveOnAptos grammar")
	}
	err := parser.SetLanguage(lang)
	if err != nil {
		t.Fatalf("Error setting language: %v", err)
	}
	return parser
}

func TestParseModuleDeclaration(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
module 0x1::coin {
    struct Coin<phantom CoinType> has key, store {
        value: u64,
    }
}
`)
	tree := parser.Parse(source, nil)
	root := tree.RootNode()

	if root.HasError() {
		t.Error("parse tree should not have errors")
	}
	if root.Kind() != "source_file" {
		t.Errorf("expected source_file, got %s", root.Kind())
	}

	module := root.NamedChild(0)
	if module == nil || module.Kind() != "module_declaration" {
		t.Error("expected module_declaration as first child")
	}

	body := module.ChildByFieldName("body")
	if body == nil {
		t.Fatal("module should have a body")
	}

	var structDecl *tree_sitter.Node
	for i := uint(0); i < body.NamedChildCount(); i++ {
		child := body.NamedChild(i)
		if child.Kind() == "struct_declaration" {
			structDecl = child
			break
		}
	}
	if structDecl == nil {
		t.Fatal("module body should contain a struct_declaration")
	}

	name := structDecl.ChildByFieldName("name")
	if name == nil || name.Utf8Text(source) != "Coin" {
		t.Errorf("expected struct name 'Coin', got '%s'", name.Utf8Text(source))
	}
}

func TestParseFunctionWithControlFlow(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
module 0x1::example {
    public fun transfer(from: &signer, to: address, amount: u64): bool {
        if (amount > 0) {
            true
        } else {
            false
        }
    }
}
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}
}

func TestParseEnumAndMatch(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
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
`)
	tree := parser.Parse(source, nil)
	root := tree.RootNode()

	if root.HasError() {
		t.Error("parse tree should not have errors")
	}

	body := root.NamedChild(0).ChildByFieldName("body")
	var hasEnum, hasFunc bool
	for i := uint(0); i < body.NamedChildCount(); i++ {
		child := body.NamedChild(i)
		switch child.Kind() {
		case "enum_declaration":
			hasEnum = true
		case "function_declaration":
			hasFunc = true
		}
	}
	if !hasEnum {
		t.Error("should contain an enum_declaration")
	}
	if !hasFunc {
		t.Error("should contain a function_declaration")
	}
}

func TestParseUseDeclarations(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
module 0x1::my_module {
    use std::vector;
    use 0x1::coin::{Self, Coin};
    use 0x1::account::Account as MyAccount;
}
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}

	body := tree.RootNode().NamedChild(0).ChildByFieldName("body")
	useCount := 0
	for i := uint(0); i < body.NamedChildCount(); i++ {
		if body.NamedChild(i).Kind() == "use_declaration" {
			useCount++
		}
	}
	if useCount != 3 {
		t.Errorf("expected 3 use declarations, got %d", useCount)
	}
}

func TestParseSpecBlock(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
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
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}

	body := tree.RootNode().NamedChild(0).ChildByFieldName("body")
	specCount := 0
	for i := uint(0); i < body.NamedChildCount(); i++ {
		if body.NamedChild(i).Kind() == "spec_block" {
			specCount++
		}
	}
	if specCount != 2 {
		t.Errorf("expected 2 spec blocks, got %d", specCount)
	}
}

func TestParseAttributes(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
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
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}
}

func TestParseGenerics(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
module 0x1::container {
    struct Box<T: store + drop> has key {
        value: T,
    }

    public fun unbox<T: store + drop>(box: Box<T>): T {
        let Box { value } = box;
        value
    }
}
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}
}

func TestParseLambdaExpression(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
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
`)
	tree := parser.Parse(source, nil)
	if tree.RootNode().HasError() {
		t.Error("parse tree should not have errors")
	}
}

func TestParseAddressBlock(t *testing.T) {
	parser := newParser(t)
	source := []byte(`
address 0x42 {
    module first {
        public fun one(): u64 { 1 }
    }
    module second {
        public fun two(): u64 { 2 }
    }
}
`)
	tree := parser.Parse(source, nil)
	root := tree.RootNode()

	if root.HasError() {
		t.Error("parse tree should not have errors")
	}

	addrBlock := root.NamedChild(0)
	if addrBlock == nil || addrBlock.Kind() != "address_block" {
		t.Errorf("expected address_block, got %v", addrBlock)
	}
}
