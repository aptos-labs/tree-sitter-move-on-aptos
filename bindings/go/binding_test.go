package tree_sitter_move_on_aptos_test

import (
	"testing"

	tree_sitter_move_on_aptos "github.com/aptos-labs/tree-sitter-move-on-aptos/bindings/go"
	tree_sitter "github.com/smacker/go-tree-sitter"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_move_on_aptos.Language())
	if language == nil {
		t.Errorf("Error loading MoveOnAptos grammar")
	}
}
