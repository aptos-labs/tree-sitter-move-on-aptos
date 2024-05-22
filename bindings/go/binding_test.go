package tree_sitter_move_aptos_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-move_aptos"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_move_aptos.Language())
	if language == nil {
		t.Errorf("Error loading MoveAptos grammar")
	}
}
