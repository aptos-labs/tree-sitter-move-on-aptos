const assert = require("node:assert");
const { test } = require("node:test");

const Parser = require("tree-sitter");
const MoveOnAptos = require(".");

test("can load grammar", () => {
  const parser = new Parser();
  assert.doesNotThrow(() => parser.setLanguage(MoveOnAptos));
});

test("can parse a module declaration", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
module 0x1::coin {
    struct Coin<phantom CoinType> has key, store {
        value: u64,
    }
}
`;
  const tree = parser.parse(sourceCode);
  const root = tree.rootNode;

  assert.strictEqual(root.type, "source_file");
  assert.ok(!root.hasError);

  const module = root.namedChildren[0];
  assert.strictEqual(module.type, "module_declaration");

  // Find the struct declaration inside the module body
  const moduleBody = module.childForFieldName("body");
  assert.ok(moduleBody, "module should have a body");

  const struct = moduleBody.namedChildren.find(
    (c) => c.type === "struct_declaration",
  );
  assert.ok(struct, "module body should contain a struct_declaration");
  assert.strictEqual(struct.childForFieldName("name").text, "Coin");
});

test("can parse a function with control flow", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
module 0x1::example {
    public fun transfer(from: &signer, to: address, amount: u64): bool {
        if (amount > 0) {
            true
        } else {
            false
        }
    }
}
`;
  const tree = parser.parse(sourceCode);
  const root = tree.rootNode;

  assert.ok(!root.hasError);

  const module = root.namedChildren[0];
  const moduleBody = module.childForFieldName("body");
  const func = moduleBody.namedChildren.find(
    (c) => c.type === "function_declaration",
  );
  assert.ok(func, "module body should contain a function_declaration");
  assert.strictEqual(func.childForFieldName("name").text, "transfer");
});

test("can parse enum and match expressions", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
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
`;
  const tree = parser.parse(sourceCode);
  const root = tree.rootNode;

  assert.ok(!root.hasError);

  const module = root.namedChildren[0];
  const moduleBody = module.childForFieldName("body");
  const members = moduleBody.namedChildren;

  const enumDecl = members.find((c) => c.type === "enum_declaration");
  assert.ok(enumDecl, "should contain an enum_declaration");
  assert.strictEqual(enumDecl.childForFieldName("name").text, "Color");

  const matchFn = members.find((c) => c.type === "function_declaration");
  assert.ok(matchFn, "should contain a function with match");
});

test("can parse use declarations", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
module 0x1::my_module {
    use std::vector;
    use 0x1::coin::{Self, Coin};
    use 0x1::account::Account as MyAccount;
}
`;
  const tree = parser.parse(sourceCode);
  assert.ok(!tree.rootNode.hasError);

  const body =
    tree.rootNode.namedChildren[0].childForFieldName("body");
  const uses = body.namedChildren.filter(
    (c) => c.type === "use_declaration",
  );
  assert.strictEqual(uses.length, 3);
});

test("can parse spec blocks", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
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
`;
  const tree = parser.parse(sourceCode);
  assert.ok(!tree.rootNode.hasError);

  const body =
    tree.rootNode.namedChildren[0].childForFieldName("body");
  const specBlocks = body.namedChildren.filter(
    (c) => c.type === "spec_block",
  );
  assert.strictEqual(specBlocks.length, 2);
});

test("can parse attributes", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
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
`;
  const tree = parser.parse(sourceCode);
  assert.ok(!tree.rootNode.hasError);
});

test("can parse generics and type constraints", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
module 0x1::container {
    struct Box<T: store + drop> has key {
        value: T,
    }

    public fun unbox<T: store + drop>(box: Box<T>): T {
        let Box { value } = box;
        value
    }
}
`;
  const tree = parser.parse(sourceCode);
  assert.ok(!tree.rootNode.hasError);
});

test("can parse vector and lambda expressions", () => {
  const parser = new Parser();
  parser.setLanguage(MoveOnAptos);

  const sourceCode = `
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
`;
  const tree = parser.parse(sourceCode);
  assert.ok(!tree.rootNode.hasError);
});
