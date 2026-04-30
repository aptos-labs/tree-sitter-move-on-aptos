# Move 1 → Move 2 Migration Tool — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a Rust CLI tool that uses tree-sitter to transform Move 1 global storage syntax into Move 2 index notation, and strip `acquires` annotations.

**Architecture:** Parse `.move` files with tree-sitter using the Move grammar. Walk the AST to find matching patterns (call expressions for `borrow_global`/`borrow_global_mut`, dereference expressions wrapping those calls, acquires clauses). Collect byte-range edits and apply them back-to-front to preserve offsets.

**Tech Stack:** Rust, tree-sitter v0.26, tree-sitter-move-on-aptos (local path dep)

---

### Task 1: Scaffold the tool crate

**Files:**
- Create: `tools/move1-to-move2/Cargo.toml`
- Create: `tools/move1-to-move2/src/main.rs`

**Step 1: Create Cargo.toml**

```toml
[package]
name = "move1-to-move2"
version = "0.1.0"
edition = "2021"

[dependencies]
tree-sitter = "0.26"
tree-sitter-move-on-aptos = { path = "../.." }
```

**Step 2: Create minimal main.rs**

```rust
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("Usage: move1-to-move2 <FILE>...");
        process::exit(1);
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_move_on_aptos::language())
        .expect("Error loading Move grammar");

    for path in &args {
        let source = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Error reading {}: {}", path, e);
            process::exit(1);
        });
        println!("Parsed {} ({} bytes)", path, source.len());
    }
}
```

**Step 3: Verify it compiles and runs**

Run: `cargo build -p move1-to-move2`
Then: `cargo run -p move1-to-move2 -- test/test_package/build/test/sources/main.move`
Expected: prints "Parsed ... (N bytes)"

**Step 4: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: scaffold move1-to-move2 tool crate"
```

---

### Task 2: Implement the edit collection framework

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`

**Step 1: Add the Edit struct and apply_edits function**

```rust
#[derive(Debug)]
struct Edit {
    start_byte: usize,
    end_byte: usize,
    replacement: String,
    rule: &'static str,
}

fn apply_edits(source: &str, mut edits: Vec<Edit>) -> String {
    // Sort by start_byte descending so we can apply back-to-front
    edits.sort_by(|a, b| b.start_byte.cmp(&a.start_byte));

    let mut result = source.to_string();
    for edit in &edits {
        result.replace_range(edit.start_byte..edit.end_byte, &edit.replacement);
    }
    result
}
```

**Step 2: Add the recursive AST walker stub**

```rust
fn collect_edits(node: tree_sitter::Node, source: &[u8], edits: &mut Vec<Edit>) {
    // Will add rule matching here in subsequent tasks
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_edits(child, source, edits);
    }
}
```

**Step 3: Wire it into main**

Update main to call `collect_edits` on the parsed tree root, then `apply_edits`, and write the result back to the file if there were changes.

```rust
fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.is_empty() {
        eprintln!("Usage: move1-to-move2 <FILE>...");
        process::exit(1);
    }

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_move_on_aptos::language())
        .expect("Error loading Move grammar");

    let mut total_edits = 0;
    for path in &args {
        let source = fs::read_to_string(path).unwrap_or_else(|e| {
            eprintln!("Error reading {}: {}", path, e);
            process::exit(1);
        });

        let tree = parser.parse(&source, None).unwrap_or_else(|| {
            eprintln!("Failed to parse {}", path);
            process::exit(1);
        });

        let mut edits = Vec::new();
        collect_edits(tree.root_node(), source.as_bytes(), &mut edits);

        if edits.is_empty() {
            continue;
        }

        for edit in &edits {
            eprintln!("  [{}] {}:{}", edit.rule, path,
                tree.root_node().start_position().row + 1);
        }

        let result = apply_edits(&source, edits);
        let num_edits = result.len() != source.len() || result != source;
        if num_edits {
            fs::write(path, &result).unwrap_or_else(|e| {
                eprintln!("Error writing {}: {}", path, e);
                process::exit(1);
            });
        }
        total_edits += 1;
    }
    eprintln!("{} file(s) modified", total_edits);
}
```

**Step 4: Verify it compiles**

Run: `cargo build -p move1-to-move2`

**Step 5: Commit**

```bash
git add tools/move1-to-move2/src/main.rs
git commit -m "feat: add edit collection framework and apply_edits"
```

---

### Task 3: Implement Rule 1 — borrow_global → &Type[addr]

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`
- Create: `tools/move1-to-move2/tests/test_transforms.rs`

The AST for `borrow_global<Coin>(addr)` is:
```
(call_expression
  function: (name_access_chain (identifier))    # "borrow_global"
  type_arguments: (type_arguments ...)          # "<Coin>"
  arguments: (arg_list ...))                    # "(addr)"
```

**Step 1: Write the test**

Create `tools/move1-to-move2/tests/test_transforms.rs`:

```rust
use std::process::Command;
use std::fs;

fn transform(input: &str) -> String {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.move");
    fs::write(&path, input).unwrap();

    let output = Command::new("cargo")
        .args(["run", "-p", "move1-to-move2", "--"])
        .arg(&path)
        .output()
        .expect("failed to run tool");

    assert!(output.status.success(), "Tool failed: {}",
        String::from_utf8_lossy(&output.stderr));
    fs::read_to_string(&path).unwrap()
}

#[test]
fn test_borrow_global_simple() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires Coin {
        borrow_global<Coin>(addr);
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
        &Coin[addr];
    }
}"#;
    assert_eq!(transform(input), expected);
}

#[test]
fn test_borrow_global_qualified_type() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires coin::Coin {
        borrow_global<coin::Coin>(addr);
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
        &coin::Coin[addr];
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Add tempfile dev-dependency**

In `tools/move1-to-move2/Cargo.toml`:
```toml
[dev-dependencies]
tempfile = "3"
```

**Step 3: Implement the rule in collect_edits**

In `collect_edits`, before the recursive walk, add:

```rust
fn collect_edits(node: tree_sitter::Node, source: &[u8], edits: &mut Vec<Edit>) {
    if node.kind() == "call_expression" {
        if let Some(func_node) = node.child_by_field_name("function") {
            let func_name = func_node.utf8_text(source).unwrap_or("");
            if func_name == "borrow_global" {
                if let (Some(type_args), Some(args)) = (
                    node.child_by_field_name("type_arguments"),
                    node.child_by_field_name("arguments"),
                ) {
                    // Extract inner type text (strip < and >)
                    let type_text = type_args.utf8_text(source).unwrap_or("");
                    let inner_type = &type_text[1..type_text.len() - 1]; // strip < >

                    // Extract inner args text (strip ( and ))
                    let args_text = args.utf8_text(source).unwrap_or("");
                    let inner_args = &args_text[1..args_text.len() - 1]; // strip ( )

                    edits.push(Edit {
                        start_byte: node.start_byte(),
                        end_byte: node.end_byte(),
                        replacement: format!("&{}[{}]", inner_type, inner_args),
                        rule: "borrow_global",
                    });
                    return; // Don't recurse into this node's children
                }
            }
        }
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_edits(child, source, edits);
    }
}
```

**Step 4: Run the test**

Run: `cargo test -p move1-to-move2 -- test_borrow_global_simple`
Expected: PASS

**Step 5: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: implement borrow_global -> &Type[addr] transform"
```

---

### Task 4: Implement Rule 2 — borrow_global_mut → &mut Type[addr]

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`
- Modify: `tools/move1-to-move2/tests/test_transforms.rs`

**Step 1: Add tests**

```rust
#[test]
fn test_borrow_global_mut_simple() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires Counter {
        borrow_global_mut<Counter>(addr);
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
        &mut Counter[addr];
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Add the match arm**

In `collect_edits`, extend the function name matching to also handle `"borrow_global_mut"`:

```rust
let (prefix, rule_name) = match func_name {
    "borrow_global" => ("&", "borrow_global"),
    "borrow_global_mut" => ("&mut ", "borrow_global_mut"),
    _ => ("", ""),
};
if !prefix.is_empty() {
    // ... same logic, use prefix in format!("{}{}[{}]", prefix, inner_type, inner_args)
}
```

**Step 3: Run tests**

Run: `cargo test -p move1-to-move2`
Expected: all pass

**Step 4: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: implement borrow_global_mut -> &mut Type[addr] transform"
```

---

### Task 5: Implement Rule 3 — *borrow_global<T>(addr) → T[addr]

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`
- Modify: `tools/move1-to-move2/tests/test_transforms.rs`

The AST for `*borrow_global<Coin>(addr)` is:
```
(dereference_expression
  (call_expression
    function: (name_access_chain (identifier))   # "borrow_global"
    type_arguments: (type_arguments ...)
    arguments: (arg_list ...)))
```

**Step 1: Add test**

```rust
#[test]
fn test_deref_borrow_global() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires Counter {
        let x = *borrow_global<Counter>(addr);
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
        let x = Counter[addr];
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Add the dereference_expression match**

In `collect_edits`, BEFORE the call_expression check (so it takes priority):

```rust
if node.kind() == "dereference_expression" {
    // Check if child is a borrow_global/borrow_global_mut call
    let child = node.child(1); // child after the '*' token
    if let Some(child) = child {
        if child.kind() == "call_expression" {
            if let Some(func_node) = child.child_by_field_name("function") {
                let func_name = func_node.utf8_text(source).unwrap_or("");
                if func_name == "borrow_global" || func_name == "borrow_global_mut" {
                    if let (Some(type_args), Some(args)) = (
                        child.child_by_field_name("type_arguments"),
                        child.child_by_field_name("arguments"),
                    ) {
                        let type_text = type_args.utf8_text(source).unwrap_or("");
                        let inner_type = &type_text[1..type_text.len() - 1];
                        let args_text = args.utf8_text(source).unwrap_or("");
                        let inner_args = &args_text[1..args_text.len() - 1];

                        edits.push(Edit {
                            start_byte: node.start_byte(),
                            end_byte: node.end_byte(),
                            replacement: format!("{}[{}]", inner_type, inner_args),
                            rule: "deref_borrow_global",
                        });
                        return;
                    }
                }
            }
        }
    }
}
```

**Step 3: Run tests**

Run: `cargo test -p move1-to-move2`
Expected: all pass

**Step 4: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: implement *borrow_global -> Type[addr] transform"
```

---

### Task 6: Implement Rule 4 — *borrow_global_mut<T>(addr) = x → T[addr] = x

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`
- Modify: `tools/move1-to-move2/tests/test_transforms.rs`

The AST for `*borrow_global_mut<Counter>(addr) = x` is:
```
(assign_expression
  lhs: (dereference_expression
    (call_expression
      function: (name_access_chain (identifier))  # "borrow_global_mut"
      type_arguments: (type_arguments ...)
      arguments: (arg_list ...)))
  op: "="
  rhs: ...)
```

Note: Rule 3 (dereference_expression check) already handles `*borrow_global_mut<T>(addr)` by itself — converting the LHS from `*borrow_global_mut<Counter>(addr)` to `Counter[addr]`. The assignment `= x` part is outside the dereference expression and stays untouched. So **Rule 4 is already covered by Rule 3's implementation** — the deref check converts the LHS, and the `= x` remains.

**Step 1: Add test to verify**

```rust
#[test]
fn test_deref_borrow_global_mut_assignment() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires Counter {
        *borrow_global_mut<Counter>(addr) = Counter { i: 0 };
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
        Counter[addr] = Counter { i: 0 };
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Run tests**

Run: `cargo test -p move1-to-move2`
Expected: all pass (Rule 3 already handles this)

**Step 3: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "test: verify *borrow_global_mut assignment transform"
```

---

### Task 7: Implement Rule 5 — strip acquires annotations

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`
- Modify: `tools/move1-to-move2/tests/test_transforms.rs`

The AST node is `acquires_clause` which appears as a child of `_regular_function_declaration` / `_native_function_declaration`. The grammar field name is `acquires`.

The grammar at line 345-346 shows `acquires_clause` comes right before the body `block`. We need to remove the `acquires` node AND any whitespace between the preceding token (return type closing paren or `)`) and the acquires text.

**Step 1: Add tests**

```rust
#[test]
fn test_strip_acquires_single() {
    let input = r#"module 0x1::test {
    fun f(addr: address) acquires Counter {
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f(addr: address) {
    }
}"#;
    assert_eq!(transform(input), expected);
}

#[test]
fn test_strip_acquires_multiple() {
    let input = r#"module 0x1::test {
    fun f() acquires Counter, Balance {
    }
}"#;
    let expected = r#"module 0x1::test {
    fun f() {
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Add acquires_clause match in collect_edits**

```rust
if node.kind() == "acquires_clause" {
    // Remove the acquires clause plus preceding whitespace
    // Find the previous sibling's end to eat the space before "acquires"
    let start = if let Some(prev) = node.prev_sibling() {
        prev.end_byte()
    } else {
        node.start_byte()
    };
    edits.push(Edit {
        start_byte: start,
        end_byte: node.end_byte(),
        replacement: String::new(),
        rule: "strip_acquires",
    });
    return;
}
```

**Step 3: Run tests**

Run: `cargo test -p move1-to-move2`
Expected: all pass

**Step 4: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: implement acquires annotation stripping"
```

---

### Task 8: Add integration test with a realistic Move file

**Files:**
- Modify: `tools/move1-to-move2/tests/test_transforms.rs`

**Step 1: Add a comprehensive test**

```rust
#[test]
fn test_full_module_transform() {
    let input = r#"module 0x1::counter {
    struct Counter has key {
        value: u64,
    }

    public fun get_value(addr: address): u64 acquires Counter {
        borrow_global<Counter>(addr).value
    }

    public fun increment(addr: address) acquires Counter {
        let counter = borrow_global_mut<Counter>(addr);
        counter.value = counter.value + 1;
    }

    public fun reset(addr: address) acquires Counter {
        *borrow_global_mut<Counter>(addr) = Counter { value: 0 };
    }

    public fun read_value(addr: address): u64 acquires Counter {
        *borrow_global<Counter>(addr).value
    }
}"#;
    let expected = r#"module 0x1::counter {
    struct Counter has key {
        value: u64,
    }

    public fun get_value(addr: address): u64 {
        &Counter[addr].value
    }

    public fun increment(addr: address) {
        let counter = &mut Counter[addr];
        counter.value = counter.value + 1;
    }

    public fun reset(addr: address) {
        Counter[addr] = Counter { value: 0 };
    }

    public fun read_value(addr: address): u64 {
        Counter[addr].value
    }
}"#;
    assert_eq!(transform(input), expected);
}
```

**Step 2: Run all tests**

Run: `cargo test -p move1-to-move2`
Expected: all pass

**Step 3: Commit**

```bash
git add tools/move1-to-move2/
git commit -m "test: add comprehensive integration test for all transform rules"
```

---

### Task 9: Polish CLI output and error handling

**Files:**
- Modify: `tools/move1-to-move2/src/main.rs`

**Step 1: Improve summary output**

Print per-file edit counts and rule breakdown. Print line numbers for each edit:

```rust
// In the per-file loop, after collecting edits:
let num = edits.len();
for edit in &edits {
    // Find line number from byte offset
    let line = source[..edit.start_byte].matches('\n').count() + 1;
    eprintln!("  {}:{} [{}]", path, line, edit.rule);
}
eprintln!("  {} edit(s) in {}", num, path);
```

**Step 2: Verify it works on a real file**

Run: `cargo run -p move1-to-move2 -- test/test_package/build/test/sources/main.move`

**Step 3: Final commit**

```bash
git add tools/move1-to-move2/
git commit -m "feat: improve CLI output with per-edit line numbers and summary"
```

## Sources

- [Move 2 Release Notes](https://aptos.dev/build/smart-contracts/book/move-2)
- [Global Storage Operators](https://aptos.dev/build/smart-contracts/book/global-storage-operators)
