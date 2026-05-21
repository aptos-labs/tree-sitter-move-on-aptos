use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use std::process;

use rayon::prelude::*;
use walkdir::WalkDir;

// ─── Configuration ─────────────────────────────────────────────

struct BoundsConfig {
    max_loop_depth: usize,
    max_generic_instantiation_length: usize,
    max_function_parameters: usize,
    max_basic_blocks: usize,
    max_type_nodes: usize,
    max_function_return_values: usize,
    max_type_depth: usize,
}

impl Default for BoundsConfig {
    fn default() -> Self {
        // Values from aptos-core/aptos-move/aptos-vm-environment/src/prod_configs.rs
        // with enable_function_values=true (current mainnet production config).
        Self {
            max_loop_depth: 5,
            max_generic_instantiation_length: 32,
            max_function_parameters: 128,
            max_basic_blocks: 1024,
            max_type_nodes: 128,
            max_function_return_values: 128,
            max_type_depth: 20,
        }
    }
}

// ─── Violations ────────────────────────────────────────────────

struct Violation {
    kind: &'static str,
    entity_kind: &'static str,
    entity: String,
    actual: usize,
    limit: usize,
    line: usize,
    col: usize,
}

// ─── Helpers ───────────────────────────────────────────────────

fn line_col(source: &str, byte_offset: usize) -> (usize, usize) {
    let prefix = &source[..byte_offset];
    let line = prefix.matches('\n').count() + 1;
    let col = prefix
        .rfind('\n')
        .map(|i| byte_offset - i)
        .unwrap_or(byte_offset + 1);
    (line, col)
}

fn count_named_children(node: tree_sitter::Node) -> usize {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).count()
}

fn is_type_node(kind: &str) -> bool {
    matches!(
        kind,
        "apply_type" | "primitive_type" | "ref_type" | "tuple_type" | "function_type"
    )
}

// ─── Check 1: Loop Depth ───────────────────────────────────────

fn max_loop_depth(node: tree_sitter::Node, depth: usize) -> usize {
    let is_loop = matches!(
        node.kind(),
        "while_expression" | "loop_expression" | "for_expression"
    );
    let new_depth = if is_loop { depth + 1 } else { depth };

    let mut max = new_depth;
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        let d = max_loop_depth(child, new_depth);
        if d > max {
            max = d;
        }
    }
    max
}

// ─── Check 2: Generic Instantiation Length ─────────────────────

fn max_type_arguments_length(node: tree_sitter::Node) -> usize {
    let mut max = 0;
    if node.kind() == "type_arguments" {
        let count = count_named_children(node);
        if count > max {
            max = count;
        }
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        let d = max_type_arguments_length(child);
        if d > max {
            max = d;
        }
    }
    max
}

// ─── Check 4: Basic Blocks (Heuristic) ────────────────────────

fn estimate_basic_blocks(node: tree_sitter::Node) -> usize {
    let mut count = 1; // entry block
    count_blocks_inner(node, &mut count);
    count
}

fn count_blocks_inner(node: tree_sitter::Node, count: &mut usize) {
    match node.kind() {
        "if_expression" | "while_expression" | "for_expression" => *count += 2,
        "loop_expression" | "break_expression" | "continue_expression" => *count += 1,
        _ => {}
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        count_blocks_inner(child, count);
    }
}

// ─── Check 5: Type Node Count ─────────────────────────────────

fn count_type_nodes(node: tree_sitter::Node) -> usize {
    let mut count = if is_type_node(node.kind()) { 1 } else { 0 };
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        count += count_type_nodes(child);
    }
    count
}

// ─── Check 7: Type Depth ──────────────────────────────────────

fn compute_type_depth(node: tree_sitter::Node) -> usize {
    if is_type_node(node.kind()) {
        let mut max_child = 0;
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let d = compute_type_depth(child);
            if d > max_child {
                max_child = d;
            }
        }
        1 + max_child
    } else {
        // Not a type node — pass through (e.g. type_arguments wrapper)
        let mut max = 0;
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let d = compute_type_depth(child);
            if d > max {
                max = d;
            }
        }
        max
    }
}

// ─── Main Check Logic ──────────────────────────────────────────

fn check_file(tree: &tree_sitter::Tree, source: &str, config: &BoundsConfig) -> Vec<Violation> {
    let mut violations = Vec::new();
    walk_declarations(tree.root_node(), source, config, &mut violations);
    violations
}

fn walk_declarations(
    node: tree_sitter::Node,
    source: &str,
    config: &BoundsConfig,
    violations: &mut Vec<Violation>,
) {
    match node.kind() {
        "spec_block" => return,
        "function_declaration" => check_function(node, source, config, violations),
        "struct_declaration" | "enum_declaration" => {
            check_struct(node, source, config, violations)
        }
        _ => {}
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        walk_declarations(child, source, config, violations);
    }
}

fn check_function(
    node: tree_sitter::Node,
    source: &str,
    config: &BoundsConfig,
    violations: &mut Vec<Violation>,
) {
    let name = node
        .child_by_field_name("name")
        .and_then(|n| n.utf8_text(source.as_bytes()).ok())
        .unwrap_or("<unknown>")
        .to_string();
    let (line, col) = line_col(source, node.start_byte());
    let body = node.child_by_field_name("body");

    // Check 3: function parameters
    if let Some(params) = node.child_by_field_name("parameters") {
        let count = count_named_children(params);
        if count > config.max_function_parameters {
            violations.push(Violation {
                kind: "max_function_parameters",
                entity_kind: "function",
                entity: name.clone(),
                actual: count,
                limit: config.max_function_parameters,
                line,
                col,
            });
        }
    }

    // Check 6: return values
    if let Some(ret) = node.child_by_field_name("return_type") {
        if ret.kind() == "tuple_type" {
            let count = count_named_children(ret);
            if count > config.max_function_return_values {
                violations.push(Violation {
                    kind: "max_function_return_values",
                    entity_kind: "function",
                    entity: name.clone(),
                    actual: count,
                    limit: config.max_function_return_values,
                    line,
                    col,
                });
            }
        }
    }

    // Check 2: generic instantiation length
    // Check both type_parameters on the declaration and type_arguments everywhere
    let max_generic = {
        let mut max = 0;
        if let Some(tp) = node.child_by_field_name("type_parameters") {
            max = count_named_children(tp);
        }
        let ta = max_type_arguments_length(node);
        if ta > max {
            max = ta;
        }
        max
    };
    if max_generic > config.max_generic_instantiation_length {
        violations.push(Violation {
            kind: "max_generic_instantiation_length",
            entity_kind: "function",
            entity: name.clone(),
            actual: max_generic,
            limit: config.max_generic_instantiation_length,
            line,
            col,
        });
    }

    // Body-dependent checks (skip native functions)
    if let Some(body) = body {
        // Check 1: loop depth
        let depth = max_loop_depth(body, 0);
        if depth > config.max_loop_depth {
            violations.push(Violation {
                kind: "max_loop_depth",
                entity_kind: "function",
                entity: name.clone(),
                actual: depth,
                limit: config.max_loop_depth,
                line,
                col,
            });
        }

        // Check 4: basic blocks (heuristic)
        let blocks = estimate_basic_blocks(body);
        if blocks > config.max_basic_blocks {
            violations.push(Violation {
                kind: "max_basic_blocks",
                entity_kind: "function",
                entity: name.clone(),
                actual: blocks,
                limit: config.max_basic_blocks,
                line,
                col,
            });
        }

        // Check 5: type nodes (across entire function scope)
        let mut tn = count_type_nodes(body);
        if let Some(params) = node.child_by_field_name("parameters") {
            tn += count_type_nodes(params);
        }
        if let Some(ret) = node.child_by_field_name("return_type") {
            tn += count_type_nodes(ret);
        }
        if tn > config.max_type_nodes {
            violations.push(Violation {
                kind: "max_type_nodes",
                entity_kind: "function",
                entity: name.clone(),
                actual: tn,
                limit: config.max_type_nodes,
                line,
                col,
            });
        }

        // Check 7: type depth (across entire function scope)
        let mut td = compute_type_depth(body);
        if let Some(params) = node.child_by_field_name("parameters") {
            let d = compute_type_depth(params);
            if d > td {
                td = d;
            }
        }
        if let Some(ret) = node.child_by_field_name("return_type") {
            let d = compute_type_depth(ret);
            if d > td {
                td = d;
            }
        }
        if td > config.max_type_depth {
            violations.push(Violation {
                kind: "max_type_depth",
                entity_kind: "function",
                entity: name.clone(),
                actual: td,
                limit: config.max_type_depth,
                line,
                col,
            });
        }
    }
}

fn check_struct(
    node: tree_sitter::Node,
    source: &str,
    config: &BoundsConfig,
    violations: &mut Vec<Violation>,
) {
    let kind_label = if node.kind() == "enum_declaration" {
        "enum"
    } else {
        "struct"
    };
    let name = node
        .child_by_field_name("name")
        .and_then(|n| n.utf8_text(source.as_bytes()).ok())
        .unwrap_or("<unknown>")
        .to_string();
    let (line, col) = line_col(source, node.start_byte());

    // Check 2: generic instantiation length on declaration + field types
    let max_generic = {
        let mut max = 0;
        if let Some(tp) = node.child_by_field_name("type_parameters") {
            max = count_named_children(tp);
        }
        let ta = max_type_arguments_length(node);
        if ta > max {
            max = ta;
        }
        max
    };
    if max_generic > config.max_generic_instantiation_length {
        violations.push(Violation {
            kind: "max_generic_instantiation_length",
            entity_kind: kind_label,
            entity: name,
            actual: max_generic,
            limit: config.max_generic_instantiation_length,
            line,
            col,
        });
    }
}

// ─── CLI ───────────────────────────────────────────────────────

fn parse_override(arg: &str, config: &mut BoundsConfig) -> bool {
    let overrides: &[(&str, fn(&mut BoundsConfig, usize))] = &[
        ("--max-loop-depth=", |c, v| c.max_loop_depth = v),
        ("--max-generic-instantiation-length=", |c, v| {
            c.max_generic_instantiation_length = v
        }),
        ("--max-function-parameters=", |c, v| {
            c.max_function_parameters = v
        }),
        ("--max-basic-blocks=", |c, v| c.max_basic_blocks = v),
        ("--max-type-nodes=", |c, v| c.max_type_nodes = v),
        ("--max-function-return-values=", |c, v| {
            c.max_function_return_values = v
        }),
        ("--max-type-depth=", |c, v| c.max_type_depth = v),
    ];
    for (prefix, setter) in overrides {
        if let Some(val) = arg.strip_prefix(prefix) {
            match val.parse::<usize>() {
                Ok(v) => {
                    setter(config, v);
                    return true;
                }
                Err(_) => {
                    eprintln!(
                        "Invalid value for {}: {}",
                        prefix.trim_end_matches('='),
                        val
                    );
                    process::exit(2);
                }
            }
        }
    }
    false
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut config = BoundsConfig::default();
    let mut paths = Vec::new();

    for arg in &args[1..] {
        if arg.starts_with("--") {
            if !parse_override(arg, &mut config) {
                eprintln!("Unknown option: {}", arg);
                process::exit(2);
            }
        } else {
            paths.push(arg.clone());
        }
    }

    if paths.is_empty() {
        eprintln!("Usage: move-bounds-checker <dir> [--max-loop-depth=N ...]");
        process::exit(2);
    }

    // Collect .move files
    let files: Vec<PathBuf> = paths
        .iter()
        .flat_map(|p| {
            let path = PathBuf::from(p);
            if path.is_dir() {
                WalkDir::new(&path)
                    .into_iter()
                    .filter_map(|e| e.ok())
                    .filter(|e| {
                        e.path()
                            .extension()
                            .is_some_and(|ext| ext == "move")
                    })
                    .map(|e| e.path().to_path_buf())
                    .collect::<Vec<_>>()
            } else {
                vec![path]
            }
        })
        .collect();

    eprintln!("Scanning {} file(s)...", files.len());

    // Process files in parallel, one Parser per thread
    let results: Vec<(PathBuf, Vec<Violation>)> = files
        .par_iter()
        .map_init(
            || {
                let mut parser = tree_sitter::Parser::new();
                parser
                    .set_language(&tree_sitter_move_on_aptos::language())
                    .expect("Error loading Move grammar");
                parser
            },
            |parser, path| {
                let source = match fs::read_to_string(path) {
                    Ok(s) => s,
                    Err(_) => return (path.clone(), Vec::new()),
                };
                let tree = match parser.parse(&source, None) {
                    Some(t) => t,
                    None => return (path.clone(), Vec::new()),
                };
                let violations = check_file(&tree, &source, &config);
                (path.clone(), violations)
            },
        )
        .collect();

    // Output violations
    let mut total = 0;
    let mut by_kind: HashMap<&str, usize> = HashMap::new();

    for (path, violations) in &results {
        for v in violations {
            println!(
                "{}:{}:{}: {} '{}' exceeds {} ({} > {})",
                path.display(),
                v.line,
                v.col,
                v.entity_kind,
                v.entity,
                v.kind,
                v.actual,
                v.limit,
            );
            *by_kind.entry(v.kind).or_insert(0) += 1;
            total += 1;
        }
    }

    // Summary on stderr
    eprintln!(
        "{} file(s) scanned, {} violation(s) found",
        files.len(),
        total
    );
    if !by_kind.is_empty() {
        let mut sorted: Vec<_> = by_kind.iter().collect();
        sorted.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in sorted {
            eprintln!("  {}: {}", kind, count);
        }
    }

    process::exit(if total > 0 { 1 } else { 0 });
}
