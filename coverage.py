"""Grammar coverage report for tree-sitter-move-on-aptos.

Measures how many named node types and named fields defined in
src/node-types.json are exercised by at least one corpus test in
test/corpus/*.txt.

Usage:
    python coverage.py [--min-node-pct N] [--min-field-pct N] [--verbose]

Exit code is non-zero if coverage is below the minimum thresholds.
"""
import argparse
import glob
import json
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).parent
NODE_TYPES_PATH = REPO_ROOT / "src" / "node-types.json"
CORPUS_GLOB = str(REPO_ROOT / "test" / "corpus" / "*.txt")

# Node types that are grammar limitations or not yet implemented —
# excluded from the "uncovered" report so the gap list stays actionable.
KNOWN_GAPS = {
    "proof_body",        # proof { } blocks: V2.5 spec feature, not yet supported
    "proof_statement",   # same
}


def load_node_types():
    with open(NODE_TYPES_PATH) as f:
        return json.load(f)


def corpus_text():
    parts = []
    for path in sorted(glob.glob(CORPUS_GLOB)):
        with open(path) as f:
            parts.append(f.read())
    return "\n".join(parts)


def covered_nodes_and_fields(text):
    node_re = re.compile(r'\((\w+)')
    field_re = re.compile(r'  (\w+): \(')
    nodes = set(m.group(1) for m in node_re.finditer(text))
    fields = set(m.group(1) for m in field_re.finditer(text))
    return nodes, fields


def analyze(node_types, corpus):
    covered_nodes, covered_fields = covered_nodes_and_fields(corpus)

    named_nodes = [n for n in node_types if n.get("named") and not n["type"].startswith("_")]

    node_results = []
    for n in named_nodes:
        ntype = n["type"]
        node_results.append({
            "type": ntype,
            "covered": ntype in covered_nodes,
            "known_gap": ntype in KNOWN_GAPS,
        })

    field_results = []
    for n in named_nodes:
        for fname, fdef in n.get("fields", {}).items():
            # Skip fields whose values are exclusively anonymous tokens — those
            # cannot appear as named nodes in S-expression output.
            types = fdef.get("types", [])
            if not any(t.get("named") for t in types):
                continue
            field_results.append({
                "node": n["type"],
                "field": fname,
                "covered": fname in covered_fields,
            })

    return node_results, field_results


def print_report(node_results, field_results, verbose):
    total_nodes = len(node_results)
    covered_nodes = sum(1 for n in node_results if n["covered"])
    actionable_uncovered = [n for n in node_results if not n["covered"] and not n["known_gap"]]

    total_fields = len(field_results)
    covered_fields = sum(1 for f in field_results if f["covered"])
    uncovered_fields = [f for f in field_results if not f["covered"]]

    node_pct = 100.0 * covered_nodes / total_nodes if total_nodes else 0
    field_pct = 100.0 * covered_fields / total_fields if total_fields else 0

    print(f"Node type coverage:  {covered_nodes}/{total_nodes} ({node_pct:.1f}%)")
    print(f"Named field coverage: {covered_fields}/{total_fields} ({field_pct:.1f}%)")

    if actionable_uncovered:
        print(f"\nUncovered node types ({len(actionable_uncovered)}):")
        for n in sorted(actionable_uncovered, key=lambda x: x["type"]):
            print(f"  {n['type']}")

    known_gap_nodes = [n for n in node_results if not n["covered"] and n["known_gap"]]
    if known_gap_nodes and verbose:
        print(f"\nKnown grammar-limitation gaps ({len(known_gap_nodes)}) [excluded from threshold]:")
        for n in sorted(known_gap_nodes, key=lambda x: x["type"]):
            print(f"  {n['type']}")

    if uncovered_fields:
        print(f"\nUncovered named fields ({len(uncovered_fields)}):")
        for f in sorted(uncovered_fields, key=lambda x: (x["node"], x["field"])):
            print(f"  {f['node']}.{f['field']}")

    if verbose and covered_nodes == total_nodes and not uncovered_fields:
        print("\nFull coverage achieved.")

    return node_pct, field_pct


def main():
    parser = argparse.ArgumentParser(description="Grammar corpus coverage report")
    parser.add_argument("--min-node-pct", type=float, default=0.0,
                        help="Fail if node coverage is below this percentage (default: 0)")
    parser.add_argument("--min-field-pct", type=float, default=0.0,
                        help="Fail if field coverage is below this percentage (default: 0)")
    parser.add_argument("--verbose", action="store_true",
                        help="Show known-gap list and full-coverage confirmation")
    args = parser.parse_args()

    node_types = load_node_types()
    corpus = corpus_text()
    node_results, field_results = analyze(node_types, corpus)
    node_pct, field_pct = print_report(node_results, field_results, args.verbose)

    failed = False
    if node_pct < args.min_node_pct:
        print(f"\nFAIL: node coverage {node_pct:.1f}% < required {args.min_node_pct}%")
        failed = True
    if field_pct < args.min_field_pct:
        print(f"\nFAIL: field coverage {field_pct:.1f}% < required {args.min_field_pct}%")
        failed = True

    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
