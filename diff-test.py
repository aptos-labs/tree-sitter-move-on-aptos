"""Differential grammar test for tree-sitter-move-on-aptos.

Parses every .move file reachable from the given paths and checks for two
classes of problems that batch-test.py does not catch:

1. Hidden ERROR / MISSING nodes — tree-sitter accepted the file (exit 0) but
   inserted error-recovery nodes internally.  These indicate the grammar is
   over-accepting and silently mis-parsing real code.

2. Unknown node types — node types that appear in parse output but are not
   listed in src/node-types.json.  This should never happen; it would signal a
   bug in the grammar or node-types.json generation.

Usage:
    python diff-test.py PATH [PATH ...] [--only-stats]

Exit code is non-zero if any file has hidden errors (unless --only-stats).
"""
import argparse
import json
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import List, Tuple

from config import exclude

REPO_ROOT = Path(__file__).parent
NODE_TYPES_PATH = REPO_ROOT / "src" / "node-types.json"

# Tree-sitter S-expression patterns
ERROR_RE = re.compile(r'\(ERROR ')
MISSING_RE = re.compile(r'\(MISSING ')
NODE_NAME_RE = re.compile(r'\(([a-z_][a-z0-9_]*)(?: |\n)')


def load_known_types() -> set:
    with open(NODE_TYPES_PATH) as f:
        return {n["type"] for n in json.load(f) if n.get("named")}


def retain_file(path: str) -> bool:
    return all(excl not in path for excl in exclude)


def walk_move_files(root: str) -> List[str]:
    result = []
    for dirpath, _, files in os.walk(root):
        for fname in files:
            if fname.endswith(".move"):
                full = os.path.join(dirpath, fname)
                if retain_file(full):
                    result.append(full)
    return result


def parse_file(path: str) -> Tuple[bool, str]:
    proc = subprocess.run(["tree-sitter", "parse", path], capture_output=True)
    output = proc.stdout.decode("utf-8", errors="replace")
    return proc.returncode == 0, output


def check_file(path: str, known_types: set) -> List[str]:
    ok, output = parse_file(path)
    if not ok:
        return []  # batch-test.py already catches parse failures

    problems = []
    if ERROR_RE.search(output):
        # Find the first ERROR location
        m = re.search(r'\(ERROR \[(\d+), \d+\]', output)
        loc = f"[{m.group(1)},…]" if m else ""
        problems.append(f"  hidden ERROR node {loc}")
    if MISSING_RE.search(output):
        m = re.search(r'\(MISSING [^\]]+\[(\d+), \d+\]', output)
        loc = f"[{m.group(1)},…]" if m else ""
        problems.append(f"  hidden MISSING node {loc}")

    unknown = {n for n in NODE_NAME_RE.findall(output) if n not in known_types}
    for u in sorted(unknown):
        problems.append(f"  unknown node type: {u}")

    return problems


def main():
    parser = argparse.ArgumentParser(description="Differential grammar test")
    parser.add_argument("PATH", nargs="+", help="Directories to search")
    parser.add_argument("--only-stats", action="store_true",
                        help="Print statistics only; do not fail on errors")
    args = parser.parse_args()

    known_types = load_known_types()
    files: List[str] = []
    for p in args.PATH:
        if not os.path.isdir(p):
            print(f"Not a directory: {p}", file=sys.stderr)
            sys.exit(1)
        files.extend(walk_move_files(p))

    hidden_errors = 0
    checked = 0
    for path in sorted(files):
        problems = check_file(path, known_types)
        if problems:
            print(f"[DIFF] {path}")
            for prob in problems:
                print(prob)
            hidden_errors += 1
        checked += 1

    print(f"\nChecked {checked} files; {hidden_errors} with hidden parse errors.")
    if hidden_errors and not args.only_stats:
        sys.exit(1)


if __name__ == "__main__":
    main()
