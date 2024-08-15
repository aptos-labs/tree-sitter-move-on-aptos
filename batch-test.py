'''This script is used to batch test move files recursively in a directory.
It will search for all files with `.move` extension and run `tree-sitter parse` on them.
If you updated the parser, you should run `tree-sitter generate` first.
'''
from functools import reduce
import os
import re
import subprocess
import argparse
from operator import add
from typing import List, Optional, Tuple

from config import exclude, special_folder, error_messages

PROMPT_EXTRACT = re.compile(r'\(([A-Z]+( [^\[\]]+)?) \[(\d+), \d+\] - \[(\d+), \d+\]\)')
BAD_LINE = re.compile(r'^(abort|return) [^\r\n]*$')
MOVE_MODULE_PATH = 'aptos-labs/move-modules'

class TestResult:
    def __init__(self, path: str, passed: bool, last_output = '') -> None:
        self.path = path
        self.passed = passed
        self.last_output = last_output


def list_move_codes(path: str) -> List[str]:
    result = []
    name_filter = re.compile(r'^.*\.move$')
    for root, _, files in os.walk(path):
        result.extend(
            map(lambda f: os.path.join(root, f),
            filter(lambda file: name_filter.match(file) is not None, files))
        )
    return result


def decompiled_workaround(file: str, prompt: str) -> bool:
    if file.find(MOVE_MODULE_PATH) == -1:
        return False
    
    match = PROMPT_EXTRACT.search(prompt)
    if match is None:
        return False
    # Extract the line numbers
    line_begin = int(match.group(3))
    line_end = int(match.group(4))

    if line_begin != line_end:
        return False

    with open(file, 'r') as f:
        lines = f.readlines()
        if line_begin >= len(lines):
            return False
        line = lines[line_begin].strip()
        if BAD_LINE.match(line) is not None:
            return True
        return line[-1] != ';'


def visit_file(file: str) -> TestResult:
    proc = subprocess.run(
        args=['tree-sitter', 'parse', file],
        capture_output=True
    )
    if proc.returncode == 0:
        return TestResult(file, True)
    last_line = ''
    for line in proc.stdout.splitlines():
        last_line = line
    last_line = last_line.decode("utf-8")

    return TestResult(file, decompiled_workaround(file, last_line), last_line)


def retain_file(file: str) -> bool:
    for excl in exclude:
        if file.find(excl) != -1:
            print(f'[IGNORE] {file}')
            return False
    return True


def assert_valid_path(path: str):
    if not os.path.exists(path):
        print(f'Path {path} does not exist!')
        exit(-1)
    if not os.path.isdir(path):
        print(f'Path `{path}` is not a directory!')
        exit(-1)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description='Batch test move files')
    parser.add_argument('PATH', nargs='+', help='Root paths to search for move files')
    parser.add_argument('--only-stats', action='store_true', help='Only print statistics and do not fail')
    return parser.parse_args()


def normalize_args(args: argparse.Namespace) -> Tuple[List[str], bool]:
    paths, stats = args.PATH, args.only_stats
    for path in paths:
        assert_valid_path(path)
    return paths, stats


def should_reject(path: str) -> bool:
    for folder in special_folder:
        if path.find(folder) == -1:
            continue
        # for file xxx.move, if xxx.exp exists, then it should be rejected
        exp = path.rstrip('move') + 'exp'
        if os.path.exists(path) and os.path.isfile(exp):
            with open(exp, 'r') as f:
                for line in f.readlines():
                    for msg in error_messages:
                        if line.find(msg) != -1:
                            return True
    return False


def visit_path(path: str) -> Tuple[int, int, int]:
    files = list_move_codes(path)
    total_files = len(files)
    files = list(filter(retain_file, files))

    results = map(visit_file, files)
    failed = 0
    for result in results:
        # Failed cases
        if result.passed == should_reject(result.path):
            if result.passed:
                print(f'[FAIL] should reject {result.path}')
            else:
                print(f'[FAIL] {result.last_output}')
            failed += 1
    return total_files, len(files), failed


def print_stats(total_files: int, tested_files: int, failed: int, paths: List[str]):
    print('\n')
    print(f'Searched {len(paths)} path(s):')
    for path in paths:
        print(f'  - {path}')
    print(f'Total move files: {total_files}, filtered: {total_files - tested_files}, failed: {failed}')
    print(f'Result: {tested_files - failed} / {tested_files}')
    if tested_files != 0:
        print('Success rate: {:.2f}%'.format((tested_files - failed) / tested_files * 100))


def main():
    args = parse_args()
    paths, only_stats = normalize_args(args)

    total_files, tested_files, failed = reduce(lambda acc, path: tuple(map(add, acc, visit_path(path))), paths, (0, 0, 0))
    print_stats(total_files, tested_files, failed, paths)

    if not only_stats:
        if failed != 0:
            print('Failed!')
            exit(-1)
        print('Passed!')


if __name__ == '__main__':
    main()