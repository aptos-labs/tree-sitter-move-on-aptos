import os
import re
import sys
import subprocess
from typing import List, Optional

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
    return TestResult(file, False, last_line.decode("utf-8"))


exclude = [
    'aptos-move/writeset-transaction-generator/templates',
    'test/repos/aptos-core/aptos-move/move-examples/move-tutorial/step_3/basic_coin.move'
]
def retain_file(file: str) -> bool:
    for excl in exclude:
        if file.find(excl) != -1:
            print(f'[IGNORE] {file}')
            return False
    return True
    

def get_root() -> str:
    if len(sys.argv) != 2:
        print(f'usage: {sys.argv[0]} <root_path>')
        print('only 2 args are accepted!')
        exit(-1)
    path = sys.argv[1]
    if not os.path.exists(path):
        print(f'Path {path} does not exist!')
        exit(-1)
    if not os.path.isdir(path):
        print(f'Path `{path}` is not a directory!')
        exit(-1)
    return path


def main():
    root = get_root()
    files = list_move_codes(get_root())

    total_files = len(files)
    files = list(filter(retain_file, files))
    filtered = total_files - len(files)

    results = map(visit_file, files)
    failed = 0
    for result in results:
        if not result.passed:
            print(f'[FAIL] {result.last_output}')
            failed += 1
    print(f'Total move source files: {total_files}')
    print(f'Filtered files: {filtered}')
    print(f'{failed} case(s) failed')
    print(f'Result: {len(files) - failed} / {len(files)}')
    if failed != 0:
        print('Failed!')
        exit(-1)
    print('Passed!')

if __name__ == '__main__':
    main()