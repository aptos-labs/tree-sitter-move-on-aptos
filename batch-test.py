import os
import re
import sys
import subprocess
from typing import List, Optional, Tuple

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
    'aptos-move/move-examples/move-tutorial/step_3/basic_coin.move',
    'third_party/move/testing-infra/transactional-test-runner/tests/vm_test_harness',
    'third_party/move/documentation/tutorial/step_3',

    # deprecated syntax
    'third_party/move/move-prover/tests/xsources/design',

    # non-standard
    'third_party/move/tools/move-cli/tests/build_tests/circular_dependencies',

    # rejected case
    'attribute_no_closing_bracket',
    'attribute_num_sign_no_bracket',

]
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


def get_paths() -> List[str]:
    if len(sys.argv) < 2:
        print(f'usage: {sys.argv[0]} <root_path> [ .. <root_path> ]')
        print('at least one path should be provided')
        exit(-1)

    paths = sys.argv[1:]
    for path in paths:
        assert_valid_path(path)
    return paths


def should_reject(path: str) -> bool:
    return False


def visit_path(path: str) -> Tuple[int, int, int]:
    files = list_move_codes(path)
    total_files = len(files)
    files = list(filter(retain_file, files))

    results = map(visit_file, files)
    failed = 0
    for result in results:
        if result.passed == should_reject(result.path):
            if result.passed:
                print(f'[FAIL] should reject {result.path}')
            else:
                print(f'[FAIL] {result.last_output}')
            failed += 1
    return total_files, len(files), failed


def main():
    total_files, tested_files, failed = 0, 0, 0

    paths = get_paths()
    for path in paths:
        total, tested, fail = visit_path(path)
        total_files += total
        tested_files += tested
        failed += fail
    
    print('\n')
    print('Searched paths:')
    for path in paths:
        print(f'  - {path}')
    print(f'Total move source files: {total_files}')
    print(f'Filtered files: {total_files - tested_files}')
    print(f'{failed} case(s) failed')
    print(f'Result: {tested_files - failed} / {tested_files}')
    if failed != 0:
        print('Failed!')
        exit(-1)
    print('Passed!')


if __name__ == '__main__':
    main()