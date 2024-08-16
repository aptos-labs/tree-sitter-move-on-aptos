
exclude = [
    'aptos-move/writeset-transaction-generator/templates',
    'aptos-move/move-examples/move-tutorial/step_3/basic_coin.move',
    'third_party/move/testing-infra/transactional-test-runner/tests/vm_test_harness',
    'third_party/move/documentation/tutorial/step_3',

    # deprecated syntax
    'third_party/move/move-prover/tests/xsources/design',

    # non-standard
    'third_party/move/tools/move-cli/tests/build_tests/circular_dependencies',

    # contains invalid hex string
    'pancake-cake-oft/sources/cake_oft.move',
    
    # rejected case
    'attribute_no_closing_bracket',
    'attribute_num_sign_no_bracket',
    'type_variable_join_single_pack.move',

    # error cases
    'abort_negative_stack_size',
    'variants_parse_err',
    'constants_standalone_let',
    'pack_err',
    'variables_err',
]

# Under these folders, if a file xxx.move exists and xxx.exp exists, then xxx.move should be rejected
# xxx.exp is the expected error output for xxx.move. `xxx.exp` might present in other folders, but
# they do not represent a parser error.
#
# Still, there are many false positives under `move_check/parser` and `move_check/expansion`.
special_folder = [
    'aptos-core/third_party/move/move-compiler/tests/move_check/parser',
    # 'aptos-core/third_party/move/move-compiler/tests/move_check/expansion',
    'aptos-core/third_party/move/move-compiler-v2/tests/checking/positional_fields',
]


error_messages = [
    'error',
    'invalid documentation comment',
]