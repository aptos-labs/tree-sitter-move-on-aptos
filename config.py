
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

    # Move compiler v1/v2 parser & expansion error test directories
    # These contain intentionally-invalid syntax for testing the compiler's own parser
    'move-compiler-v2/tests/more-v1/parser',
    'move-compiler-v2/tests/more-v1/expansion',

    # UTF-8 error tests - intentionally invalid Unicode in identifiers/keywords
    'move-compiler-v2/tests/checking/utf8-comment',

    # Intentionally broken syntax in specific test files
    'move-compiler-v2/tests/checking/positional_fields/positional_fields_no_support',
    'move-compiler-v2/tests/checking/positional_fields/named_tuple_ability_decl_invalid',
    'move-compiler-v2/tests/checking/positional_fields/struct_postfix_ability_decl_invalid',
    'move-compiler-v2/tests/checking/positional_fields/constructor_invalid0',
    'move-compiler-v2/tests/checking/control_flow/loop_labels_parse_err',
    'move-compiler-v2/tests/checking/dotdot/assign0.move',
    'move-compiler-v2/tests/checking/dotdot/assign1.move',
    'move-compiler-v2/tests/checking/dotdot/field_update_not_supported.move',
    'move-compiler-v2/tests/checking/variants/variants_test_parse_err1.move',
    'move-compiler-v2/tests/checking/visibility-checker/direct_visibility_err3.move',
    'move-compiler-v2/tests/checking-lang-v2.4/illegal_dollar_ident.move',
    'move-compiler-v2/tests/op-equal/invalid4.move',

    # Intentionally broken string/hex literals and native/public const
    'move-compiler-v2/tests/folding/constants_quoted_string.move',
    'move-compiler-v2/tests/folding/constants_single_quote.move',
    'move-compiler-v2/tests/folding/constants_hexstring.move',
    'move-compiler-v2/tests/folding/constant_native.move',
    'move-compiler-v2/tests/folding/constant_public.move',

    # Empty type arguments <> - intentionally invalid
    'move-compiler-v2/tests/folding/bad_type_argument_arity_const.move',
    'move-compiler-v2/tests/checking/typing/bad_type_argument_arity_const.move',
    'move-compiler-v2/tests/checking/typing/bad_type_argument_arity_fun.move',
    'move-compiler-v2/tests/checking/typing/bad_type_argument_arity_struct.move',
    'move-compiler-v2/tests/checking/typing/bad_type_argument_arity_struct_pack.move',
    'move-compiler-v2/tests/checking/typing/bad_type_argument_arity_struct_unpack.move',
    'move-compiler-v2/tests/checking/typing/v1-naming/vector_literal_type_arity.move',
    'move-compiler-v2/tests/checking/naming/global_builtin_zero_type_arguments.move',
    'move-compiler-v2/tests/checking/naming/other_builtins_invalid.move',

    # Grammar limitation: turbofish on method calls (s.method::<T>(args))
    'move-compiler-v2/tests/checking/receiver/call_errors.move',
    'move-compiler-v2/tests/checking/receiver/generic_calls.move',
    'move-compiler-v2/tests/checking/receiver/generic_calls_typed.move',
    'move-compiler-v2/tests/checking-lang-v2.2/receiver/generic_receiver_calls.move',

    # Grammar limitation: lambda/function types (|T|T)
    'move-compiler-v2/tests/checking-lang-v2.2/lambda/chained_calls.move',
    'move-compiler-v2/tests/checking-lang-v2.2/lambda/vector_assignability.move',
    'move-compiler-v2/tests/checking-lang-v2.2/lambda/bug_16195_variant1.move',
    'move-compiler-v2/tests/checking-lang-v2.2/lambda/inner_fun_variance.move',

    # Grammar limitation: behavior predicate specs (requires_of, aborts_of, etc.)
    'move-compiler-v2/tests/checking-lang-v2.4/specs/behavior_predicates',

    # Grammar limitation: cast with generic type (0 as Cup<u8>)
    'move-compiler-v2/tests/checking/typing/cast_invalid.move',

    # Grammar limitation: decimal addresses (address 123 {})
    'move-compiler-v2/tests/checking/typing/hex_and_decimal_address.move',

    # Grammar limitation: acquires with generic types
    'move-compiler-v2/tests/checking-lang-v1/acquires_list_generic.move',

    # Grammar limitation: access specifier with glob S(*)
    'move-compiler-v2/tests/checking-lang-v1/access_ok.move',
    'move-compiler-v2/tests/checking-lang-v2.2/access_specifiers/access_not_supported.move',
    'move-compiler-v2/tests/checking-lang-v1/expansion/access_specifier_not_supported.move',

    # Spec pragma qualified identifiers
    'move-compiler-v2/tests/checking/specs/pragmas_err.move',
    'move-compiler-v2/tests/checking/specs/intrinsic_decl_err.move',
    'move-model/tests/sources/pragmas_err.move',
    'move-model/tests/sources/intrinsic_decl_err.move',

    # Decompiler/linter test files with parser issues
    'tools/move-decompiler/tests/control-flow-recovery/noexit_loops.move',
    'tools/move-decompiler/tests/move-v2-features/enum.move',
    'tools/move-linter/tests/model_ast_lints/bad_lint_attribute_10.move',
    'move-prover/tests/sources/functional/friend_error.move',

    # Large legacy file with parser issues
    'move-examples/diem-framework/move-packages/DPN/sources/DiemAccount.move',

    # 'for' as function name: tree-sitter keyword conflict (for_expression vs call_expression)
    'transactional-tests/tests/control_flow/for_user.move',
    'transactional-tests/tests/control_flow/for_loop_counter.move',

    # Known parser limitation: dot_expression method call ()  not consumed in match arms
    # This is a pre-existing tree-sitter LR limitation with optional(arg_list) in dot_expression
    'transactional-tests/tests/no-v1-comparison/enum/enum_matching.move',
    'transactional-tests/tests/no-v1-comparison/enum/enum_scoping.move',
    'transactional-tests/tests/no-v1-comparison/enum/bug_14733_match_critical_edge.move',
    'transactional-tests/tests/no-v1-comparison/enum/match_cover.move',
    'tests/bytecode-generator/matching_ok.move',
    'tests/bytecode-generator/matching_coverage_err.move',
    'tests/bytecode-generator/matching_ability_err.move',
    'tests/file-format-generator/struct_variants.move',
]

# Under these folders, if a file xxx.move exists and xxx.exp exists, then xxx.move should be rejected
# xxx.exp is the expected error output for xxx.move. `xxx.exp` might present in other folders, but
# they do not represent a parser error.
#
# Directories containing intentionally-invalid compiler test files. The should_reject() function
# checks for .exp files containing "error" messages to identify expected-failure tests.
special_folder = [
    # Move compiler v1 parser test directory (intentionally invalid syntax)
    'move-compiler/tests/move_check/parser',
    # 'move-compiler/tests/move_check/expansion',
]


error_messages = [
    'error',
    'invalid documentation comment',
]