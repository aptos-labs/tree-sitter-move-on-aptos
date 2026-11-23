
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
    'aptos-framework/sources/datastructures/storage_slot_or_inline.move',

    # Intentionally invalid syntax in compiler / linter negative tests
    'move-compiler-v2/tests/checking/natives/native_enum.move',
    'move-compiler-v2/tests/checking/control_flow/for_header_spec_needs_semicolon.move',
    'move-compiler-v2/tests/checking/control_flow/for_trailing_spec_no_semicolon.move',
    'move-compiler-v2/tests/checking/control_flow/for_spec_only_invariant_ensures.move',
    'move-compiler-v2/tests/checking/control_flow/for_spec_only_invariant_assert.move',
    'move-compiler-v2/tests/checking/control_flow/for_spec_header_and_trailing.move',
    'move-compiler-v2/tests/match-checks/literal_bind_outside_match.move',
    'move-compiler-v2/tests/match-checks/match_literal_ref_pattern.move',
    'tools/move-linter/tests/default-only/unreachable_code_return_in_binop.move',
    'tools/move-linter/tests/default-only/unreachable_code_control_exp_as_term.move',

    # Grammar limitation: Move v2.4 semicolon-optional block `if` statements
    'aptos-framework/sources/multisig_account.move',
    'AptosFramework/multisig_account.move',
    'aptos-framework/sources/aggregator/optional_aggregator.move',
    'AptosFramework/aggregator/optional_aggregator.move',
    'e2e-move-tests/src/tests/paranoid_type_check_scaling.data/pack/sources/scaling.move',

    # Grammar limitation: Move v2.4 visibility modifiers on constants (public/package/friend const)
    'move-compiler-v2/tests/checking-lang-v2.4/constant_script_visibility.move',
    'move-compiler-v2/tests/checking-lang-v2.4/cross_module_const_in_initializer_rejected.move',
    'move-compiler-v2/tests/checking-lang-v2.4/friend_const_',
    'move-compiler-v2/tests/checking-lang-v2.4/package_const_',
    'move-compiler-v2/tests/checking-lang-v2.4/public_const_basic.move',
    'move-compiler-v2/tests/checking-lang-v2.4/public_const_in_script.move',
    'transactional-tests/tests/no-v1-comparison/public_const/',
    'tools/move-linter/tests/model_ast_lints/needless_visibility_const.move',
    'tools/move-linter/tests/model_ast_lints/deprecated_usage.move',
    'tools/move-linter/tests/model_ast_lints/unused/truly_unused/unused_friend_package_const.move',
    'tools/move-linter/tests/model_ast_lints/unused/truly_unused/public_const_no_warning.move',

    # Grammar limitation: Move v2.4 match range and negative-literal patterns
    'move-compiler-v2/tests/checking-lang-v2.2/primitive_match_range.move',
    'move-compiler-v2/tests/match-transforms/range_',
    'move-compiler-v2/tests/match-checks/range_',
    'transactional-tests/tests/no-v1-comparison/match_extensions/range_',
    'transactional-tests/tests/no-v1-comparison/match_extensions/all_integer_types.move',
    'move-model/exchange/tests/ast_sources/enums.move',
    'lean/verex-e2e-tests/VerexE2ETests/MoveToVerexSL/enums.move',
    'lean/v0/transpiler/Transpiler/Tests/Programs/enums.move',

    # Grammar limitation: Move v2.4/v2.5 trailing `for (...) { ... } spec { ... }` blocks
    'aptos-framework/sources/genesis.move',
    'aptos-framework/sources/stake.move',
    'datastructures/big_ordered_map.move',
    'move-prover/tests/sources/functional/for_loop_invariants.move',

    # Grammar limitation: Move v2.4/v2.5 spec extensions (`proof { ... }`, `spec lemma`, `[weight = N]`,
    # state labels `S |~ ...`, behavior predicates `*_of`, `ghost` fields, `reads`/`modifies` clauses)
    'framework/move-stdlib/sources/fixed_point32.move',
    'MoveStdlib/fixed_point32.move',
    'MoveStdlib/sources/fixed_point32.move',
    'pack_stdlib/sources/fixed_point32.move',
    'pack_stdlib_incompat/sources/fixed_point32.move',
    'aptos-stdlib/sources/data_structures/big_vector.spec.move',
    'configs/staking_config.spec.move',
    'flow/evaluation/spec-inference/corpus-v1.2/framework/sources/AptosFramework/code.spec.move',
    'flow/evaluation/spec-inference/corpus-v1.2/framework/sources/AptosTrading/orders/bulk_order_types.spec.move',
    'move-compiler-v2/tests/checking-lang-v2.2/specs/weight_version_err.move',
    'move-compiler-v2/tests/checking-lang-v2.4/specs/behavior_folds_of_err.move',
    'move-compiler-v2/tests/checking-lang-v2.4/specs/behavior_unchanged_of_err.move',
    'tools/move-linter/tests/model_ast_lints/unused/spec_lemmas.move',
    'move-model/exchange/tests/ast_sources/access_of.move',
    'move-model/exchange/tests/ast_sources/ghost_fields.move',
    'move-model/exchange/tests/ast_sources/lemmas.move',
    'lean/verex-e2e-tests/VerexE2ETests/MoveToVerexSL/state_labels.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/behavior_label_witness.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/behavior_manual_facts.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/closure_frame_widening.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/contract_proof_bindings.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/field_update_labels.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/function_wrappers.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/generic_function_literals_model.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/generic_function_literals_partial.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/point_asserts.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/projected_global_reads.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/proof_steps.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/scopes.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/state_label_scope.move',
    'lean/verex-e2e-tests/VerexE2ETests/SourceVerify/stored_write_frames.move',
    'move-prover/bytecode-pipeline/tests/spec_instrumentation/proofs/',
    'move-prover/doc/higher-order-paper-26/examples/sources/',
    'move-prover/doc/inference-paper-26/v1/examples/',
    'move-prover/doc/inference-paper-26/v2/examples/',
    'move-prover/tests/inference/calculator.',
    'move-prover/tests/inference/functional_result_memory.exp.move',
    'move-prover/tests/inference/ghost_field_update.',
    'move-prover/tests/inference/guarded_state_labels.exp.move',
    'move-prover/tests/inference/loops.exp.move',
    'move-prover/tests/inference/memory_free_after_write.exp.move',
    'move-prover/tests/inference/memory_free_declared_effects.exp.move',
    'move-prover/tests/inference/mutations.exp.move',
    'move-prover/tests/inference/opaque_create_then_update.exp.move',
    'move-prover/tests/inference/sourcifier_roundtrip.exp.move',
    'move-prover/tests/inference/state_labels.exp.move',
    'move-prover/tests/inference/trusted_boundary_callee.exp.move',
    'move-prover/tests/inference/vault.move',
    'move-prover/tests/sources/functional/proof/',
    'move-prover/tests/sources/functional/state_labels/',
    'move-prover/tests/sources/functional/ghost_field.move',
    'move-prover/tests/sources/functional/ghost_field_bp.move',
    'move-prover/tests/sources/functional/ghost_field_closure_equality.move',
    'move-prover/tests/sources/functional/ghost_field_closure_equality_ext.move',
    'move-prover/tests/sources/functional/ghost_field_equality.move',
    'move-prover/tests/sources/functional/ghost_field_err.move',
    'move-prover/tests/sources/functional/ghost_field_invariants.move',
    'move-prover/tests/sources/functional/ghost_field_map_equality.move',
    'move-prover/tests/sources/functional/ghost_field_map_equality_ext.move',
    'move-prover/tests/sources/functional/bv_internal.move',
    'move-prover/tests/sources/functional/loop_memory_havoc.move',
    'move-prover/tests/sources/functional/modifies_of_fun_call.move',
    'move-prover/tests/sources/functional/modifies_spec_fun.move',
    'move-prover/tests/sources/functional/reads_check.move',
    'move-prover/tests/sources/functional/table_closure_value.move',
    'move-prover/tests/sources/functional/transitions.move',
    'move-prover/tests/sources/functional/uninterpreted_spec_fun_congruence.move',
    'move-prover/tests/sources/functional/closures/amm_example.move',
    'move-prover/tests/sources/functional/closures/apply_carrier_unread_memory.move',
    'move-prover/tests/sources/functional/closures/behavioral_congruence.move',
    'move-prover/tests/sources/functional/closures/behavioral_results.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_captured_declared.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_captured_declared_dep_dep.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_captured_reader.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin_call_result.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin_kept.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin_overwritten.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin_packed.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_field_origin_packed_kept.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_forward_chain.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_mut_ref.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_native_target_capture.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_native_target_capture_ok.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_reached_native_frames.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_reached_native_frames_ok.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_stored_spec_mention.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_transparent.move',
    'move-prover/tests/sources/functional/closures/fun_param_frame_transparent_ok.move',
    'move-prover/tests/sources/functional/closures/inline/bp_pure_callee.move',
    'move-prover/tests/sources/functional/closures/inline/fold_symbolic.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of_callee_ensures.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of_collect.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of_idx.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of_multi.move',
    'move-prover/tests/sources/functional/closures/inline/folds_of_ref.move',
    'move-prover/tests/sources/functional/closures/inline/specialize_generic_caller.move',
    'move-prover/tests/sources/functional/closures/lambda_deferred_frame_escape.move',
    'move-prover/tests/sources/functional/closures/lambda_forwarded_param_',
    'move-prover/tests/sources/functional/closures/lambda_funparam_aux_exp_memory_err.move',
    'move-prover/tests/sources/functional/closures/lambda_funparam_declared_memory_err.move',
    'move-prover/tests/sources/functional/closures/lambda_nested_hof_memory_err.move',
    'move-prover/tests/sources/functional/closures/lambda_spec_global_memory.move',
    'move-prover/tests/sources/functional/closures/lambda_unknown_closure_declared_frame.move',
    'move-prover/tests/sources/functional/closures/result_of_old_label.move',
    'move-prover/tests/sources/functional/closures/result_of_transition_state_label_let.move',
    'move-prover/tests/sources/functional/closures/stored_fun_',
    'move-prover/tests/sources/regression/boogie_field_sel_injectivity.move',
    'move-prover/tests/sources/regression/closure_requires_guarded.move',
    'move-prover/tests/sources/regression/fun_arg_frame_check',
    'move-prover/tests/sources/regression/generic_aliasing_write_routes.move',
    'move-prover/tests/sources/regression/ghost_init_generic_cast.move',
    'move-prover/tests/sources/regression/opaque_call_fun_arg_writes.move',
    'move-prover/tests/sources/regression/quant_range_binder_scope.move',
    'move-prover/tests/sources/regression/state_domain_',
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