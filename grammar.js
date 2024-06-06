// Helper functions

// source: https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
const sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const sepBy = (sep, rule) => optional(sepBy1(sep, rule));
const sepByComma = (rule) => seq(sepBy(',', rule), optional(','));

// A helper function to match an identifier within an access chain.
// When `wildcard = true`, the identifier can be `*`.
const chain_ident = ($, wildcard) => field('path', (wildcard) ? choice('*', $.identifier) : $.identifier);

// LeadingNameAccess = <NumericalAddress> | <Identifier>
// `Identifier` can be `*` if `wildcard = true`
const leading_name_access = ($, wildcard) => alias(
    choice(chain_ident($, wildcard), $.numerical_addr),
    $.leading_name_access
);

// New rule:
// NameAccessChain = <Identifier>
//                 | <LeadingNameAccess> "::" <Identifier> ( "::" <Identifier> )?
// `Identifier` can be `*` if `wildcard = true`
const name_access_chain = ($, wildcard) => {
    const ident = () => chain_ident($, wildcard);
    return alias(
        choice(
            field('name', choice(ident(), $.discouraged_name)),
            seq(
                choice(leading_name_access($, wildcard), $.discouraged_name),
                field('access_two', seq('::', ident())),
                optional(field('access_three', seq('::', ident())))
            ),
        ),
        $.name_access_chain,
    );
};

// Escaped sequence like \n, \", \xab, ...
// It is used within a `token` rule, so it cannot be a rule identifier. Tree-sitter forbids
// usage of `$.xxx` with in `token(..)` and `token.immediate(..)`.
const escaped_sequence = token.immediate(seq(
    '\\',
    choice(
        /x[0-9a-fA-F]{2}/,
        /[ntr0\\"]/,
    ),
));

// Binary operators with precedence levels.
const binary_operators = [
    [],                                                                     // 1 (no binary operators have precedence 1)
    [['==>', 'equal_equal_greater'], ['<==>', 'less_equal_equal_greater']], // 2
    [['||', 'pipe_pipe']],                                                  // 3
    [['&&', 'amp_amp']],                                                    // 4
    [['==', 'equal_equal'], ['!=', 'exclaim_equal'], ['<', 'less'],
    ['>', 'greater'], ['<=', 'less_equal'], ['>=', 'greater_equal']],       // 5
    [['..', 'period_period']],  // 6
    [['|', 'pipe']],            // 7
    [['^', 'caret']],           // 8
    [['&', 'amp']],             // 9
    [['<<', 'less_less'], ['>>', 'greater_greater']],   // 10
    [['+', 'plus'], ['-', 'minus']],                    // 11
    [['*', 'star'], ['/', 'slash'], ['%', 'percent']],  // 12
];

const expr_precedence = {
    DEFAULT: 1,
    LAST: -1,

    // immediately after the multiplicative operators (*, /, %)
    UNARY: 13,
    FIELD: 14,
    CALL: 15,
    RANGE: 16,
};

// The tree-sitter grammar. Mostly based on the `move-compiler`'s grammar.
// `export default` is not used as `tree-sitter` only accepts CommonJS module.
module.exports = grammar({
    name: 'move_on_aptos',

    word: $ => $.identifier,

    conflicts: $ => [
        [$.primitive_type, $.term],
        [$._name_expr],
        [$._reuseable_keywords, $.for_loop_expr],
        [$.discouraged_name, $._type],
    ],

    extras: $ => [
        // `$.comments` will crash tree-sitter somehow.
        $.line_comment,
        $.block_comment,
        /\s/,
    ],

    externals: $ => [
        $._block_doc_comment_marker,
        $._block_comment_content,
        $._doc_line_comment,

        // If a syntax error is encountered during regular parsing, Tree-sitter’s first action during error
        // recovery will be to call the external scanner’s scan function with all tokens marked valid.
        $._error_sentinel,
    ],

    rules: {
        // Parse a file:
        //  File = (<Attributes> (<AddressBlock> | <Module> | <Script>))*
        source_file: $ => repeat(seq(
            // Attributes = ("#" "[" Comma<Attribute> "]")*
            optional($.attributes),
            // <AddressBlock> | <Module> | <Script>
            choice($.module, $.script, $.address_block)
        )),

        number_type: _ => choice(
            'u8', 'u16', 'u32', 'u64', 'u128', 'u256'
        ),

        /// `signer` is quite special: it is used externally as a type, but internally can be a variable.
        signer: _ => 'signer',
        _quantifier_directive: _ => choice('exists', 'forall', 'choose', 'min'),

        primitive_type: $ => choice(
            $.number_type,
            'bool', 'address', 'vector',
        ),

        identifier: _ => /[a-zA-Z_]\w*/,
        var_name: $ => choice($.identifier, $.discouraged_name),
        // FIXME: this is a workaround for the existing chaotic naming scheme. Keywords are not supposed to be identifiers.
        discouraged_name: $ => choice($.primitive_type, $._quantifier_directive, $._reuseable_keywords),
        _reuseable_keywords: _ => choice(
            'for', 'while', 'friend',
        ),

        number: _ => choice(
            /\d[\d_]*/,
            /0[xX][\da-fA-F][\da-fA-F_]*/,
            /0b[01][01_]*/,
            /0o[0-7][0-7_]*/,
        ),
        numerical_addr: $ => $.number,
        bool_literal: $ => choice('true', 'false'),
        typed_number: $ => alias(seq($.number, $.number_type), 'typed_number'),
        byte_string: _ => choice(
            /x\"[\da-fA-F]*\"/,
            token(seq('b"', repeat(choice(escaped_sequence, /[^\\"]/)), '"')),
        ),

        // Parse a Type:
        //         Type = _Type | _RefType
        //
        //         _Type =
        //          <NameAccessChain> <TypeArgs>?
        //          | "|" Comma<Type> "|" <Type>?
        //          | "(" Comma<Type> ")"
        //
        //         _RefType =  "&" <_Type> | "&mut" <_Type>
        // **Double** ('&&') reference is not allowed.
        type: $ => choice($._type, $._ref_type),
        _type: $ => choice(
            seq(choice(name_access_chain($, false), $.primitive_type), optional($.type_args)),
            // `||' is treated as an empty param type list in this context.
            // TODO: verify the associativity
            prec.right(expr_precedence.DEFAULT,
                field('closure_type', seq('|', field('param_types', sepByComma($.type)), '|', field('return_type', optional($.type))))
            ),
            field('tuple', seq('(', sepByComma($.type), ')')),
        ),
        _ref_type: $ => choice(
            field('ref', seq('&', $._type)),
            field('mut_ref', seq('&mut', $._type)),
        ),


        // Parse an expression:
        //      Exp =
        //            <LambdaBindList> <Exp>
        //          | <Quantifier>                  spec only
        //          | <BinOpExp>
        //          | <UnaryExp> "=" <Exp>
        _expr: $ => choice(
            $.assignment,
            $._op_expr,
            $.quantifier,
            field('lambda', seq($.lambda_bind_list, $._expr)),
        ),
        assignment: $ => prec.left(expr_precedence.DEFAULT, seq(alias($._unary_expr, $.target), '=', alias($._expr, $.value))),

        // Parse a list of bindings for lambda.
        //      LambdaBindList = "|" Comma<Bind> "|"
        lambda_bind_list: $ => seq('|', sepByComma($._bind), '|'),

        // Parses a quantifier expressions
        //
        //   <Quantifier> =
        //       ( "forall" | "exists" ) <QuantifierBindings>? <Triggers>? ("where" <Exp>)? ":" Exp
        //     | ( "choose" [ "min" ] ) <QuantifierBind> "where" <Exp>
        //   <QuantifierBindings>   = <QuantifierBind> ("," <QuantifierBind>)*
        //   <QuantifierBind>       = <Identifier> ":" <Type> | <Identifier> "in" <Exp>
        //   <Triggers>             = ("{" Comma<Exp> "}")+
        quantifier: $ => choice(
            seq(
                field('scope', choice('forall', 'exists')),
                sepBy1(',', $.quantifier_bind),
                optional($.triggers),
                optional(seq('where', $._expr)),
                ':',
                field('assertion', $._expr),
            ),
            seq(
                'choose',
                optional('min'),
                $.quantifier_bind,
                'where',
                field('condition', $._expr),
            )
        ),
        quantifier_bind: $ => choice(
            field('type_bind', seq(field('var', $.identifier), ':', $.type)),
            field('value_bind', seq(field('var', $.identifier), 'in', field('scope', $._expr))),
        ),
        triggers: $ => repeat1(seq('{', sepByComma(field('trigger', $._expr)), '}')),

        // Old grammar:
        //      BinOpExp = <BinOpExp> <BinOp> <BinOpExp>
        //               | <UnaryExp>
        // Now grammar:
        //      OpExp = <BinOpExp>      # anonymous
        //            | <UnaryExp>
        //      BinOpExp = <OpExp> <BinOp> <OpExp>
        _op_expr: $ => choice(
            $._unary_expr,
            $.bin_op_expr,
        ),
        bin_op_expr: $ => choice(
            // binary operators
            ...binary_operators.flatMap(
                (level, index) => level.map(([symbol, name]) => prec.left(
                    index + 2,
                    seq(
                        field('lhs', $._op_expr),
                        alias(symbol, $.binary_operator),
                        field('rhs', $._op_expr)
                    )
                ))
            ),
        ),

        // Parse a unary expression:
        //      UnaryExp =
        //          "!" <UnaryExp>
        //          | "&mut" <UnaryExp>
        //          | "&" <UnaryExp>
        //          | "*" <UnaryExp>
        //          | "move" <Var>
        //          | "copy" <Var>
        //          | <DotOrIndexChain>
        _unary_expr: $ => choice(
            $.not_expr,
            $.ref_expr,
            $.ref_mut_expr,
            $.deref_expr,
            $.move_expr,
            $.copy_expr,

            $._dot_or_index_chain,
        ),
        not_expr: $ => prec(expr_precedence.UNARY, seq('!', $._unary_expr)),
        ref_expr: $ => prec(expr_precedence.UNARY, seq('&', $._unary_expr)),
        ref_mut_expr: $ => prec(expr_precedence.UNARY, seq('&mut', $._unary_expr)),
        deref_expr: $ => prec(expr_precedence.UNARY, seq('*', $._unary_expr)),
        move_expr: $ => prec(expr_precedence.UNARY, seq('move', field('variable', $.identifier))),
        copy_expr: $ => prec(expr_precedence.UNARY, seq('copy', field('variable', $.identifier))),

        // Parse an expression term optionally followed by a chain of dot or index accesses:
        //      DotOrIndexChain =
        //          <DotOrIndexChain> "." <Identifier> [ ["::" <TypeArgs>]  <CallArgs> ]
        //          | <DotOrIndexChain> "[" <Exp> "]"                      spec only
        //          | <Term>
        _dot_or_index_chain: $ => choice(
            $.access_field,
            $.receiver_call,
            $.mem_access,
            alias($.term, 'expr'),
        ),
        receiver_call: $ => prec.left(expr_precedence.CALL, seq(
            field('receiver', $._dot_or_index_chain), '.', field('func', $.identifier),
            optional(field('type_generics', seq('::', $.type_args))),
            $.call_args,
        )),
        mem_access: $ => prec.left(expr_precedence.CALL, seq($._dot_or_index_chain, '[', field('index', $._expr), ']')),
        access_field: $ => prec.left(expr_precedence.FIELD, seq(
            field('element', $._dot_or_index_chain), '.', field('field', $.identifier),
        )),

        // Parse an expression term:
        //      Term =
        //          "break"
        //          | "continue"
        //          | "vector" ('<' Comma<Type> ">")? "[" Comma<Exp> "]"
        //          | <Value>
        //          | "(" Comma<Exp> ")"
        //          | "(" <Exp> ":" <Type> ")"
        //          | "(" <Exp> "as" <Type> ")"
        //          | <Sequence>
        //          | "if" "(" <Exp> ")" <Exp> "else" "{" <Exp> "}"
        //          | "if" "(" <Exp> ")" "{" <Exp> "}"
        //          | "if" "(" <Exp> ")" <Exp> ("else" <Exp>)?
        //          | "while" "(" <Exp> ")" "{" <Exp> "}"
        //          | "while" "(" <Exp> ")" <Exp> <SpecLoopInvariant>?
        //          | "loop" <Exp>
        //          | "loop" "{" <Exp> "}"
        //          | "return" "{" <Exp> "}"
        //          | "return" <Exp>?
        //          | "abort" "{" <Exp> "}"
        //          | "abort" <Exp>
        //          | "for" "(" <Exp> "in" <Exp> ".." <Exp> <SpecLoopInvariant>? ")" "{" <Exp> "}"
        //          | <SpecBlock>
        //          | <NameExp>
        //
        // The conflict resolution is based on `tree-sitter-javascript`'s approach.
        // TODO: make sure this behaves the same as the `move-compiler`.
        //       `lambda_bind_list` might also be involved.
        term: $ => choice(
            field('break_expr', 'break'),
            field('continue_expr', 'continue'),
            field('vector_access', seq('vector', optional($.type_args), '[', sepByComma($._expr), ']')),
            $.value,
            field('tuple_expr', seq('(', sepByComma($._expr), ')')),
            seq('(', field('expr', $._expr), ':', $.type, ')'),
            field('cast_expr', seq('(', $._expr, 'as', $.type, ')')),

            $.block,
            $._name_expr,

            field('spec_block', $.spec_block),

            // control flow expressions
            $.if_expr,
            $.while_expr,
            $.loop_expr,
            $.return_expr,
            $.abort_expr,
            $.for_loop_expr,
        ),
        parenthesized_expr: $ => seq('(', $._expr, ')'),
        block: $ => alias($._sequence, $.block),
        // Control flow expressions:
        if_expr: $ => prec.right(seq(
            'if',
            field('condition', $.parenthesized_expr),
            field('then', $._control_body),
            optional(field('else', seq('else', $._control_body)))
        )),
        while_expr: $ => prec.left(expr_precedence.DEFAULT, seq(
            'while',
            field('condition', $.parenthesized_expr),
            field('body', $._control_body),
            optional($.spec_loop_invariant),
        )),
        loop_expr: $ => seq('loop', field('body', $._control_body)),
        return_expr: $ => choice(
            prec(expr_precedence.LAST, 'return'),
            prec.left(seq('return', field('value', $._expr))),
        ),
        abort_expr: $ => seq('abort', field('condition', $._expr)),
        for_loop_expr: $ => seq(
            'for', '(',
            field('var', $.var_name), 'in', field('begin', $._unary_expr), '..', field('end', $._unary_expr),
            optional($.spec_loop_invariant),
            ')',
            field('body', $.block),
        ),

        // The body of `if`, `while`, `loop` expressions.
        //      ControlBody = <Sequence> | <Exp>
        // This rule is useful to conform the cases like this:
        //      if (a) {x}.f else c => (if (a) {x}).f else c => gives a parse error
        //
        // `{ }` is treated immediately after `if` or `while` or `loop` keyword.
        // So `if (a) {x}.f` is parsed as `(if (a) {x}).f` instead of `if (a) ({x}.f)`.
        _control_body: $ => choice(
            prec(expr_precedence.DEFAULT, $._sequence),
            $._expr,
        ),

        // Parse a pack, call, or other reference to a name:
        //      NameExp =
        //          <NameAccessChain> <OptionalTypeArgs> "{" Comma<ExpField> "}"
        //          | <NameAccessChain> <OptionalTypeArgs> <CallArgs>
        //          | <NameAccessChain> "!" <CallArgs>
        //          | <NameAccessChain> <OptionalTypeArgs>
        _name_expr: $ => choice(
            seq(field('name', name_access_chain($, false)), optional(field('generics', $.type_args))),
            field('call', seq(
                field('func_name', name_access_chain($, false)), optional(field('generics', $.type_args)), $.call_args
            )),
            field('pack', seq(
                field('struct_name', name_access_chain($, false)), optional(field('generics', $.type_args)),
                '{', sepByComma($.expr_field), '}'
            )),
            // macro call
            field('macro_call', seq(
                field('macro_name', name_access_chain($, false)), '!', $.call_args
            )),
        ),

        call_args: $ => seq('(', sepByComma($._expr), ')'),
        type_args: $ => seq('<', sepByComma($.type), '>'),

        // Parse a field name optionally followed by a colon and an expression argument:
        //      ExpField = <Field> <":" <Exp>>?
        expr_field: $ => seq(
            field('field', $.identifier),
            optional(field('value', seq(':', $._expr)))
        ),


        // Parse a value:
        //      Value =
        //          "@" <LeadingNameAccess>
        //          | "true"
        //          | "false"
        //          | <Number>
        //          | <NumberTyped>
        //          | <ByteString>
        value: $ => prec(expr_precedence.DEFAULT, choice(
            seq('@', leading_name_access($, false)),
            $.bool_literal,
            $.number,
            $.typed_number,
            $.byte_string,
        )),

        // Attributes = ("#" "[" Comma<Attribute> "]")*
        // However, tree sitter does not allow empty matching. Thus, `attributes` only
        // accepts non-empty attribute list.
        attributes: $ => repeat1(seq('#', '[', sepByComma($.attribute), ']')),

        // Parse a single attribute
        //      Attribute =
        //          <AttributeName>
        //          | <AttributeName> "=" <AttributeValue>
        //          | <AttributeName> "(" Comma<Attribute> ")"
        //      AttributeName = <Identifier> ( "::" Identifier )* // merged into one identifier
        attribute: $ => choice(
            field('attribute', $._attribute_name),
            seq(field('attribute', $._attribute_name), '=', field('value', $._attribute_val)),
            seq(field('attribute', $._attribute_name), '(', sepByComma($.attribute), ')')
        ),
        _attribute_name: $ => sepBy1('::', field('attr_path', $.identifier)),

        // Parse an attribute value. Either a value literal or a module access
        //  AttributeValue = <Value> | <NameAccessChain>
        _attribute_val: $ => choice(prec(expr_precedence.DEFAULT, $.value), name_access_chain($, false)),

        // AddressBlock = "address" <LeadingNameAccess> "{" (<Attributes> <Module>)* "}"
        address_block: $ => seq(
            'address',
            leading_name_access($, false),
            '{', repeat(seq(optional($.attributes), $.module)), '}'
        ),

        // Parse a module:
        //   Module =
        //       <DocComments> ( "spec" | "module") (<LeadingNameAccess>::)?<ModuleName> "{"
        //           ( <Attributes>
        //               ( <UseDecl> | <FriendDecl> |
        //                 <SpecFunction> | <SpecBlock> | <Invariant>
        //                 <DocComments> <ModuleMemberModifiers>
        //                     (<ConstantDecl> | <StructDecl> | <FunctionDecl>) )
        //               )
        //           )*
        //       "}"
        //   ModuleMemberModifiers = <ModuleMemberModifier>*
        _module_ident: $ => 'module',
        module: $ => seq(
            // TODO(doc): doc comments are not supported by now.
            choice('spec', $._module_ident),
            // (<LeadingNameAccess>::)?<ModuleName>
            seq(
                optional(seq(field('path', leading_name_access($, false)), '::')),
                field('module_name', $.identifier)
            ),
            '{', repeat($.declaration), '}'
        ),

        declaration: $ => seq(
            optional($.attributes),
            choice(
                $.use_decl,
                $.friend_decl,

                prec(expr_precedence.DEFAULT, seq('spec', $.spec_func)),
                $.spec_block,
                $.spec_invariant,

                // TODO(doc): doc comments
                seq(repeat($.module_member_modifier), choice($.constant_decl, $.struct_decl, $.function_decl)),
            )
        ),

        // Parse an optional specification block:
        //     SpecBlockTarget =
        //          <Identifier> <SpecTargetSignatureOpt>?
        //        |  "fun" <Identifier>  # deprecated
        //        | "struct <Identifier> # deprecated
        //        | "module"
        //        | "schema" <Identifier> <TypeParameters>?
        //     SpecBlock =
        //        <DocComments> "spec" ( <SpecFunction> | <SpecBlockTarget>? "{" <UseDecl>* <SpecBlockMember>* "}" )
        //     TypeParameters = '<' Comma<TypeParameter> ">"
        spec_block: $ => seq(
            // TODO(doc): doc comments, might reconsider doc matching
            'spec',
            choice(
                $.spec_func,
                seq(optional($.spec_block_target),
                    field('body', seq(
                        '{',
                        repeat($.use_decl),
                        field('member', repeat($._spec_block_member)),
                        '}'
                    )),
                )
            ),
        ),
        spec_block_target: $ => choice(
            field('signature', seq(
                field('func_name', $.identifier),
                optional($._spec_target_signature_opt),
            )),
            'module',
            field('schema', seq(
                'schema',
                field('schema_name', $.identifier),
                optional($.type_params)
            ))
        ),

        // SpecTargetSignatureOpt = (TypeParameters)? "(" Comma<Parameter> ")" (":" <Type>)?
        _spec_target_signature_opt: $ => seq(
            optional($.type_params),
            field('parameters', seq('(', sepByComma($.parameter), ')')),
            field('return_type', optional(seq(':', $.type))),
        ),

        // FIXME: It's too complicated to enforce invariant-only rule.
        spec_loop_invariant: $ => alias($.spec_block, $.spec_loop_invariant),

        // Parse a spec block member:
        //    SpecBlockMember = <DocComments> ( <Invariant> | <Condition> | <SpecFunction> | <SpecVariable>
        //                                   | <SpecInclude> | <SpecApply> | <SpecPragma> | <SpecLet>
        //                                   | <SpecUpdate> | <SpecAxiom> )
        // TODO(doc): doc comments
        _spec_block_member: $ => choice(
            $.spec_invariant,
            $.spec_condition,
            $.spec_func,
            $.spec_variable,
            $.spec_include,
            $.spec_apply,
            $.spec_pragma,
            $.spec_let,
            $.spec_update,
            $.spec_axiom,
        ),

        // Invariant = "invariant" <OptionalTypeParameters> [ "update" ] <ConditionProperties> <Exp> ";"
        spec_invariant: $ => seq(
            'invariant',
            field('type_params', optional($.type_params)),
            field('update', optional('update')),
            optional($.condition_props),
            $._expr,
            ';'
        ),

        // Parse a specification condition:
        //    SpecCondition =
        //        ("assert" | "assume" | "ensures" | "requires" ) <ConditionProperties> <Exp> ";"
        //      | "aborts_if" <ConditionProperties> <Exp> ["with" <Exp>] ";"
        //      | ("aborts_with" | "modifies") <ConditionProperties> <Exp> [Comma <Exp>]* ";"
        //      | "decreases" <ConditionProperties> <Exp> ";"
        //      | "emits" <ConditionProperties> <Exp> "to" <Exp> [If <Exp>] ";"
        //    ConditionProperties = ( "[" Comma<SpecPragmaProperty> "]" )?
        spec_condition: $ => choice(
            seq(
                field('kind', choice('assert', 'assume', 'ensures', 'requires')),
                optional($.condition_props),
                $._expr,
                ';'
            ),

            // aborts_if
            seq(
                field('kind', 'aborts_if'),
                optional($.condition_props), $._expr,
                field('with', optional(seq('with', $._expr))),
                ';'
            ),

            // ("aborts_with" | "modifies")
            seq(
                field('kind', choice('aborts_with', 'modifies')),
                optional($.condition_props), sepBy1(',', $._expr),
                ';'
            ),

            // emits
            seq(
                field('kind', 'emits'),
                optional($.condition_props), field('emission', $._expr),
                'to', field('target', $._expr),
                optional(field('condition', seq('if', $._expr))),
                ';'
            ),
        ),
        condition_props: $ => seq('[', field('property', sepByComma($._spec_pragma_prop)), ']'),


        // Parse a specification variable.
        //     SpecVariable = ( "global" | "local" )?
        //                    <Identifier> <OptionalTypeParameters>
        //                    ":" <Type>
        //                    [ "=" Exp ]  // global only
        //                    ";"
        spec_variable: $ => seq(
            optional(field('scope', choice('global', 'local'))),
            field('variable', $.identifier),
            optional($.type_params),
            ':', $.type,
            optional(field('value', seq('=', $._expr))),
            ';'
        ),

        // Parse a specification function.
        //     SpecFunction = "fun" <SpecFunctionSignature> ( "{" <Sequence> "}" | ";" )
        //                  | "native" "fun" <SpecFunctionSignature> ";"
        //     SpecFunctionSignature =
        //         <Identifier> <OptionalTypeParameters> "(" Comma<Parameter> ")" ":" <Type>
        spec_func: $ => choice(
            seq('fun',
                field('signature', $._spec_func_signatures),
                choice(field('body', $._sequence), ';')
            ),
            seq('native', 'fun', field('signature', $._spec_func_signatures), ';'),
        ),
        _spec_func_signatures: $ => seq(
            field('spec_func_name', $.identifier),
            optional($.type_params),
            field('params', seq('(', sepByComma($.parameter), ')')),
            ':', field('return_type', $.type),
        ),

        // Parse a specification schema include.
        //    SpecInclude = "include" <Exp> ";"
        spec_include: $ => seq('include', optional($.condition_props), $._expr, ';'),

        // Parse a specification schema apply.
        //    SpecApply = "apply" <Exp> "to" Comma<SpecApplyPattern>
        //                                   ( "except" Comma<SpecApplyPattern> )? ";"
        spec_apply: $ => seq(
            'apply', $._expr, 'to',
            field('targets', sepByComma($._spec_apply_pattern)),
            optional(field('exclusions', seq('except', sepByComma($._spec_apply_pattern)))),
            ';',
        ),
        // Parse a function pattern:
        //     SpecApplyPattern = <Visibility>? <SpecApplyFragment>+ <OptionalTypeArgs>
        //     SpecApplyFragment = <Identifier> | "*"
        _spec_apply_pattern: $ => seq(
            optional(field('visibility', choice('public', 'internal'))),
            // TODO: weird pattern: name fragments followed by each other without space
            field('name_pattern', repeat1($._spec_apply_fragment)),
            field('type_params', optional($.type_params)),
        ),
        _spec_apply_fragment: $ => choice(
            '*', $.identifier
        ),

        // Parse a specification pragma:
        //    SpecPragma = "pragma" Comma<SpecPragmaProperty> ";"
        //    SpecPragmaProperty = (<Identifier> | 'friend') ( "=" <Value> | <NameAccessChain> )?
        // NOTICE: `friend` here is not a keyword but an identifier name.
        spec_pragma: $ => seq(
            'pragma',
            field('properties', sepByComma($._spec_pragma_prop)),
            ';'
        ),
        _spec_pragma_prop: $ => seq(
            field('prop_name', $.var_name),
            optional(field('value', seq('=', choice($.value, name_access_chain($, false))))),
        ),

        // Parse a specification let.
        //     SpecLet =  "let" [ "post" ] <Identifier> "=" <Exp> ";"
        spec_let: $ => seq(
            'let',
            field('post_state', optional('post')),
            field('variable', $.var_name),
            '=',
            field('value', $._expr),
            ';',
        ),

        // Parse a specification update.
        //     SpecUpdate = "update" <UnaryExp> "=" <Exp> ";"
        spec_update: $ => seq('update', $.assignment, ';'),

        // Parse an axiom:
        //     a = "axiom" <OptionalTypeParameters> <ConditionProperties> <Exp> ";"
        spec_axiom: $ => seq(
            'axiom',
            field('kind', optional($.type_params)),
            optional($.condition_props),
            $._expr,
            ';'
        ),

        // Visibility = "public" ( "(" "script" | "friend" ")" )?
        visibility: $ => seq('public', optional(seq('(', choice('script', 'friend'), ')'))),

        // ModuleMemberModifier = <Visibility> | "native"
        module_member_modifier: $ => choice($.visibility, 'native', 'entry'),

        // ModuleIdent = <LeadingNameAccess>(wildcard = false) "::" <ModuleName>
        module_ident: $ => seq(leading_name_access($, false), '::', field('module_name', $.identifier)),

        // Parse a use declaration:
        //      UseDecl =
        //          "use" <ModuleIdent> <UseAlias> ";" |
        //          "use" <ModuleIdent> :: <UseMember> ";" |
        //          "use" <ModuleIdent> :: "{" Comma<UseMember> "}" ";"
        use_decl: $ => seq(
            'use',
            field('path', $.module_ident),
            choice(
                optional($._use_alias),
                seq('::', $._use_member),
                seq('::', seq('{', sepByComma($._use_member), '}')),
            ), ';'),

        // UseAlias = ("as" <Identifier>)?
        _use_alias: $ => seq('as', field('alias_name', $.identifier)),

        // UseMember = <Identifier> <UseAlias>
        _use_member: $ => field('member', seq($.identifier, optional($._use_alias))),

        // FriendDecl = "friend" <NameAccessChain>(wildcard: false) ";"
        friend_decl: $ => seq('friend', name_access_chain($, false), ';'),

        // ConstantDecl = "const" <Identifier> ":" <Type> "=" <Exp> ";"
        constant_decl: $ => seq('const', field('name', $.identifier), ':', field('type', $.type), '=', field('value', $._expr), ';'),

        // Parse a function declaration:
        //      FunctionDecl =
        //          [ "inline" ] "fun" <FunctionDefName>
        //          <OptionalTypeParameters>
        //          "(" Comma<Parameter> ")"
        //          (":" <Type>)?
        //          "pure" | ( ( "!" )? ("acquires" | "reads" | "writes" ) <AccessSpecifierList> )*
        //          ("{" <Sequence> "}" | ";")
        //      OptionalTypeParameters = '<' Comma<TypeParameter> ">" | <empty>
        //      Sequence = <UseDecl>* (<SequenceItem> ";")* <Exp>?
        function_decl: $ => seq(
            optional('inline'),
            'fun', field('function_name', $.identifier),
            optional($.type_params),
            '(', field('parameters', sepByComma($.parameter)), ')',
            optional(seq(':', $.type)),
            choice(
                field('pure', 'pure'),
                repeat(seq(
                    optional(field('negated', '!')), choice('acquires', 'reads', 'writes'),
                    $.access_specifier_list
                ))
            ),
            // Sequence
            choice(field('body', $._sequence), ';'),
        ),
        _sequence: $ => seq('{', seq(
            repeat($.use_decl),
            repeat($._sequence_item),
            optional($._expr),
        ), '}'),

        // Parse a type parameter:
        //  TypeParameter   = <Identifier> <Constraint>?
        //  TypeParameters = '<' Comma<TypeParameter> ">"
        //  Constraint      = ":" <Ability> (+ <Ability>)*
        type_param: $ => seq(field('type', $.identifier), optional($.constraints)),
        type_params: $ => seq('<', sepByComma($.type_param), '>'),
        constraints: $ => seq(':', sepBy1('+', $._ability)),

        // Parameter = <Var> ":" <Type>
        parameter: $ => seq(field('variable', $.identifier), ':', $.type),

        // AccessSpecifierList  = <AccessSpecifier> ( "," <AccessSpecifier> )* ","?
        // AccessSpecifier      = <NameAccessChainWithWildcard> <TypeArgs>? <AddressSpecifier>
        // AddressSpecifier     = <empty> | "(" <AddressSpecifierArg> ")"
        // AddressSpecifierArg  = "*" | <AddressBytes> | <NameAccessChain> [ <TypeArgs>? "(" <Identifier> ")" ]?
        access_specifier_list: $ => seq(sepBy1(',', $.access_specifier), optional(',')),
        access_specifier: $ => seq(name_access_chain($, true), optional($.type_args), optional(seq('(', $._address_specifier, ')'))),
        _address_specifier: $ => choice(
            '*',
            // NumericalAddress = <Number>
            field('literal_address', $.numerical_addr),
            seq(
                field('func', name_access_chain($, false)),
                optional(seq(
                    optional($.type_args),
                    // FIXME: only optional when NameAccessChain_::One(name) = chain.value.
                    seq('(', field('arg', $.identifier), ')')
                )),
            ),
        ),

        // StructDecl =
        //     "struct" <StructDefName> ("has" <Ability> (, <Ability>)+)?
        //     ("{" Comma<FieldAnnot> "}" | ";")
        // StructDefName        = <Identifier> <StructTypeParameter>?
        // StructTypeParameter  = '<' Comma<TypeParameterWithPhantomDecl> '>'
        // TypeParameterWithPhantomDecl = "phantom"? <TypeParameter>
        struct_decl: $ => seq(
            'struct',
            $._struct_def_name,
            optional(seq('has', $.abilities)),
            choice(field('body', $._struct_body), ';'),
        ),
        _struct_body: $ => choice(seq('{', sepByComma($.field_annot), '}')),
        _struct_def_name: $ => seq(
            field('struct_name', $.identifier),
            optional(seq('<', sepByComma($._struct_type_parameter), '>')),
        ),
        _struct_type_parameter: $ => seq(optional($.phantom), $.type_param),
        phantom: _ => 'phantom',

        // FieldAnnot = <DocComments> <Field> ":" <Type>
        field_annot: $ => seq(
            // TODO(doc): doc comments,
            field('field', $.identifier), ':', $.type
        ),

        // Parse a type ability
        //      Ability =
        //            "copy"
        //          | "drop"
        //          | "store"
        //          | "key"
        _ability: $ => choice('copy', 'drop', 'store', 'key'),

        abilities: $ => sepBy1(',', $._ability),

        // SequenceItem = <Exp> | "let" <BindList> (":" <Type>)? ("=" <Exp>)?
        _sequence_item: $ => seq(choice(
            $._expr,
            $.let_expr,
        ), ';'),

        let_expr: $ => seq('let', $.bind_list, optional(seq(':', $.type)), optional(seq('=', alias($._expr, $.value)))),

        // BindList = <Bind> | "(" Comma<Bind> ")"
        bind_list: $ => choice($._bind, seq('(', sepByComma($._bind), ')')),

        // Bind = <Var>
        //      | <NameAccessChain> <OptionalTypeArgs> "{" Comma<BindField> "}"
        _bind: $ => choice(
            field('variable', $.var_name),
            seq(name_access_chain($, false), optional($.type_args), '{', sepByComma($.bind_field), '}'),
        ),

        // OptionalTypeArgs = '<' Comma<Type> ">" | <empty>
        type_args: $ => seq('<', sepByComma($.type), '>'),

        // BindField    = <Field> <":" <Bind>>?
        // Field        = <Identifier>
        bind_field: $ => seq(field('field', $.var_name), optional(seq(':', $._bind))),

        // Parse a script:
        //      Script = "script" "{"
        //              (<Attributes> <UseDecl>)*
        //              (<Attributes> <ConstantDecl>)*
        //              <Attributes> <DocComments> <ModuleMemberModifiers> <FunctionDecl>
        //              (<Attributes> <SpecBlock>)*
        //      "}"
        script: $ => seq(
            'script', '{',
            // use declarations
            repeat(seq(optional($.attributes), $.use_decl)),
            // constant declarations
            repeat(seq(optional($.attributes), $.constant_decl)),
            // function
            field('function', seq(
                optional($.attributes),
                // TODO(doc): doc comments
                repeat($.module_member_modifier),
                $.function_decl,
            )),
            // spec blocks
            repeat(seq(optional($.attributes), $.spec_block)),
            '}'
        ),

        // Comments
        comments: $ => choice(
            $.line_comment,
            $.block_comment,
        ),

        // https://github.com/tree-sitter/tree-sitter-rust/blob/9c84af007b0f144954adb26b3f336495cbb320a7/grammar.js#L1527
        //
        // To differentiate between doc line comments and regular line comments, we need an external scanner.
        // Although it is possible to use regex to extract doc comments, tree-sitter behaves weirdly.
        line_comment: $ => seq(
            '//',
            choice(
                seq(token.immediate(prec(2, '//')), /.*/),
                // _doc_line_comment is essentially `/.*/` with trailing `\n`.
                // However, using regex to match `_doc_line_comment` is problematic due to confusion with '////xxx'
                seq(token.immediate(prec(2, '/')), field('doc', alias($._doc_line_comment, $.doc_comment))),
                token.immediate(prec(1, /.*/)),
            ),
        ),

        // block_comment: $ => seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/'),

        // External scanners are needed to match nested block (doc) comments.
        block_comment: $ => seq(
            '/*',
            optional(
                choice(
                    // Documentation block comments: /** docs */
                    seq(
                        $._block_doc_comment_marker,
                        optional(field('doc', alias($._block_comment_content, $.doc_comment))),
                    ),
                    // Non-doc block comments
                    $._block_comment_content,
                )
            ),
            '*/',
        ),
    }
});