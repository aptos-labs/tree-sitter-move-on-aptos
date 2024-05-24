// Helper functions

// source: https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
const sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const sepBy = (sep, rule) => optional(sepBy1(sep, rule));
const sepByComma = (rule) => sepBy(',', rule);

const keyword = (word) => field(word, word);

const chain_ident = ($, wildcard) => field('path', (wildcard) ? choice('x', $.identifier) : $.identifier);
// LeadingNameAccess = <NumericalAddress> | <Identifier>
// `Identifier` can be `*` if `wildcard = true`
const leading_name_access = ($, wildcard) => field(
    'leading_name_access',
    choice(chain_ident($, wildcard), $.numerical_addr)
);
// NameAccessChain = <LeadingNameAccess> ( "::" <Identifier> ( "::" <Identifier> )? )?
// `Identifier` can be `*` if `wildcard = true`
const name_access_chain = ($, wildcard) => {
    const ident = () => chain_ident($, wildcard);
    return field(
        'name_access_chain',
        seq(
            leading_name_access($, wildcard),
            optional(field('access_two',
                seq('::', ident(),
                    optional(field('access_three', seq('::', ident()))))
            )),
        ),
    );
};

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
    default: 1,
    last: -1,

    // immediately after the multiplicative operators (*, /, %)
    unary: 13,
    field: 14,
    call: 15,
    range: 16,
};

module.exports = grammar({
    name: 'move_aptos',

    word: $ => $.identifier,

    conflicts: $ => [
        [$.type, $._bind],
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
            keyword('u8'), keyword('u16'), keyword('u32'), keyword('u64'), keyword('u128'), keyword('u256')
        ),

        primitive_type: $ => choice(
            $.number_type,
            keyword('bool'), keyword('address'), keyword('signer'), keyword('vector')
        ),

        identifier: _ => /[a-zA-Z_]\w*/,
        number: _ => choice(
            /\d[\d_]*/,
            /0[xX][\da-fA-F_]+/,
            /0b[01_]+/,
            /0o[0-7_]+/,
        ),
        numerical_addr: $ => $.number,
        typed_number: $ => alias(seq($.number, $.number_type), 'typed_number'),
        byte_string: _ => choice(
            /x\"[\da-fA-F]*\"/,
            /b\"[\w]*\"/,
        ),

        // Parse a Type:
        //      Type =
        //          <NameAccessChain> <TypeArgs>?
        //          | "&" <Type>
        //          | "&mut" <Type>
        //          | "|" Comma<Type> "|" Type
        //          | "(" Comma<Type> ")"
        type: $ => choice(
            prec(expr_precedence.default, $.primitive_type),
            seq(name_access_chain($, false), optional($.type_args)),
            field('ref', seq('&', $.type)),
            field('mut_ref', seq('&mut', $.type)),
            // `||' is treated as an empty param type list in this context.
            // TODO: verify the associativity
            prec.right(expr_precedence.default, field('closure_type', seq('|', sepByComma($.type), '|', $.type))),
            field('tuple', seq('(', sepByComma($.type), ')')),
        ),

        // Parse an expression:
        //      Exp =
        //            <LambdaBindList> <Exp>
        //          | <Quantifier>                  spec only
        //          | <BinOpExp>
        //          | <UnaryExp> "=" <Exp>
        expr: $ => choice(
            prec.left(field('assignment', seq($._unary_expr, '=', $.expr))),
            $._bin_op_expr,
            $.quantifier,
            field('lambda', seq($.lambda_bind_list, $.expr)),
        ),

        // Parse a list of bindings for lambda.
        //      LambdaBindList = "|" Comma<Bind> "|"
        lambda_bind_list: $ => seq('|', sepByComma($._bind), '|'),

        // Parses a quantifier expressions
        //
        //   <Quantifier> =
        //       ( "forall" | "exists" ) <QuantifierBindings> <Triggers>? ("where" <Exp>)? ":" Exp
        //     | ( "choose" [ "min" ] ) <QuantifierBind> "where" <Exp>
        //   <QuantifierBindings>   = <QuantifierBind> ("," <QuantifierBind>)*
        //   <QuantifierBind>       = <Identifier> ":" <Type> | <Identifier> "in" <Exp>
        //   <Triggers>             = ("{" Comma<Exp> "}")+
        quantifier: $ => choice(
            seq(
                field('scope', choice(keyword('forall'), keyword('exists'))),
                sepByComma($.quantifier_bind),
                optional($.triggers),
                optional(seq('where', $.expr)),
                ':',
                field('assertion', $.expr),
            ),
            seq(
                choice('choose', 'min'),
                $.quantifier_bind,
                'where',
                field('body', $.expr),
            )
        ),
        quantifier_bind: $ => choice(
            field('type_bind', seq(field('var', $.identifier), ':', $.type)),
            field('exist_bind', seq(field('var', $.identifier), 'in', field('scope', $.expr))),
        ),
        triggers: $ => repeat1(seq('{', field('trigger', $.expr), '}')),

        // Parse a binary operator expression:
        //      BinOpExp = <BinOpExp> <BinOp> <BinOpExp>
        //               | <UnaryExp>
        _bin_op_expr: $ => choice(
            prec(expr_precedence.unary, $._unary_expr),

            // binary operators
            ...binary_operators.flatMap(
                (level, index) => level.map(([symbol, name]) => prec.left(
                    index + 2,
                    field(name, seq($._bin_op_expr, symbol, $._bin_op_expr))
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
            field('not_expr', seq('!', $._unary_expr)),
            field('ref_expr', seq('&', $._unary_expr)),
            field('ref_mut_expr', seq('&mut', $._unary_expr)),
            field('deref_expr', seq('*', $._unary_expr)),
            field('move_expr', seq('move', $._unary_expr)),
            field('copy_expr', seq('copy', $._unary_expr)),

            prec(expr_precedence.field, $._dot_or_index_chain),
        ),

        // Parse an expression term optionally followed by a chain of dot or index accesses:
        //      DotOrIndexChain =
        //          <DotOrIndexChain> "." <Identifier> [ ["::" <TypeArgs>]  <CallArgs> ]
        //          | <DotOrIndexChain> "[" <Exp> "]"                      spec only
        //          | <Term>
        _dot_or_index_chain: $ => choice(
            field('dot', seq($._dot_or_index_chain, '.', field('field', $.identifier))),
            field('call', seq($._dot_or_index_chain, '.', field('field', $.identifier),
                optional(field('type_generics', seq('::', $.type_args))),
                $.call_args,
            )),
            field('mem_access', seq($._dot_or_index_chain, '[', field('index', $.expr), ']')),
            field('value', $.term),
        ),

        // Parse an expression term:
        //      Term =
        //          "break"
        //          | "continue"
        //          | "vector" ('<' Comma<Type> ">")? "[" Comma<Exp> "]"
        //          | <Value>
        //          | "(" Comma<Exp> ")"
        //          | "(" <Exp> ":" <Type> ")"
        //          | "(" <Exp> "as" <Type> ")"
        //          | "{" <Sequence>
        //          | "if" "(" <Exp> ")" <Exp> "else" "{" <Exp> "}"
        //          | "if" "(" <Exp> ")" "{" <Exp> "}"
        //          | "if" "(" <Exp> ")" <Exp> ("else" <Exp>)?
        //          | "while" "(" <Exp> ")" "{" <Exp> "}"
        //          | "while" "(" <Exp> ")" <Exp> (SpecBlock)?
        //          | "loop" <Exp>
        //          | "loop" "{" <Exp> "}"
        //          | "return" "{" <Exp> "}"
        //          | "return" <Exp>?
        //          | "abort" "{" <Exp> "}"
        //          | "abort" <Exp>
        //          | "for" "(" <Exp> "in" <Exp> ".." <Exp> ")" "{" <Exp> "}"
        //          | <NameExp>
        //
        // The conflict resolution is based on `tree-sitter-javascript`'s approach.
        // TODO: make sure this behaves the same as the `move-compiler`.
        //       `lambda_bind_list` might also be involved.
        term: $ => choice(
            field('break_expr', 'break'),
            field('continue_expr', 'continue'),
            field('vector_access', seq('vector', optional($.type_args), '[', sepByComma($.expr), ']')),
            field('value_expr', $.value),
            field('tuple_expr', seq('(', sepByComma($.type), ')')),
            seq('(', $.expr, ':', $.type, ')'),
            field('cast_expr', seq('(', $.expr, 'as', $.type, ')')),

            $.expr_block,
            $._name_expr,

            field('spec_block', seq('spec', $.spec_block)),

            // control flow expressions
            $.if_expr,
            $.while_expr,
            $.loop_expr,
            $.return_expr,
            $.abort_expr,
            $.for_loop_expr,
        ),
        parenthesized_expr: $ => seq('(', $.expr, ')'),
        expr_block: $ => prec.right(seq('{', $.expr, '}')),
        // Control flow expressions:
        if_expr: $ => prec.right(seq(
            'if',
            field('condition', $.parenthesized_expr),
            field('then', $.expr),
            optional(field('else', seq('else', $.expr)))
        )),
        while_expr: $ => seq(
            'while',
            field('condition', $.parenthesized_expr),
            field('body', $.expr),
        ),
        loop_expr: $ => seq('loop', field('body', $.expr)),
        return_expr: $ => choice(
            prec(expr_precedence.last, 'return'),
            prec.left(seq('return', field('value', $.expr))),
        ),
        abort_expr: $ => seq('abort', field('condition', $.expr)),
        for_loop_expr: $ => seq(
            'for', '(',
            field('init', $.expr), 'in', field('range', $.expr), '..', field('increment', $.expr),
            ')',
            field('body', $.expr_block),
        ),

        // Parse a pack, call, or other reference to a name:
        //      NameExp =
        //          <NameAccessChain> <OptionalTypeArgs> "{" Comma<ExpField> "}"
        //          | <NameAccessChain> <OptionalTypeArgs> <CallArgs>
        //          | <NameAccessChain> "!" <CallArgs>
        //          | <NameAccessChain> <OptionalTypeArgs>
        _name_expr: $ => choice(
            // <NameAccessChain> <TypeArgs>? prefix
            field('name', seq(name_access_chain($, false), optional(field('generics', $.type_args)))),
            prec(expr_precedence.call, field('call', seq(
                name_access_chain($, false), optional(field('generics', $.type_args)), $.call_args
            ))),
            prec(expr_precedence.default, field('pack', seq(
                name_access_chain($, false), optional(field('generics', $.type_args)),
                '{', sepByComma($.expr_field), '}'
            ))),
            // macro call
            prec(expr_precedence.default, field('macro_call', seq(name_access_chain($, false), '!', $.call_args))),
        ),

        call_args: $ => seq('(', sepByComma($.expr), ')'),
        type_args: $ => seq('<', sepByComma($.type), '>'),

        // Parse a field name optionally followed by a colon and an expression argument:
        //      ExpField = <Field> <":" <Exp>>?
        expr_field: $ => seq(
            field('field', $.identifier),
            optional(field('value', seq(':', $.expr)))
        ),


        // Parse a value:
        //      Value =
        //          "@" <LeadingNameAccess>
        //          | "true"
        //          | "false"
        //          | <Number>
        //          | <NumberTyped>
        //          | <ByteString>
        value: $ => prec(expr_precedence.default, choice(
            seq('@', leading_name_access($, false)),
            'true',
            'false',
            $.number,
            $.typed_number,
            $.byte_string,
        )),

        // Attributes = ("#" "[" Comma<Attribute> "]")*
        // However, tree sitter does not allow empty matching. Thus, `attributes` only
        // accepts non-empty attribute list.
        attributes: $ => repeat1(seq('#', '[', sepByComma($.attribute), ']')),

        // Attribute =
        //     <Identifier>
        //     | <Identifier> "=" <AttributeValue>
        //     | <Identifier> "(" Comma<Attribute> ")"
        attribute: $ => choice(
            field('attribute_item', $.identifier),
            seq(field('attribute_item', $.identifier), '=', $.attribute_val),
            seq(field('attribute_item', $.identifier), '(', sepByComma($.attribute), ')')
        ),

        // Parse an attribute value. Either a value literal or a module access
        //  AttributeValue = <Value> | <NameAccessChain>
        attribute_val: $ => choice(prec(expr_precedence.default, $.value), name_access_chain($, false)),

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
        //               ( <UseDecl> | <FriendDecl> | <SpecBlock> |
        //                 <DocComments> <ModuleMemberModifiers>
        //                     (<ConstantDecl> | <StructDecl> | <FunctionDecl>) )
        //               )
        //           )*
        //       "}"
        //   ModuleMemberModifiers = <ModuleMemberModifier>*
        module: $ => seq(
            // TODO: doc comments are not supported by now.
            choice('spec', 'module'),
            // (<LeadingNameAccess>::)?<ModuleName>
            seq(
                optional(seq(field('path', leading_name_access($, false)), '::')),
                field('module_name', $.identifier)
            ),
            '{', repeat(seq(
                optional($.attributes),
                choice(
                    $.use_decl,
                    $.friend_decl,
                    $.spec_block,

                    seq(
                        // TODO: doc comments
                        repeat($.module_member_modifier),
                        choice($.constant_decl, $.struct_decl, $.function_decl)
                    ),
                )
            )), '}'
        ),

        // TODO: spec block
        spec_block: $ => choice('singleton', 'regular'),

        // Visibility = "public" ( "(" "script" | "friend" ")" )?
        visibility: $ => seq(keyword('public'), optional(seq('(', choice(keyword('script'), keyword('friend')), ')'))),

        // ModuleMemberModifier = <Visibility> | "native"
        module_member_modifier: $ => choice($.visibility, keyword('native')),

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
                $._use_member,
                seq('{', sepByComma($._use_member), '}')
            ), ';'),

        // UseAlias = ("as" <Identifier>)?
        _use_alias: $ => seq('as', field('alias_name', $.identifier)),

        // UseMember = <Identifier> <UseAlias>
        _use_member: $ => seq(field('member', $.identifier), optional($._use_alias)),

        // FriendDecl = "friend" <NameAccessChain>(wildcard: false) ";"
        friend_decl: $ => seq('friend', name_access_chain($, false), ';'),

        // ConstantDecl = "const" <Identifier> ":" <Type> "=" <Exp> ";"
        constant_decl: $ => seq('const', field('name', $.identifier), ':', $.type, '=', field('value', $.expr), ';'),

        // Parse a function declaration:
        //      FunctionDecl =
        //          [ "inline" ] "fun" <FunctionDefName>
        //          <OptionalTypeParameters>
        //          "(" Comma<Parameter> ")"
        //          (":" <Type>)?
        //          ( (( "!" )? ("acquires" | "reads" | "writes" ) | "pure") <AccessSpecifierList> )*
        //          ("{" <Sequence> "}" | ";")
        //      OptionalTypeParameters = '<' Comma<TypeParameter> ">" | <empty>
        //      Sequence = <UseDecl>* (<SequenceItem> ";")* <Exp>?
        function_decl: $ => seq(
            optional(keyword('inline')),
            'fun', field('function_name', $.identifier),
            optional(seq('<', sepByComma($.type_param), '>')),
            '(', field('parameters', sepByComma($.parameter)), ')',
            optional(seq(':', $.type)),
            repeat(seq(
                choice(
                    // Only `acquires`, `reads` and `writes` can be negated.
                    seq(optional(field('negated', '!')), choice('acquires', 'reads', 'writes')),
                    'pure'
                ),
                $.access_specifier_list
            )),
            // Sequence
            choice(field('body', seq('{',
                seq(
                    repeat($.use_decl),
                    repeat(seq($.sequence_item, ';')),
                    optional($.expr),
                ), '}')), ';'),
        ),

        // Parse a type parameter:
        //  TypeParameter   = <Identifier> <Constraint>?
        //  Constraint      = ":" <Ability> (+ <Ability>)*
        type_param: $ => seq(field('type', $.identifier), optional($.constraints)),
        constraints: $ => seq(':', sepBy1('+', $._ability)),

        // Parameter = <Var> ":" <Type>
        parameter: $ => seq(field('variable', $.identifier), ':', $.type),

        // AccessSpecifierList  = <AccessSpecifier> ( "," <AccessSpecifier> )* ","?
        // AccessSpecifier      = <NameAccessChainWithWildcard> <AddressSpecifier>
        // AddressSpecifier     = <empty> | "(" <AddressSpecifierArg> ")"
        // AddressSpecifierArg  = "*" | <AddressBytes> | <NameAccessChain> <TypeArgs>? "(" <Identifier> ")"
        access_specifier_list: $ => seq($.access_specifier, repeat(seq(',', $.access_specifier)), optional(',')),
        access_specifier: $ => seq(name_access_chain($, true), optional(seq('(', $._address_specifier, ')'))),
        _address_specifier: $ => choice(
            '*',
            // NumericalAddress = <Number>
            field('literal_address', $.number),
            seq(
                field('func', name_access_chain($, false)),
                optional($.type_args),
                // TODO: can be optional when NameAccessChain_::One(name) = chain.value.
                '(', field('arg', $.identifier), ')'
            ),
        ),

        // StructDecl =
        //     "struct" <StructDefName> ("has" <Ability> (, <Ability>)+)?
        //     ("{" Comma<FieldAnnot> "}" | ";")
        // StructDefName =
        //     <Identifier> <OptionalTypeParameters>
        struct_decl: $ => seq(
            'struct',
            field('struct_name', $.identifier),
            optional(seq('has', $.abilities)),
            choice(field('body', seq('{', sepByComma($.field_annot), '}')), ';')
        ),

        // FieldAnnot = <DocComments> <Field> ":" <Type>
        field_annot: $ => seq(
            // TODO: doc comments,
            field('field', $.identifier), ':', $.type
        ),

        // Parse a type ability
        //      Ability =
        //            "copy"
        //          | "drop"
        //          | "store"
        //          | "key"
        _ability: $ => choice(keyword('copy'), keyword('drop'), keyword('store'), keyword('key')),

        abilities: $ => sepBy1(',', $._ability),

        // SequenceItem = <Exp> | "let" <BindList> (":" <Type>)? ("=" <Exp>)?
        sequence_item: $ => choice(
            $.expr,
            $.let_expr,
        ),

        let_expr: $ => seq('let', $.bind_list, optional(seq(':', $.type)), optional(seq('=', $.expr))),

        // BindList = <Bind> | "(" Comma<Bind> ")"
        bind_list: $ => choice($._bind, seq('(', sepByComma($._bind), ')')),

        // Bind = <Var>
        //      | <NameAccessChain> <OptionalTypeArgs> "{" Comma<BindField> "}"
        _bind: $ => choice(
            field('variable', $.identifier),
            seq(name_access_chain($, false), optional($.type_args), '{', sepByComma($.bind_field), '}'),
        ),

        // OptionalTypeArgs = '<' Comma<Type> ">" | <empty>
        type_args: $ => seq('<', sepByComma($.type), '>'),

        // BindField    = <Field> <":" <Bind>>?
        // Field        = <Identifier>
        bind_field: $ => seq(field('field', $.identifier), optional(seq(':', $._bind))),

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
                // TODO: doc comments
                repeat($.module_member_modifier),
                $.function_decl,
            )),
            // spec blocks
            repeat(seq(optional($.attributes), $.spec_block)),
            '}'
        ),
    }
});
