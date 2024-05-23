// Helper functions

// source: https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
const sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const sepBy = (sep, rule) => optional(sepBy1(sep, rule));

const sepByComma = (rule) => sepBy(',', rule);

const leading_name_access = ($, wildcard) =>
    (wildcard)
        ? choice($.identifier, '*', $.numerical_addr)
        : choice($.identifier, $.numerical_addr);
const name_access_chain = ($, wildcard) => {
    // TODO.
    return 'name_access_chain';
};


module.exports = grammar({
    name: 'move_aptos',

    word: $ => $.identifier,

    rules: {
        // Parse a file:
        //  File = (<Attributes> (<AddressBlock> | <Module> | <Script>))*
        source_file: $ => repeat(seq(
            // Attributes = ("#" "[" Comma<Attribute> "]")*
            optional($.attributes),
            // <AddressBlock> | <Module> | <Script>
            choice($.module, $.script, $.address_block)
        )),

        // TODO: identifier
        identifier: $ => 'identifier',

        // TODO: number
        number: $ => 'number',

        // TODO: number typed
        number_typed: $ => 'number typed',

        // TODO: byte_string
        byte_string: $ => 'byte string',

        // TODO: numerical address
        numerical_addr: $ => 'numerical address',

        // Parse a Type:
        //      Type =
        //          <NameAccessChain> ('<' Comma<Type> ">")?
        //          | "&" <Type>
        //          | "&mut" <Type>
        //          | "|" Comma<Type> "|" Type
        //          | "(" Comma<Type> ")"
        type: $ => choice(
            seq(name_access_chain($, false), optional(seq('<', sepByComma($.type), '>'))),
            field('ref', seq('&', $.type)),
            field('mut_ref', seq('&mut', $.type)),
            // `||' is treated as an empty param type list in this context.
            // TODO: verify the associativity
            prec.right(1, field('closure', seq('|', sepByComma($.type), '|', $.type))),
            field('tuple', seq('(', sepByComma($.type), ')')),
        ),

        // Parse an expression:
        //      Exp =
        //            <LambdaBindList> <Exp>
        //          | <Quantifier>                  spec only
        //          | <BinOpExp>
        //          | <UnaryExp> "=" <Exp>
        expr: $ => choice(
            field('assignment', seq($.unary_expr, '=', $.expr)),
            $.bin_op_expr,
            $.quantifier,
            field('lambda', seq($.lambda_bind_list, $.expr)),
        ),

        // Parse a list of bindings for lambda.
        //      LambdaBindList = "|" Comma<Bind> "|"
        lambda_bind_list: $ => seq('|', sepByComma($._bind), '|'),

        // TODO: binary operations
        bin_op_expr: $ => 'bin_op_expr',

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
                field('scope', choice('forall', 'exists')),
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

        // Parse a unary expression:
        //      UnaryExp =
        //          "!" <UnaryExp>
        //          | "&mut" <UnaryExp>
        //          | "&" <UnaryExp>
        //          | "*" <UnaryExp>
        //          | "move" <Var>
        //          | "copy" <Var>
        //          | <DotOrIndexChain>
        unary_expr: $ => choice(
            field('not_expr', seq('!', $.unary_expr)),
            field('ref_expr', seq('&', $.unary_expr)),
            field('ref_mut_expr', seq('&mut', $.unary_expr)),
            field('deref_expr', seq('*', $.unary_expr)),
            field('move_expr', seq('move', $.unary_expr)),
            field('copy_expr', seq('copy', $.unary_expr)),

            $.dot_or_index_chain,
        ),

        // Parse an expression term optionally followed by a chain of dot or index accesses:
        //      DotOrIndexChain =
        //          <DotOrIndexChain> "." <Identifier> [  "(" Comma<Exp> ")" ]
        //          | <DotOrIndexChain> "[" <Exp> "]"                      spec only
        //          | <Term>
        dot_or_index_chain: $ => choice(
            field('chain', seq(
                $.dot_or_index_chain, '.', field('field', $.identifier),
                optional(seq('(', field('args', sepByComma($.expr)), ')'))
            )),
            field('access', seq($.dot_or_index_chain, '[', field('index', $.expr), ']')),
            $.term,
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
        //
        // The conflict resolution is based on `tree-sitter-javascript`'s approach.
        // TODO: make sure this behaves the same as the `move-compiler`.
        //       `lambda_bind_list` might also be involved.
        term: $ => choice(
            field('break_expr', 'break'),
            field('continue_expr', 'continue'),
            seq('vector', optional(seq('<', sepByComma($.type), '>')), '[', sepByComma($.expr), ']'),
            field('value_expr', $.value),
            field('tuple_expr', seq('(', sepByComma($.type), ')')),
            seq('(', $.expr, ':', $.type, ')'),
            field('cast_expr', seq('(', $.expr, 'as', $.type, ')')),

            $.expr_block,

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

        if_expr: $ => prec.right(seq(
            'if', field('condition', $.parenthesized_expr),
            field('then', $.expr),
            optional(field('else', seq('else', $.expr)))
        )),
        while_expr: $ => seq(
            'while',
            field('cond', $.parenthesized_expr),
            field('body', $.expr),
            // FIXME: "while" "(" <Exp> ")" <Exp> (SpecBlock)?
        ),
        loop_expr: $ => seq('loop', field('body', $.expr)),
        return_expr: $ => seq('return', optional(field('value', $.expr))),
        abort_expr: $ => seq('abort', field('condition', $.expr)),
        for_loop_expr: $ => seq(
            'for', '(',
            field('init', $.expr), 'in', field('range', $.expr), '..', field('increment', $.expr),
            ')',
            field('body', $.expr_block),
        ),


        // Parse a value:
        //      Value =
        //          "@" <LeadingNameAccess>
        //          | "true"
        //          | "false"
        //          | <Number>
        //          | <NumberTyped>
        //          | <ByteString>
        value: $ => choice(
            seq('@', leading_name_access($, false)),
            'true',
            'false',
            $.number,
            $.number_typed,
            $.byte_string,
        ),

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
        attribute_val: $ => choice($.value, name_access_chain($, false)),

        // TODO: name_access_chain (treated with special care)
        _name_access_chain: $ => 'name_access_chain',

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
            seq(optional(seq(leading_name_access($, false), '::')), field('module_name', $.identifier)),
            '{', repeat(seq(
                optional($.attributes),
                choice(
                    $.use_decl,
                    $.friend_decl,
                    $.spec_block,

                    field('declaration', seq(
                        // TODO: doc comments
                        repeat($.module_member_modifier),
                        choice($.constant_decl, $.struct_decl, $.function_decl)
                    )),
                )
            )), '}'
        ),

        // TODO: spec block
        spec_block: $ => choice('singleton', 'regular'),

        // Visibility = "public" ( "(" "script" | "friend" ")" )?
        visibility: $ => seq('public', optional(seq('(', choice('script', 'friend'), ')'))),

        // ModuleMemberModifier = <Visibility> | "native"
        module_member_modifier: $ => choice($.visibility, 'native'),

        // ModuleIdent = <LeadingNameAccess>(wildcard = false) "::" <ModuleName>
        module_ident: $ => seq(leading_name_access($, false), '::', field('module_name', $.identifier)),

        // Parse a use declaration:
        //      UseDecl =
        //          "use" <ModuleIdent> <UseAlias> ";" |
        //          "use" <ModuleIdent> :: <UseMember> ";" |
        //          "use" <ModuleIdent> :: "{" Comma<UseMember> "}" ";"
        use_decl: $ => seq('use', $.module_ident, choice(
            $._use_alias,
            $._use_member,
            seq('{', sepByComma($._use_member), '}')
        ), ';'),

        // UseAlias = ("as" <Identifier>)?
        _use_alias: $ => seq('as', field('alias_name', $.identifier)),

        // UseMember = <Identifier> <UseAlias>
        _use_member: $ => seq(field('original_member', $.identifier), $._use_alias),

        // FriendDecl = "friend" <NameAccessChain>(wildcard: false) ";"
        friend_decl: $ => seq('friend', name_access_chain($, false), ';'),

        // ConstantDecl = "const" <Identifier> ":" <Type> "=" <Exp> ";"
        constant_decl: $ => seq('const', field('const_name', $.identifier), ':', $.type, '=', $.expr, ';'),

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
            optional('inline'), 'fun', field('function_name', $.identifier),
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
        type_param: $ => seq(
            field('type', $.identifier),
            optional(field('constraints', seq(':', $.ability, repeat(seq('+', $.ability)))))
        ),

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
            'struct', field('struct_def_name', $.identifier),
            optional(seq('has', sepBy1(',', $.ability))),
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
        ability: $ => choice('copy', 'drop', 'store', 'key'),

        // SequenceItem = <Exp> | "let" <BindList> (":" <Type>)? ("=" <Exp>)?
        sequence_item: $ => choice(
            $.expr,
            field('let_statement', seq('let', $.bind_list, optional(seq(':', $.type)), optional(seq('=', $.expr))))
        ),

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
