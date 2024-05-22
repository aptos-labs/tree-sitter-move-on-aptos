// Helper functions

// source: https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js
const sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)));
const sepBy = (sep, rule) => optional(sepBy1(sep, rule));

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

        // TODO: type
        type: $ => 'type',

        // TODO: expression
        expr: $ => 'expr',

        // TODO: numerical address
        numerical_addr: $ => 'numerical address',

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
        attributes: $ => repeat1(seq('#', '[', sepBy(',', $.attribute), ']')),

        // Attribute =
        //     <Identifier>
        //     | <Identifier> "=" <AttributeValue>
        //     | <Identifier> "(" Comma<Attribute> ")"
        attribute: $ => choice(
            field('attribute_item', $.identifier),
            seq(field('attribute_item', $.identifier), '=', $.attribute_val),
            seq(field('attribute_item', $.identifier), '(', sepBy(',', $.attribute), ')')
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
            seq('{', sepBy(',', $._use_member), '}')
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
            optional(seq('<', sepBy(',', $.type_param), '>')),
            '(', field('parameters', sepBy(',', $.parameter)), ')',
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
            choice(field('body', seq('{', sepBy(',', $.field_annot), '}')), ';')
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
        bind_list: $ => choice($._bind, seq('(', sepBy(',', $._bind), ')')),

        // Bind = <Var>
        //      | <NameAccessChain> <OptionalTypeArgs> "{" Comma<BindField> "}"
        _bind: $ => choice(
            field('variable', $.identifier),
            seq(name_access_chain($, false), optional($.type_args), '{', sepBy(',', $.bind_field), '}'),
        ),

        // OptionalTypeArgs = '<' Comma<Type> ">" | <empty>
        type_args: $ => seq('<', sepBy(',', $.type), '>'),

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
