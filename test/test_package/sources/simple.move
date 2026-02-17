/// What is there even in a module?
/// Let's see
/*
  (doc_comment [0, 0] - [1, 13])
  (module [2, 0] - [55, 1]
    address: (identifier [2, 7] - [2, 18])
    name: (identifier [2, 20] - [2, 26])
    (module_body [2, 27] - [55, 1]
*/
module module_addr::Simple_ {

    /* #[test_only]
     /// Test only struct
     /// Documentation, not line comment FIXME, not identified properly
     struct TestOnlyStruct {
         a: bool,
         b: u8,
         c: u16,
         d: u32,
         e: u64,
         f: u128,
         g: u256,
         h: address,
         i: signer,
     }*/

    /*
      (line_comment [34, 4] - [34, 19]) FIXME: This is supposed to be doc_comment
      (struct_declaration [35, 4] - [35, 48]
        name: (identifier [35, 11] - [35, 23])
        (abilities [35, 24] - [35, 45]
          (ability [35, 28] - [35, 33])
          (ability [35, 35] - [35, 39])
          (ability [35, 41] - [35, 45])))
          */
    /// Doc comment
    struct SimpleStruct has store, copy, drop {}

    /*
    (struct_declaration [46, 4] - [46, 78]
        name: (identifier [46, 11] - [46, 29])
        (type_parameters [46, 29] - [46, 53]
          (type_parameter [46, 30] - [46, 52]
            (identifier [46, 38] - [46, 39])
            (type_constraints [46, 39] - [46, 52]
              (ability [46, 41] - [46, 45])
              (ability [46, 48] - [46, 52]))))
        (abilities [46, 54] - [46, 75]
          (ability [46, 58] - [46, 63])
          (ability [46, 65] - [46, 69])
          (ability [46, 71] - [46, 75])))
    */
    struct TypedStructPhantom<phantom T: copy + drop> has store, copy, drop {}

    /*
    (struct_declaration [48, 4] - [50, 5]
        name: (identifier [48, 11] - [48, 22])
        (type_parameters [48, 22] - [48, 38]
          (type_parameter [48, 23] - [48, 37]
            (identifier [48, 23] - [48, 24])
            (type_constraints [48, 24] - [48, 37]
              (ability [48, 26] - [48, 30])
              (ability [48, 33] - [48, 37]))))
        (abilities [48, 39] - [48, 60]
          (ability [48, 43] - [48, 48])
          (ability [48, 50] - [48, 54])
          (ability [48, 56] - [48, 60]))
        (field_list [49, 8] - [49, 12]
          (field_declaration [49, 8] - [49, 12]
            name: (identifier [49, 8] - [49, 9])
            type: (type_identifier [49, 11] - [49, 12]
              (identifier [49, 11] - [49, 12])))))
    */
    struct TypedStruct<T: copy + drop> has store, copy, drop {
        a: T
    }

    /*
    (enum_declaration [20, 4] - [26, 5]
        name: (identifier [20, 9] - [20, 19])
        (abilities [20, 20] - [20, 41]
          (ability [20, 24] - [20, 29])
          (ability [20, 31] - [20, 35])
          (ability [20, 37] - [20, 41]))
    */
    enum SimpleEnum has store, copy, drop {
        /*
         (enum_variant_list [20, 42] - [26, 5]
          (enum_variant [21, 8] - [21, 24]
            name: (identifier [21, 8] - [21, 9])
            (positional_field_list [21, 10] - [21, 22]
              (type_identifier [21, 10] - [21, 22]
                (identifier [21, 10] - [21, 22]))))
        */
        A(SimpleStruct, TypedStruct<u8>),
        /*
        (enum_variant [22, 8] - [24, 10]
            name: (identifier [22, 8] - [22, 9])
            (field_list [23, 12] - [23, 49]
              (field_declaration [23, 12] - [23, 48]
                name: (identifier [23, 12] - [23, 13])
                type: (scoped_type_identifier [23, 15] - [23, 48]
                  (scoped_type_identifier [23, 15] - [23, 34]
                    (identifier [23, 15] - [23, 26])
                    (identifier [23, 28] - [23, 34]))
                  (identifier [23, 36] - [23, 48])))))
          */
        B {
            a: SimpleStruct,
        },
        /*
          (enum_variant [108, 8] - [108, 9]
            name: (identifier [108, 8] - [108, 9]))))
        */
        C
    }

    /*
      (attribute_item [153, 4] - [153, 74]
        (attribute [153, 6] - [153, 73]
          (identifier [153, 6] - [153, 27])
          (attribute_arguments [153, 28] - [153, 72]
            (attribute_assignment [153, 28] - [153, 72]
              (identifier [153, 28] - [153, 33])
              (scoped_identifier [153, 36] - [153, 72]
                (scoped_identifier [153, 36] - [153, 59]
                  (identifier [153, 36] - [153, 51])
                  (identifier [153, 53] - [153, 59]))
                (identifier [153, 61] - [153, 72]))))))
     (line_comment [116, 4] - [116, 22]) FIXME: Should be doc comment
      (struct_declaration [117, 4] - [127, 5]
        name: (identifier [117, 11] - [117, 21])
        (abilities [117, 22] - [117, 29]
          (ability [117, 26] - [117, 29]))
        (field_list [118, 8] - [126, 24]
          (field_declaration [118, 8] - [118, 15]
            name: (identifier [118, 8] - [118, 9])
            type: (primitive_type [118, 11] - [118, 15]))
          (field_declaration [119, 8] - [119, 13]
            name: (identifier [119, 8] - [119, 9])
            type: (primitive_type [119, 11] - [119, 13]))
          (field_declaration [120, 8] - [120, 14]
            name: (identifier [120, 8] - [120, 9])
            type: (primitive_type [120, 11] - [120, 14]))
          (field_declaration [121, 8] - [121, 14]
            name: (identifier [121, 8] - [121, 9])
            type: (primitive_type [121, 11] - [121, 14]))
          (field_declaration [122, 8] - [122, 14]
            name: (identifier [122, 8] - [122, 9])
            type: (primitive_type [122, 11] - [122, 14]))
          (field_declaration [123, 8] - [123, 15]
            name: (identifier [123, 8] - [123, 9])
            type: (primitive_type [123, 11] - [123, 15]))
          (field_declaration [124, 8] - [124, 15]
            name: (identifier [124, 8] - [124, 9])
            type: (primitive_type [124, 11] - [124, 15]))
          (field_declaration [125, 8] - [125, 18]
            name: (identifier [125, 8] - [125, 9])
            type: (primitive_type [125, 11] - [125, 18]))
          (field_declaration [126, 8] - [126, 23]
            name: (identifier [126, 8] - [126, 9])
            type: (type_identifier [126, 11] - [126, 23]
              (identifier [126, 11] - [126, 23])))))
    */
    #[resource_group_member(group = aptos_framework::object::ObjectGroup)]
    /// Struct allowed
    struct RealStruct has key {
        a: bool,
        b: u8,
        c: u16,
        d: u32,
        e: u64,
        f: u128,
        g: u256,
        h: address,
        i: SimpleStruct,
    }

    /*
     (attribute_item [166, 4] - [166, 16]
        (attribute [166, 6] - [166, 15]
          (identifier [166, 6] - [166, 15])))
      (line_comment [167, 4] - [167, 17])
      (enum_declaration [168, 4] - [175, 5]
        name: (identifier [168, 9] - [168, 21])
        (enum_variant_list [168, 22] - [175, 5]



    */
    #[test_only]
    /// Test only
    enum TestOnlyEnum {
        /*
        (enum_variant [169, 8] - [169, 10]
            name: (identifier [169, 8] - [169, 9]))
        */
        A,
        /*
         (enum_variant [170, 8] - [170, 14]
            name: (identifier [170, 8] - [170, 9])
            (positional_field_list [170, 10] - [170, 12]
              (primitive_type [170, 10] - [170, 12])))
        */
        B(u8),
        /*
        (enum_variant [171, 8] - [174, 9]
            name: (identifier [171, 8] - [171, 9])
            (field_list [172, 12] - [173, 18]


        */
        C {
            /*
            (field_declaration [172, 12] - [172, 18]
                name: (identifier [172, 12] - [172, 13])
                type: (primitive_type [172, 15] - [172, 18]))
                */
            a: u16,
            /*
            (field_declaration [173, 12] - [173, 18]
                name: (identifier [173, 12] - [173, 13])
                type: (primitive_type [173, 15] - [173, 18]))))))
            */
            b: u32
        }
    }

    enum RealEnum has drop {
        A,
        B(u8),
        C(TypedStruct<u8>),
        D(TypedStructPhantom<u8>),
        E {
            a: u8
        },
        F,
        // Note enum types must start with a capital letter
    }

    package fun do_stuff(input: RealEnum): u8 {
        // Looks like matching with a full type is fine
        let _ = match (&input) {
            RealEnum::A => &0,
            RealEnum::B(inner) => inner,
            RealEnum::C(inner) => &inner.a,
            RealEnum::D(_) => &100,
            RealEnum::E { a } => a,
            _ => &69,
        };

        match (&input) {
            RealEnum::A => 0,
            // Short names does not
            B(inner) => *inner,
            C(inner) => inner.a,
            D(_) => 100,
            E { a } => *a,
            _ => 69,
        }
    }

    fun a_(): address{
        @0123
    }
}
/*
address module_addr {
  module inner_mod {
      fun do_nothing() {}
  }
}

// FIXME: Secondary modules in the same code should be allowed like the compiler does
module module_addr::simple2 {
    fun do_nothing() {}
}

#[test_only]
module module_addr::simple_test {
    fun do_nothing() {}
}
*/
