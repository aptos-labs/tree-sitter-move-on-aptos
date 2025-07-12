/// Test module
///
module module_addr::test_module {

    use std::option::{Self, Option};
    use std::signer;
    use aptos_std::smart_table::{Self, SmartTable};
    use aptos_framework::dispatchable_fungible_asset;
    use aptos_framework::fungible_asset::{Self, Metadata, FungibleStore};
    use aptos_framework::object::{Self, Object, ExtendRef, DeleteRef};
    use aptos_framework::primary_fungible_store;
    use aptos_framework::timestamp;

    #[resource_group_member(group = aptos_framework::object::ObjectGroup)]
    /// A single lockup, which has the same lockup period for all of them
    ///
    /// These are stored on objects, which map to the appropriate escrows
    enum TestEnum has drop {
        A {
            v1: u8,
            v2: u16,
        }
        B {
            v1: u8,
            v2: u16,
            v3: bool
        }
    }

    struct TestStruct has drop {
        v1: u8,
        v2: u16
    }

    public fun test_fun() {
        let enum1 = TestEnum::A {
            v1: 0,
            v2: 1
        };

        match (enum1) {
            (TestEnum::A { v1: a, v2 }) => {}
            _ => {}
        };

        let TestEnum::A {
            v1,
            v2
        } = enum1;

        let enum2 = TestEnum::B {
            v1,
            v2,
            v3: true
        };

        match (enum2) {
            B { v1, v2, v3 } => {}
            _ => {}
        };

        let TestEnum::B {
            v1: bv1,
            v3: _,
            ..
        } = enum2;

        let str = TestStruct {
            v1,
            v2,
        };
        let TestStruct { v1: tsv1, .. } = str;

        let var: u8 = 0;
        let ref = &mut var;
        let ref_ref = &mut ref;
        *var = 1;
        **var = 2;

        let (num, b) = tuple(true);
    }

    public fun tuple(input: bool): (u8, bool) {
        let var = if (input) {
            (5, false)
        } else {
            (2, true)
        };

        var
    }
}
