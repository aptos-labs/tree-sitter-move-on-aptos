/// Test module
///
module module_addr::test_module {

    #[resource_group_member(group = aptos_framework::object::ObjectGroup)]
    /// A single lockup, which has the same lockup period for all of them
    ///
    /// These are stored on objects, which map to the appropriate escrows
    enum TestEnum<phantom T> has key, drop {
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

    struct TestStruct<phantom T> has drop {
        v1: u8,
        v2: u16
    }

    public fun test_fun() {
        let enum1 = TestEnum::A<u8> {
            v1: 0,
            v2: 1
        };

        match (&enum1) {
            (TestEnum::A<u8> { v1: _a, v2 }) => {
                let _v2 = v2;
            }
            _ => {}
        };

        let TestEnum::A {
            v1,
            v2
        } = enum1;

        let enum2 = TestEnum::B<u8> {
            v1,
            v2,
            v3: true
        };

        match (&enum2) {
            B { v1, v2, v3 } => {
                let _ = if (*v3) *v1 else *v2 as u8;
            }
            _ => {}
        };

        let TestEnum::B {
            v1: _bv1,
            v3: _,
            ..
        } = enum2;

        let str = TestStruct<u8> {
            v1,
            v2,
        };
        let TestStruct { v1: _tsv1, .. } = str;

        let var: u8 = 0;
        let ref = &mut var;
        *ref = 1;

        let (_num, _b) = tuple(true);
    }

    public fun tuple(input: bool): (u8, bool) {
        let (num, b) = if (input) {
            (5, false)
        } else {
            (2, true)
        };

        (num, b)
    }

    enum Ordering {
        Equal,
        Greater,
        Less
    }

    native public fun compare<T>(first: &T, second: &T): Ordering;

    public fun is_eq(self: &Ordering): bool {
        self is Ordering::Equal
    }
}
