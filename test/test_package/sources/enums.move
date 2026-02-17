module 0x42::test_enums {

    struct S has copy, drop, store { f: u64 }

    enum InnerEnum has store, copy, drop{
        A, B, C
    }

    #[resource_group_member(group = aptos_framework::object::ObjectGroup)]
    enum Example has key, copy, drop {
        A,
        B(u64),
        C {
            a: bool,
            b: u8,
            s: S,
        },
        D {
            inner: InnerEnum
        }
    }

    #[test_only]
    fun test_match(e: Example): u64 {
        match (e) {
            Example::A => 0,
            Example::B(x) => x,
            // FIXME: Support '..'
            Example::C { b, s: S { f }, a: _ }=> {
                (b as u64) + f
            },
            _ => {
                1337
            }
        }

    }

    #[view]
    package fun view_e(addr: address): Example acquires Example {
        Example[addr]
    }

    public(package) inline fun borrow_e_old(addr: address): &Example {
        borrow_global<Example>(addr)
    }

    inline fun borrow_e_new(addr: address): &Example {
        &Example[addr]
    }

    public inline fun borrow_e_mut_old(addr: address): &mut Example {
        borrow_global_mut<Example>(addr)
    }

    inline fun borrow_e_mut_new(addr: address): &mut Example {
        &mut Example[addr]
    }

    public fun access_array(vec: vector<Example>, i: u64): Example {
        vec[i]
    }
}
