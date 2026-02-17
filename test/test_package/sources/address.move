/// Address of framework
address 0x1 {
#[test_only]
/// Module declaration
module one1 {
    use aptos_framework::fungible_asset;
    use aptos_framework::coin as coin;
    use aptos_framework::fungible_asset::deposit;
    use aptos_framework::fungible_asset::withdraw as w;
    use aptos_framework::fungible_asset::Metadata;
    use aptos_framework::fungible_asset::FungibleAsset as fa;
    use aptos_std::debug::{Self, Box, print};

    use aptos_framework::aptos_coin::{Self as a, AptosCoin as ac, initialize as b};

    /// do something
    fun test1() {}

    inline fun test2(): address {
        @1
    }

    public inline fun test3(): address {
        @aptos_framework
    }

    package fun test4(): bool {
        true
    }

    inline fun test5(
        a: u8,
        b: u16,
        c: u32,
        d: u64,
        e: u128,
        f: u256,
        A: address,
        signer: signer,
        package: bool,
        _: u8,
        _ignored: u8,
    ): u8 {
    }
}
}

