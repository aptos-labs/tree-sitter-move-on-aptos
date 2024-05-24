module 0x42::example {
    use aptos_framework::aptos_coin::AptosCoin as aptos_coin;
    use aptos_framework::coin::{Self, Coin as coin};
    use aptos_framework::create_signer::create_signer;
    use aptos_framework::event::{EventHandle, emit_event, emit};
    use std::error;

    friend aptos_framework::genesis;
    friend aptos_framework::resource_account;

    struct CapabilityOffer<phantom T> has store { for: Option<address> }

    #[event]
    struct KeyRotation has drop, store {
        account: address,
        old_authentication_key: vector<u8>,
        new_authentication_key: vector<u8>,
    }

    /// Resource representing an account.
    struct Account has key, store {
        authentication_key: vector<u8>,
        sequence_number: u64,
        guid_creation_num: u64,
        coin_register_events: EventHandle<CoinRegisterEvent>,
        key_rotation_events: EventHandle<KeyRotationEvent>,
        rotation_capability_offer: CapabilityOffer<RotationCapability>,
        signer_capability_offer: CapabilityOffer<SignerCapability>,
    }

    struct Example has copy, drop { i: u64 }

    use std::debug;
    friend 0x42::another_example;

    const ONE: u64 = 1;

}