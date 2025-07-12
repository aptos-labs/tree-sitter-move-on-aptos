script {
    use aptos_framework::object::Object;
    use aptos_framework::option::Option;
    use aptos_framework::aptos_account;
    use aptos_std::signer;
    use aptos_framework::object;

    /// Comment describing function
    fun some_Name<T: key, U: drop>(
        account1: &signer,
        account2: &signer,
        /*i1: u8,
        i2: u16,
        i3: u32,
        i4: u64,
        i5: u128,
        i6: u256,
        i7: address,
        i8: bool,
        i9: vector<u8>,
        i10: Object<T>,
        i11: Option<u256>,
        _I_12: U*/
    ) {
        // TODO: add more info here
        /*let total = (i1 as u256) + (i2 as u256) + (i3 as u256) + (i4 as u256) + (i5 as u256) + i6;

        if (i8) total *= 2 else total /= 2;
        if (i9.length() >= 1) {
            total += (i9[0] as u256);
        } else if (i11.is_some()) {
            total -= i11.destroy_some();
        } else {
            total %= 2
        };
        let receiver = signer::address_of(account2);
        */
        let balance: u64 = (total / 3) as u64; // FIXME: Should support no parens cast
        aptos_account::transfer(account1, receiver, balance);
        aptos_account::transfer(account1, i7, balance);
        let addr = object::object_address(&i10);
        aptos_account::transfer(account1, addr, balance);
    }
}
