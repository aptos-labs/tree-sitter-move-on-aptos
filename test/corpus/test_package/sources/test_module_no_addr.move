/**
 Block doc comments
*/
module 0x1234::test_module_no_addr {
    const HEX: vector<u8> = x"0123456789ABCDEFabcdef";
    const BYTE: vector<u8> = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_+=<>,./?':;\"`~!@#$%^&*()    ";

    friend 0x1234::test_module;
    /*
      Block comments
    */

    // One line comment

    /// Doc comment
    public(friend) fun do_nothing(): bool { true }

    friend fun do_something(): bool { false }
}
