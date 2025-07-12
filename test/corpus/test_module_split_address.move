// Legacy syntax
address 0x1234 {
    /**
     Block doc comments
    */
    module test_module_no_addr {
        const HEX: vector<u8> = x"0123456789ABCDEFabcdef";
        const BYTE: vector<u8> = b"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_+=<>,./?':;\"`~!@#$%^&*()    ";

        /*
          Block comments
        */

        // One line comment

        /// Doc comment
        fun do_nothing(): bool { true }
    }
}
