// Legacy syntax
address 0x1234 {
    /**
     Block doc comments
    */
    module test_module_split {
        const HEX: vector<u8> = x"0123456789ABCDEFabcdef";

        /*
          Block comments
        */

        // One line comment

        /// Doc comment
        fun do_nothing(): bool { true }
    }
}
