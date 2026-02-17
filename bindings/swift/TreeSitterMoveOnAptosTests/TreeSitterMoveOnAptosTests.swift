import XCTest
import SwiftTreeSitter
import TreeSitterMoveOnAptos

final class TreeSitterMoveOnAptosTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Move on Aptos grammar")
    }

    func testParseModuleDeclaration() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::coin {
            struct Coin<phantom CoinType> has key, store {
                value: u64,
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)

        let root = tree!.rootNode
        XCTAssertNotNil(root)
        XCTAssertEqual(root!.nodeType, "source_file")

        // Verify no errors in parse tree
        XCTAssertFalse(root!.hasError)
    }

    func testParseFunctionWithControlFlow() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::example {
            public fun transfer(from: &signer, to: address, amount: u64): bool {
                if (amount > 0) {
                    true
                } else {
                    false
                }
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseEnumAndMatch() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::color {
            enum Color {
                Red,
                Green,
                Blue,
            }

            public fun is_primary(c: &Color): bool {
                match (c) {
                    Color::Red | Color::Blue => true,
                    _ => false,
                }
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseUseDeclarations() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::my_module {
            use std::vector;
            use 0x1::coin::{Self, Coin};
            use 0x1::account::Account as MyAccount;
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseSpecBlock() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::verified {
            fun add(a: u64, b: u64): u64 {
                a + b
            }

            spec add {
                ensures result == a + b;
            }

            spec module {
                pragma verify = true;
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseAttributes() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::tests {
            #[test]
            fun test_basic() {
                assert!(1 + 1 == 2, 0);
            }

            #[test, expected_failure(abort_code = 1)]
            fun test_failure() {
                abort 1
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseGenerics() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        module 0x1::container {
            struct Box<T: store + drop> has key {
                value: T,
            }

            public fun unbox<T: store + drop>(box: Box<T>): T {
                let Box { value } = box;
                value
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }

    func testParseAddressBlock() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_move_on_aptos())
        try parser.setLanguage(language)

        let source = """
        address 0x42 {
            module first {
                public fun one(): u64 { 1 }
            }
            module second {
                public fun two(): u64 { 2 }
            }
        }
        """

        let tree = parser.parse(source)
        XCTAssertNotNil(tree)
        XCTAssertFalse(tree!.rootNode!.hasError)
    }
}
