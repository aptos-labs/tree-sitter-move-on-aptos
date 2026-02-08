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
}
