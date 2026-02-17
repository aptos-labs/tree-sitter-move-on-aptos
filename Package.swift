// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "TreeSitterMoveOnAptos",
    products: [
        .library(name: "TreeSitterMoveOnAptos", targets: ["TreeSitterMoveOnAptos"]),
    ],
    dependencies: [
        .package(url: "https://github.com/tree-sitter/swift-tree-sitter",
                 from: "0.8.0"),
    ],
    targets: [
        .target(name: "TreeSitterMoveOnAptos",
                path: ".",
                exclude: [
                    "Cargo.toml",
                    "Makefile",
                    "binding.gyp",
                    "bindings/c",
                    "bindings/go",
                    "bindings/node",
                    "bindings/python",
                    "bindings/rust",
                    "prebuilds",
                    "grammar.js",
                    "package.json",
                    "package-lock.json",
                    "pyproject.toml",
                    "setup.py",
                    "test",
                    "examples",
                    ".editorconfig",
                    ".github",
                    ".gitignore",
                    ".gitattributes",
                    ".gitmodules",
                ],
                sources: [
                    "src/parser.c",
                    "src/scanner.c",
                ],
                resources: [
                    .copy("queries")
                ],
                publicHeadersPath: "bindings/swift",
                cSettings: [.headerSearchPath("src")]),
        .testTarget(name: "TreeSitterMoveOnAptosTests",
                    dependencies: [
                        "TreeSitterMoveOnAptos",
                        .product(name: "SwiftTreeSitter", package: "swift-tree-sitter"),
                    ],
                    path: "bindings/swift/TreeSitterMoveOnAptosTests")
    ],
    cLanguageStandard: .c11
)
