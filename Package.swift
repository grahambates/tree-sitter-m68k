// swift-tools-version:5.3
import PackageDescription


let package = Package(
    name: "TreeSitterM68k",
    platforms: [.macOS(.v10_13), .iOS(.v11)],
    products: [
        .library(name: "TreeSitterM68k", targets: ["TreeSitterM68k"]),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "TreeSitterM68k",
            path: ".",
            exclude: [
                "binding.gyp",
                "bindings",
                "Cargo.toml",
                "corpus",
                "grammar.js",
                "LICENSE.md",
                "package-lock.json",
                "package.json",
                "README.md",
                "src/grammar.json",
                "src/node-types.json"
            ],
            sources: [
                "src/parser.c"
            ],
            resources: [
                .copy("queries")
            ],
            publicHeadersPath: "bindings/swift",
            cSettings: [
                .headerSearchPath("src")
            ]
        )
    ]
)
