// swift-tools-version:5.1

import PackageDescription

let package = Package(
    name: "swiftlox",
    // platforms: [
    //     .macOS(SupportedPlatform.MacOSVersion.v12)
    // ],
    products: [
        .executable(name: "swiftlox", targets: ["Lox"]),
    ],
    dependencies: [],
    targets: [
        .target(
            name: "Lox",
            dependencies: []
        ),
    ]
)