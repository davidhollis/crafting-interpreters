import Darwin

if CommandLine.arguments.count > 2 {
    fputs("Usage:", stderr)
    fputs("    swiftlox [filename]\n", stderr)
    exit(64)
} else if CommandLine.arguments.count == 2 {
    let filePath = CommandLine.arguments[1]
    Interpreter.runFile(path: filePath)
} else {
    Interpreter.runPrompt()
}