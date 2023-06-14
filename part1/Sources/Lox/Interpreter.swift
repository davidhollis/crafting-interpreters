import Darwin
import Foundation

class Interpreter {
    // TODO
}

// MARK: - running code

extension Interpreter {
    public static func run(string line: String) {
        // TODO
    }

    public static func runFile(path: String) {
        let scriptContents: String
        do {
            scriptContents = try String(contentsOfFile: path, encoding: .utf8)
        } catch {
            fputs("Failed to read \(path):\n    \(error)\n", stderr)
            exit(1)
        }

        self.run(string: scriptContents)
    }

    public static func runPrompt() {
        print("> ", terminator: "")
        while let line = readLine() {
            self.run(string: line)
            print("> ", terminator: "")
        }
    }
}