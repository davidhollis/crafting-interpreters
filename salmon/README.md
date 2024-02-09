# Salmon — lox in a lower level language (Rust)

This is a lox compiler and bytecode interpreter in the style of Part 2 of _Crafting Interpreters_, but written in Rust. It's as complete as it's going to get for the moment, and I'm pretty proud of the ergonomics given that this was just an educational project. Try writing some incorrect code and checking out the error handling!

To run a script file: `cargo run -- path/to/script.lox`

To compile a script file and list the bytecode in a human-readable form: `cargo run -- --dump-bytecode path/to/script.lox`

To start a REPL: `cargo run`  
Note: the REPL has readline-like line editing, and hitting return twice will clear the current cell without executing it.

To see other options: `cargo run -- --help`
