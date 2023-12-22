# camlox — lox in OCaml

This is a handwritten Lox interpreter in the style of Part 1 of _Crafting Interpreters_, but written in OCaml. It's feature-complete, but only barely.

To run a script file: `dune exec camlox -- path/to/script.lox`

To start a REPL: `dune exec camlox`

I'm probably done with this now, as I'm interested in moving on to Part 2, but if I ever do revisit it, the next few things I'd do are, in no particular order:

  * Clean up the default display for numbers
  * Add readline support to the REPL
  * Plumb token offsets through and use that to improve error messages
      * And while I'm at it, make the error message format more uniform
  * Add function and class literals to the expression grammar, e.g.:
      * `var double = fun (x) { return x + x; }`
          * maybe even an expression form `fun (x) { x + x }` that desugars to the above
      * `var Subclass = class < Superclass { method_a() { ... } }`
  * Modify the signatures of a bunch of functions, especially in the resolver, to eliminate a bunch of `fun () ->` noise in `Result.>>=` chains
  * Add a bunch of comments to the more complex match branches
  * Add explicit cases for invalid `Expr.Literal` forms to `Tree_walker.evaluate_expr` and remove the `_` branch so the compiler can actually detect non-exhaustive matches when the expression grammar changes
