open Base

type t = { mutable had_error : bool }

let create () = { had_error = false }
let clear reporter = reporter.had_error <- false
let is_clean reporter = not reporter.had_error

let report reporter line where message =
  Stdio.prerr_endline
    (Printf.sprintf "[line %d] Error %s: %s" line where message);
  reporter.had_error <- true
