open Base

type t = { mutable had_error : bool; mutable had_runtime_error : bool }

let create () = { had_error = false; had_runtime_error = false }

let clear reporter =
  reporter.had_error <- false;
  reporter.had_runtime_error <- false

let is_clean reporter =
  (not reporter.had_error) && not reporter.had_runtime_error

let report reporter ?(runtime = false) line where message =
  Stdio.prerr_endline
    (Printf.sprintf "[line %d] Error %s: %s" line where message);
  if runtime then reporter.had_runtime_error <- true
  else reporter.had_error <- true
