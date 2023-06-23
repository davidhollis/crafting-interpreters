open Base

type t = {
  mutable had_error : bool;
  mutable had_runtime_error : bool;
  mutable parent : t option;
  mutable error_buffer : string list;
}

let create () =
  {
    had_error = false;
    had_runtime_error = false;
    parent = None;
    error_buffer = [];
  }

let create_from parent =
  {
    had_error = false;
    had_runtime_error = false;
    parent = Some parent;
    error_buffer = [];
  }

let clear reporter =
  reporter.had_error <- false;
  reporter.had_runtime_error <- false;
  reporter.error_buffer <- []

let is_clean reporter =
  (not reporter.had_error) && not reporter.had_runtime_error

let handle_error_message reporter message =
  if Option.is_some reporter.parent then
    reporter.error_buffer <- message :: reporter.error_buffer
  else Stdio.prerr_endline message

let report reporter ?(runtime = false) line where message =
  handle_error_message reporter
    (Printf.sprintf "[line %d] Error %s: %s" line where message);
  if runtime then reporter.had_runtime_error <- true
  else reporter.had_error <- true

let commit reporter =
  match reporter.parent with
  | Some parent ->
      parent.had_error <- parent.had_error || reporter.had_error;
      parent.had_runtime_error <-
        parent.had_runtime_error || reporter.had_runtime_error;
      List.rev reporter.error_buffer
      |> List.iter ~f:(handle_error_message parent);
      clear reporter
  | None -> ()
