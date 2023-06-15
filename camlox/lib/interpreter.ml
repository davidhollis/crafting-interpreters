open Base
open Stdio

type state = { mutable had_error : bool }

let empty_state () = { had_error = false }
let clear_error state = state.had_error <- false

let run _state s =
  let scanner = Scanner.create s in
  Scanner.scan_all scanner
  |> List.iter ~f:(Fn.compose print_endline Token.describe)

let run_file path =
  let interpreter_state = empty_state () in
  let script_contents = In_channel.with_file path ~f:In_channel.input_all in
  run interpreter_state script_contents;
  not interpreter_state.had_error

let get_line prompt =
  print_string prompt;
  Out_channel.flush stdout;
  In_channel.input_line stdin

let rec run_prompt ?(interpreter_state = empty_state ()) () =
  match get_line "> " with
  | Some line ->
      run interpreter_state line;
      clear_error interpreter_state;
      run_prompt ~interpreter_state ()
  | None -> print_endline "\nBye!"

let report state line where message =
  prerr_endline (Printf.sprintf "[line %d] Error %s: %s" line where message);
  state.had_error <- true
