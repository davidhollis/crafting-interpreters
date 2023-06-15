open Base
open Stdio

type state = { error_reporter : Errors.t }

let empty_state () = { error_reporter = Errors.create () }

let run state s =
  let scanner = Scanner.create state.error_reporter s in
  Scanner.scan_all scanner
  |> List.iter ~f:(Fn.compose print_endline Token.describe)

let run_file path =
  let interpreter_state = empty_state () in
  let script_contents = In_channel.with_file path ~f:In_channel.input_all in
  run interpreter_state script_contents;
  Errors.is_clean interpreter_state.error_reporter

let get_line prompt =
  print_string prompt;
  Out_channel.flush stdout;
  In_channel.input_line stdin

let rec run_prompt ?(interpreter_state = empty_state ()) () =
  match get_line "> " with
  | Some line ->
      run interpreter_state line;
      Errors.clear interpreter_state.error_reporter;
      run_prompt ~interpreter_state ()
  | None -> print_endline "\nBye!"
