open Base
open Stdio

type state = { error_reporter : Errors.t; interpreter : Tree_walker.t }

let empty_state () =
  let error_reporter = Errors.create () in
  { error_reporter; interpreter = Tree_walker.create error_reporter }

let run state s =
  let open Option in
  let scanner = Scanner.create state.error_reporter s in
  let parser = Parser.create state.error_reporter (Scanner.scan_all scanner) in
  match Parser.parse parser >>= Tree_walker.evaluate_expr state.interpreter with
  | Some value -> print_endline ("==> " ^ Ast.Value.show value)
  | None -> prerr_endline "Encountered one or more errors."

let run_file path =
  let interpreter_state = empty_state () in
  let script_contents = In_channel.with_file path ~f:In_channel.input_all in
  run interpreter_state script_contents;
  interpreter_state.error_reporter

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
