open Base
open Stdio
open Result

type state = {
  error_reporter : Errors.t;
  scanner : Scanner.t;
  parser : Parser.t;
  interpreter : Tree_walker.t;
}

let empty_state () =
  let error_reporter = Errors.create () in
  {
    error_reporter;
    scanner = Scanner.create error_reporter;
    parser = Parser.create error_reporter;
    interpreter = Tree_walker.create error_reporter;
  }

let run state program_str =
  program_str
  |> Scanner.scan_all state.scanner
  >>= Parser.parse state.parser
  >>= Tree_walker.run_program state.interpreter

let eval state expr_string =
  expr_string
  |> Scanner.scan_all state.scanner
  >>= Parser.parse_repl state.parser
  >>= Tree_walker.run_repl state.interpreter
  |> function
  | Ok Ast.Value.Nil -> ()
  | Ok value -> print_endline ("==> " ^ Ast.Value.to_string value)
  | Error `LexError -> prerr_endline "[!] Malformed input"
  | Error `ParseError -> prerr_endline "[!] Parse error"
  | Error `RuntimeError -> prerr_endline "[!] Runtime error"
  | Error `NotImplemented ->
      prerr_endline
        "[?] Used a language feature that hasn't been implemented yet"

let run_file path =
  let interpreter_state = empty_state () in
  let script_contents = In_channel.with_file path ~f:In_channel.input_all in
  ignore (run interpreter_state script_contents);
  interpreter_state.error_reporter

let get_line prompt =
  print_string prompt;
  Out_channel.flush stdout;
  In_channel.input_line stdin

let rec run_prompt ?(interpreter_state = empty_state ()) () =
  match get_line "camlox> " with
  | Some line ->
      eval interpreter_state line;
      Errors.clear interpreter_state.error_reporter;
      run_prompt ~interpreter_state ()
  | None -> print_endline "\nBye!"
