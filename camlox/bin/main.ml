open Camlox;;

match Sys.argv with
| [| _ |] -> Interpreter.run_prompt ()
| [| _; script_file |] ->
    let result = Interpreter.run_file script_file in
    if result.had_error then exit 65
    else if result.had_runtime_error then exit 70
    else exit 0
| _ ->
    print_endline "Usage:";
    print_endline "  camlox [filename]";
    exit 64
