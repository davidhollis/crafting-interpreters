open Camlox;;

match Sys.argv with
| [| _ |] -> Interpreter.run_prompt ()
| [| _; script_file |] ->
    if Interpreter.run_file script_file then exit 0 else exit 65
| _ ->
    print_endline "Usage:";
    print_endline "  camlox [filename]";
    exit 64
