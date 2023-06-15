open Camlox;;

match Sys.argv with
| [| _ |] -> Interpreter.run_prompt ()
| [| _; script_file |] -> Interpreter.run_file script_file
| _ ->
    print_endline "Usage:";
    print_endline "  camlox [filename]";
    exit 64
