let run s = print_endline ("running script:\n" ^ s)

let run_file path =
  let script_contents = In_channel.with_open_text path In_channel.input_all in
  run script_contents

let get_line prompt =
  print_string prompt;
  Out_channel.flush stdout;
  In_channel.input_line stdin

let rec run_prompt () =
  match get_line "> " with
  | Some line ->
      run line;
      run_prompt ()
  | None -> print_endline "\nBye!"
