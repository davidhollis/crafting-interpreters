type t = {
  error_reporter : Errors.t;
  source : string;
  mutable current_line : int;
  mutable current_offset : int;
  mutable current_token_start : int;
}

let create error_reporter source =
  {
    error_reporter;
    source;
    current_line = 1;
    current_offset = 0;
    current_token_start = 0;
  }

let current_lexeme scanner =
  String.sub scanner.source scanner.current_token_start
    (scanner.current_offset - scanner.current_token_start)

let make_token scanner token_type =
  Token.create token_type (current_lexeme scanner) scanner.current_line

let is_at_end scanner = scanner.current_offset >= String.length scanner.source

let advance scanner =
  let current_char = scanner.source.[scanner.current_offset] in
  scanner.current_offset <- scanner.current_offset + 1;
  current_char

let match_next scanner expected_char =
  if is_at_end scanner then false
  else if scanner.source.[scanner.current_offset] != expected_char then false
  else (
    scanner.current_offset <- scanner.current_offset + 1;
    true)

let peek scanner =
  if is_at_end scanner then '\x00' else scanner.source.[scanner.current_offset]

let rec scan_next scanner =
  scanner.current_token_start <- scanner.current_offset;
  if is_at_end scanner then None
  else
    match advance scanner with
    | '(' -> Some (make_token scanner Token.LeftParen)
    | ')' -> Some (make_token scanner Token.RightParen)
    | '{' -> Some (make_token scanner Token.LeftBrace)
    | '}' -> Some (make_token scanner Token.RightBrace)
    | ',' -> Some (make_token scanner Token.Comma)
    | '.' -> Some (make_token scanner Token.Dot)
    | '-' -> Some (make_token scanner Token.Minus)
    | '+' -> Some (make_token scanner Token.Plus)
    | ';' -> Some (make_token scanner Token.Semicolon)
    | '*' -> Some (make_token scanner Token.Asterisk)
    | '!' ->
        if match_next scanner '=' then Some (make_token scanner Token.BangEqual)
        else Some (make_token scanner Token.Bang)
    | '=' ->
        if match_next scanner '=' then
          Some (make_token scanner Token.EqualEqual)
        else Some (make_token scanner Token.Equal)
    | '<' ->
        if match_next scanner '=' then Some (make_token scanner Token.LessEqual)
        else Some (make_token scanner Token.Less)
    | '>' ->
        if match_next scanner '=' then
          Some (make_token scanner Token.GreaterEqual)
        else Some (make_token scanner Token.Greater)
    | '/' ->
        if match_next scanner '/' then (
          while (not (is_at_end scanner)) && not ('\n' = peek scanner) do
            ignore (advance scanner)
          done;
          scan_next scanner)
        else Some (make_token scanner Token.Slash)
    | ' ' | '\r' | '\t' ->
        (* Ignore most whitespace... *)
        scan_next scanner
    | '\n' ->
        (* ... except newlines, which cause us to increment the line counter *)
        scanner.current_line <- scanner.current_line + 1;
        scan_next scanner
    | c ->
        Errors.report scanner.error_reporter scanner.current_line ""
          (Printf.sprintf "Unexpected character %c" c);
        None

let scan_all scanner : Token.t list =
  let rec consume tokens =
    match scan_next scanner with
    | Some token -> consume (token :: tokens)
    | None -> Token.eof (scanner.current_line + 1) :: tokens
  in
  List.rev (consume [])
