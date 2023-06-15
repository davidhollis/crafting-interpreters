open Base

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
  String.sub scanner.source ~pos:scanner.current_token_start
    ~len:(scanner.current_offset - scanner.current_token_start)

let make_token scanner token_type =
  Token.create token_type (current_lexeme scanner) scanner.current_line

let is_at_end scanner = scanner.current_offset >= String.length scanner.source

let advance scanner =
  let current_char = scanner.source.[scanner.current_offset] in
  scanner.current_offset <- scanner.current_offset + 1;
  current_char

let match_next scanner expected_char =
  let open Char in
  if is_at_end scanner then false
  else if scanner.source.[scanner.current_offset] = expected_char then (
    scanner.current_offset <- scanner.current_offset + 1;
    true)
  else false

let peek scanner =
  if is_at_end scanner then '\x00' else scanner.source.[scanner.current_offset]

let peek_next scanner =
  if scanner.current_offset + 1 >= String.length scanner.source then '\x00'
  else scanner.source.[scanner.current_offset + 1]

let consume_until scanner end_char =
  let open Char in
  while (not (is_at_end scanner)) && not (end_char = peek scanner) do
    if '\n' = peek scanner then scanner.current_line <- scanner.current_line + 1;
    ignore (advance scanner)
  done

let is_digit c =
  let open Char in
  '0' <= c && c <= '9'

let is_alpha c =
  let open Char in
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

let is_alnum c = is_alpha c || is_digit c

let scan_string scanner =
  consume_until scanner '"';
  if is_at_end scanner then (
    Errors.report scanner.error_reporter scanner.current_line ""
      "Hit EOF while scanning string";
    None)
  else (
    ignore (advance scanner);
    let string_value =
      String.sub scanner.source
        ~pos:(scanner.current_token_start + 1)
        ~len:(scanner.current_offset - scanner.current_token_start - 2)
    in
    Some (make_token scanner (Token.String string_value)))

let scan_number scanner =
  let open Char in
  while is_digit (peek scanner) do
    ignore (advance scanner)
  done;
  if '.' = peek scanner && is_digit (peek_next scanner) then (
    ignore (advance scanner);
    while is_digit (peek scanner) do
      ignore (advance scanner)
    done);
  Float.of_string_opt (current_lexeme scanner)
  |> Option.map ~f:(fun value -> make_token scanner (Token.Number value))

let scan_identifier scanner =
  while is_alnum (peek scanner) do
    ignore (advance scanner)
  done;
  Some (make_token scanner (Token.type_of_word (current_lexeme scanner)))

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
          consume_until scanner '\n';
          scan_next scanner)
        else Some (make_token scanner Token.Slash)
    | ' ' | '\r' | '\t' ->
        (* Ignore most whitespace... *)
        scan_next scanner
    | '\n' ->
        (* ... except newlines, which cause us to increment the line counter *)
        scanner.current_line <- scanner.current_line + 1;
        scan_next scanner
    | '"' -> (
        match scan_string scanner with
        | Some _ as token -> token
        | None -> scan_next scanner)
    | c when is_digit c -> (
        match scan_number scanner with
        | Some _ as token -> token
        | None -> scan_next scanner)
    | c when is_alpha c -> scan_identifier scanner
    | c ->
        Errors.report scanner.error_reporter scanner.current_line ""
          (Printf.sprintf "Unexpected character %c" c);
        scan_next scanner

let scan_all scanner : Token.t list =
  let rec consume tokens =
    match scan_next scanner with
    | Some token -> consume (token :: tokens)
    | None -> Token.eof (scanner.current_line + 1) :: tokens
  in
  List.rev (consume [])
