open Base
open Result

type t = {
  error_reporter : Errors.t;
  mutable source : string;
  mutable current_line : int;
  mutable current_offset : int;
  mutable current_token_start : int;
}

let create error_reporter =
  {
    error_reporter;
    source = "";
    current_line = 1;
    current_offset = 0;
    current_token_start = 0;
  }

let reset scanner source =
  scanner.source <- source;
  scanner.current_line <- 1;
  scanner.current_offset <- 0;
  scanner.current_token_start <- 0

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

let lex_error scanner message =
  Errors.report scanner.error_reporter scanner.current_line "" message;
  fail `LexError

let scan_string scanner =
  consume_until scanner '"';
  if is_at_end scanner then lex_error scanner "Hit EOF while scanning string"
  else (
    ignore (advance scanner);
    let string_value =
      String.sub scanner.source
        ~pos:(scanner.current_token_start + 1)
        ~len:(scanner.current_offset - scanner.current_token_start - 2)
    in
    return (make_token scanner (Token.String string_value)))

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
  match Float.of_string_opt (current_lexeme scanner) with
  | Some f -> return (make_token scanner (Token.Number f))
  | None ->
      lex_error scanner
        (Printf.sprintf "malformed float literal '%s'" (current_lexeme scanner))

let scan_identifier scanner =
  while is_alnum (peek scanner) do
    ignore (advance scanner)
  done;
  return (make_token scanner (Token.type_of_word (current_lexeme scanner)))

let rec scan_next scanner =
  scanner.current_token_start <- scanner.current_offset;
  if is_at_end scanner then fail `EOF
  else
    match advance scanner with
    | '(' -> return (make_token scanner Token.LeftParen)
    | ')' -> return (make_token scanner Token.RightParen)
    | '{' -> return (make_token scanner Token.LeftBrace)
    | '}' -> return (make_token scanner Token.RightBrace)
    | ',' -> return (make_token scanner Token.Comma)
    | '.' -> return (make_token scanner Token.Dot)
    | '-' -> return (make_token scanner Token.Minus)
    | '+' -> return (make_token scanner Token.Plus)
    | ';' -> return (make_token scanner Token.Semicolon)
    | '*' -> return (make_token scanner Token.Asterisk)
    | '!' ->
        if match_next scanner '=' then
          return (make_token scanner Token.BangEqual)
        else return (make_token scanner Token.Bang)
    | '=' ->
        if match_next scanner '=' then
          return (make_token scanner Token.EqualEqual)
        else return (make_token scanner Token.Equal)
    | '<' ->
        if match_next scanner '=' then
          return (make_token scanner Token.LessEqual)
        else return (make_token scanner Token.Less)
    | '>' ->
        if match_next scanner '=' then
          return (make_token scanner Token.GreaterEqual)
        else return (make_token scanner Token.Greater)
    | '/' ->
        if match_next scanner '/' then (
          consume_until scanner '\n';
          scan_next scanner)
        else return (make_token scanner Token.Slash)
    | ' ' | '\r' | '\t' ->
        (* Ignore most whitespace... *)
        scan_next scanner
    | '\n' ->
        (* ... except newlines, which cause us to increment the line counter *)
        scanner.current_line <- scanner.current_line + 1;
        scan_next scanner
    | '"' -> scan_string scanner
    | c when is_digit c -> scan_number scanner
    | c when is_alpha c -> scan_identifier scanner
    | c -> lex_error scanner (Printf.sprintf "Unexpected character %c" c)

let scan_all scanner source =
  reset scanner source;
  let rec consume tokens =
    match scan_next scanner with
    | Ok token -> consume (token :: tokens)
    | Error `EOF -> return (Token.eof (scanner.current_line + 1) :: tokens)
    | Error `LexError -> fail `LexError
  in
  consume [] >>| List.rev
