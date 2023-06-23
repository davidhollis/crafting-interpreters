open Base
open Result
open Ast

type t = {
  error_reporter : Errors.t;
  mutable tokens : Token.t array;
  mutable current_token : int;
  mutable should_fail : bool;
}

let create error_reporter =
  { error_reporter; tokens = [||]; current_token = 0; should_fail = false }

let reset parser tokens =
  parser.tokens <- Array.of_list tokens;
  parser.current_token <- 0;
  parser.should_fail <- false

let peek parser = parser.tokens.(parser.current_token)
let previous parser = parser.tokens.(parser.current_token - 1)
let is_at_end parser = Token.has_type (peek parser) Token.EOF

let parse_error parser message =
  (match peek parser with
  | { tpe = Token.EOF; line; _ } ->
      Errors.report parser.error_reporter line "at end" message
  | { line; _ } as tok ->
      Errors.report parser.error_reporter line
        (Printf.sprintf "at '%s'" (Token.describe tok))
        message);
  fail `ParseError

let advance parser =
  if not (is_at_end parser) then
    parser.current_token <- parser.current_token + 1;
  previous parser

let check parser token_type =
  if is_at_end parser then false else Token.has_type (peek parser) token_type

let consume parser token_type message =
  if check parser token_type then return (ignore (advance parser))
  else parse_error parser message

let rec synchronize parser =
  let previous_token =
    if parser.current_token > 0 then previous parser
    else Token.create Token.Nil "nil" (-1)
  and current_token = peek parser in
  let is_synchronized =
    is_at_end parser
    || Token.has_type previous_token Token.Semicolon
    || Token.has_type current_token Token.Class
    || Token.has_type current_token Token.For
    || Token.has_type current_token Token.Fun
    || Token.has_type current_token Token.If
    || Token.has_type current_token Token.Print
    || Token.has_type current_token Token.Return
    || Token.has_type current_token Token.Var
    || Token.has_type current_token Token.While
  in
  if is_synchronized then ()
  else (
    ignore (advance parser);
    synchronize parser)

let rec match_any parser = function
  | next_token :: rest_of_tokens ->
      if check parser next_token then (
        ignore (advance parser);
        true)
      else match_any parser rest_of_tokens
  | [] -> false

let parse_simple_binary token_types ~next parser =
  let rec parse_branch expr_so_far =
    if match_any parser token_types then
      let op = previous parser in
      next parser
      |> bind ~f:(fun right ->
             parse_branch (Expr.Binary (expr_so_far, op, right)))
    else return expr_so_far
  in
  next parser |> bind ~f:(fun left -> parse_branch left)

let rec parse_expression parser = parse_equality parser

and parse_equality parser =
  parse_simple_binary
    [ Token.BangEqual; Token.EqualEqual ]
    ~next:parse_comparison parser

and parse_comparison parser =
  parse_simple_binary
    [ Token.Less; Token.LessEqual; Token.Greater; Token.GreaterEqual ]
    ~next:parse_term parser

and parse_term parser =
  parse_simple_binary [ Token.Plus; Token.Minus ] ~next:parse_factor parser

and parse_factor parser =
  parse_simple_binary [ Token.Asterisk; Token.Slash ] ~next:parse_unary parser

and parse_unary parser =
  if match_any parser [ Token.Bang; Token.Minus ] then
    let op = previous parser in
    parse_unary parser |> map ~f:(fun subexpr -> Expr.Unary (op, subexpr))
  else parse_primary parser

and parse_primary parser =
  if
    match_any parser
      [
        Token.True;
        Token.False;
        Token.Nil;
        Token.String "";
        Token.Number 0.;
        Token.Identifier;
      ]
  then return (Expr.Literal (previous parser))
  else if match_any parser [ Token.LeftParen ] then
    parse_expression parser
    |> bind ~f:(fun inner ->
           consume parser Token.RightParen
             "expected closing paren ')' after expression"
           |> map ~f:(fun () -> Expr.Grouping inner))
  else parse_error parser "expected some kind of expression"

let rec parse_program parser =
  let rec parse_statement_sequence stmts =
    if is_at_end parser then
      if parser.should_fail then fail `ParseError else return (List.rev stmts)
    else
      parse_declaration parser |> function
      | Ok next_stmt -> parse_statement_sequence (next_stmt :: stmts)
      | Error `ContinueParsing -> parse_statement_sequence stmts
      | Error `ParseError as other_error -> other_error
  in
  parse_statement_sequence []

and parse_declaration parser =
  (if match_any parser [ Token.Var ] then parse_var_decl parser
   else parse_statement parser)
  |> function
  | Ok _ as stmt -> stmt
  | Error `ParseError ->
      synchronize parser;
      parser.should_fail <- true;
      fail `ContinueParsing

and parse_var_decl parser =
  (match peek parser with
  | { tpe = Token.Identifier; _ } as var_name ->
      ignore (advance parser);
      if match_any parser [ Token.Equal ] then
        parse_expression parser >>| fun expr -> Stmt.Var (var_name, Some expr)
      else return (Stmt.Var (var_name, None))
  | _ -> parse_error parser "expected identifier after 'var'")
  >>= fun var_stmt ->
  consume parser Token.Semicolon
    "expected semicolon at the end of a var statement"
  >>| fun () -> var_stmt

and parse_statement parser =
  if match_any parser [ Token.Print ] then parse_print_stmt parser
  else parse_expr_stmt parser

and parse_print_stmt parser =
  parse_expression parser >>= fun expr ->
  consume parser Token.Semicolon
    "expected semicolon at the end of a print statement"
  >>= fun () -> return (Stmt.Print expr)

and parse_expr_stmt parser =
  parse_expression parser >>= fun expr ->
  consume parser Token.Semicolon
    "expected semicolon at the end of an expression statement"
  >>= fun () -> return (Stmt.Expression expr)

let parse parser tokens =
  reset parser tokens;
  parse_program parser
