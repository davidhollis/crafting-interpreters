open Base
open Result
open Ast

type t = {
  mutable error_reporter : Errors.t;
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

let parse_error ?at_token parser message =
  (match Option.value at_token ~default:(peek parser) with
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
      next parser >>= fun right ->
      parse_branch (Expr.Binary (expr_so_far, op, right))
    else return expr_so_far
  in
  next parser >>= parse_branch

let rec parse_expression parser = parse_assignment parser

and parse_assignment parser =
  parse_logical_or parser >>= fun lhs ->
  if match_any parser [ Token.Equal ] then
    let equals_token = previous parser in
    parse_assignment parser >>= fun body ->
    match lhs with
    | Expr.Literal ({ tpe = Token.Identifier; _ } as name_token) ->
        return (Expr.Assign (name_token, body))
    | _ ->
        parse_error ~at_token:equals_token parser
          (Printf.sprintf "invalid lefthand side of assignment expression: %s"
             (Printer.print_sexpr lhs))
  else return lhs

and parse_logical_or parser =
  parse_simple_binary [ Token.Or ] ~next:parse_logical_and parser

and parse_logical_and parser =
  parse_simple_binary [ Token.And ] ~next:parse_equality parser

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
    parse_unary parser >>| fun subexpr -> Expr.Unary (op, subexpr)
  else parse_call parser

and parse_call parser =
  let rec build_arg_lists callee =
    if match_any parser [ Token.LeftParen ] then
      let paren_token = previous parser in
      parse_arguments parser >>= fun arg_list ->
      build_arg_lists (Expr.Call (callee, paren_token, arg_list))
    else return callee
  in
  parse_primary parser >>= build_arg_lists

and parse_arguments parser =
  (if check parser Token.RightParen then return []
   else
     let rec parse_next_arg arg_list =
       if List.length arg_list >= 255 then
         parse_error parser
           "Cannot have more than 255 arguments in a single argument list"
       else if match_any parser [ Token.Comma ] then
         parse_expression parser >>= fun next_arg ->
         parse_next_arg (next_arg :: arg_list)
       else return arg_list
     in
     parse_expression parser >>| List.return >>= parse_next_arg >>| List.rev)
  >>= fun arg_list ->
  consume parser Token.RightParen
    "expected closing paren ')' after argument list"
  >>| fun () -> arg_list

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
    parse_expression parser >>= fun inner ->
    consume parser Token.RightParen
      "expected closing paren ')' after expression"
    >>| fun () -> Expr.Grouping inner
  else parse_error parser "expected some kind of expression"

let rec parse_program parser =
  let rec parse_statement_sequence stmts =
    if is_at_end parser then
      if parser.should_fail then fail `ParseError else return (List.rev stmts)
    else
      parse_declaration parser |> function
      | Ok next_stmt -> parse_statement_sequence (next_stmt :: stmts)
      | Error `ContinueParsing -> parse_statement_sequence stmts
      | Error _ as other_error -> other_error
  in
  parse_statement_sequence []

and parse_declaration parser =
  (if match_any parser [ Token.Var ] then parse_var_decl parser
   else parse_statement parser)
  |> function
  | Error `ParseError ->
      ignore (advance parser);
      synchronize parser;
      parser.should_fail <- true;
      fail `ContinueParsing
  | stmt_result -> stmt_result

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
  if match_any parser [ Token.For ] then parse_for_stmt parser
  else if match_any parser [ Token.If ] then parse_if_stmt parser
  else if match_any parser [ Token.Print ] then parse_print_stmt parser
  else if match_any parser [ Token.While ] then parse_while_stmt parser
  else if match_any parser [ Token.LeftBrace ] then parse_block parser
  else parse_expr_stmt parser

and parse_for_stmt parser =
  consume parser Token.LeftParen "expected '(' after 'for'" >>= fun () ->
  (if match_any parser [ Token.Semicolon ] then return None
   else if match_any parser [ Token.Var ] then
     parse_var_decl parser >>| Option.return
   else parse_expr_stmt parser >>| Option.return)
  >>= fun maybe_initializer_stmt ->
  (if check parser Token.Semicolon then return None
   else parse_expression parser >>| Option.return)
  >>= fun maybe_condition_expr ->
  consume parser Token.Semicolon "expected ';' after for condition"
  >>= fun () ->
  (if check parser Token.RightParen then return None
   else parse_expression parser >>| Option.return)
  >>= fun maybe_increment_expr ->
  consume parser Token.RightParen "expected ')' after for clauses" >>= fun () ->
  parse_statement parser >>= fun body ->
  let loop_stmt =
    Stmt.While
      ( (match maybe_condition_expr with
        | Some condition_expr -> condition_expr
        | None -> Expr.Literal (Token.create Token.True "true" (-1))),
        match maybe_increment_expr with
        | Some increment_expr ->
            Stmt.Block [ body; Stmt.Expression increment_expr ]
        | None -> body )
  in
  match maybe_initializer_stmt with
  | Some initializer_stmt -> return (Stmt.Block [ initializer_stmt; loop_stmt ])
  | None -> return loop_stmt

and parse_if_stmt parser =
  consume parser Token.LeftParen "expected '(' after 'if'" >>= fun () ->
  parse_expression parser >>= fun cond_expr ->
  consume parser Token.RightParen "expected ')' after condition" >>= fun () ->
  parse_statement parser >>= fun then_stmt ->
  if match_any parser [ Token.Else ] then
    parse_statement parser >>= fun else_stmt ->
    return (Stmt.If (cond_expr, then_stmt, Some else_stmt))
  else return (Stmt.If (cond_expr, then_stmt, None))

and parse_print_stmt parser =
  parse_expression parser >>= fun expr ->
  consume parser Token.Semicolon
    "expected semicolon at the end of a print statement"
  >>= fun () -> return (Stmt.Print expr)

and parse_while_stmt parser =
  consume parser Token.LeftParen "expected '(' after 'while'" >>= fun () ->
  parse_expression parser >>= fun cond_expr ->
  consume parser Token.RightParen "expected ')' after condition" >>= fun () ->
  parse_statement parser >>= fun body_stmt ->
  return (Stmt.While (cond_expr, body_stmt))

and parse_block parser =
  let rec parse_decl_sequence stmts =
    if is_at_end parser then
      parse_error parser "expected right brace at end of block"
    else if match_any parser [ Token.RightBrace ] then
      return (Stmt.Block (List.rev stmts))
    else
      parse_declaration parser >>= fun next_stmt ->
      parse_decl_sequence (next_stmt :: stmts)
  in
  parse_decl_sequence []

and parse_expr_stmt parser =
  parse_expression parser >>= fun expr ->
  consume parser Token.Semicolon
    "expected semicolon at the end of an expression statement"
  >>= fun () -> return (Stmt.Expression expr)

let parse parser tokens =
  reset parser tokens;
  parse_program parser

let parse_repl parser tokens =
  reset parser tokens;
  let original_error_reporter = parser.error_reporter in
  parser.error_reporter <- Errors.create_from parser.error_reporter;
  match parse_program parser with
  | Ok stmts ->
      parser.error_reporter <- original_error_reporter;
      return (Either.First stmts)
  | Error _ -> (
      reset parser tokens;
      parser.error_reporter <- Errors.create_from original_error_reporter;
      match parse_expression parser with
      | Ok expr ->
          parser.error_reporter <- original_error_reporter;
          return (Either.Second expr)
      | Error _ ->
          Errors.commit parser.error_reporter;
          parser.error_reporter <- original_error_reporter;
          fail `ParseError)
