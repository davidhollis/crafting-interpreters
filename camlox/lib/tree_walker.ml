open Base
open Result
open Ast

type t = { error_reporter : Errors.t; mutable environment : Env.t }

let create error_reporter =
  { error_reporter; environment = Env.create error_reporter }

let runtime_error tree_walker line message =
  Errors.report ~runtime:true tree_walker.error_reporter line
    "in `Tree_walker.evaluate'" message;
  fail `RuntimeError

let declare_var tree_walker name_token value =
  Env.declare tree_walker.environment name_token value

let assign_var tree_walker name_token value =
  Env.assign tree_walker.environment name_token value

let get_var tree_walker name_token = Env.get tree_walker.environment name_token

let rec evaluate_expr tree_walker = function
  | Expr.Literal { tpe = Token.True; _ } -> return (Value.Boolean true)
  | Expr.Literal { tpe = Token.False; _ } -> return (Value.Boolean false)
  | Expr.Literal { tpe = Token.Nil; _ } -> return Value.Nil
  | Expr.Literal { tpe = Token.Number num; _ } -> return (Value.Number num)
  | Expr.Literal { tpe = Token.String str; _ } -> return (Value.String str)
  | Expr.Literal ({ tpe = Token.Identifier; _ } as name_token) ->
      get_var tree_walker name_token
  | Expr.Grouping subexpr -> evaluate_expr tree_walker subexpr
  | Expr.Unary (({ tpe = op; line; _ } as op_token), right_expr) -> (
      evaluate_expr tree_walker right_expr >>= fun right ->
      match (op, right) with
      | Token.Minus, Value.Number n -> return (Value.Number (-.n))
      | Token.Bang, _ -> return (Value.Boolean (not (Value.is_truthy right)))
      | Token.Minus, _ ->
          runtime_error tree_walker line
            (Printf.sprintf "Cannot apply unary operator %s to value %s"
               (Token.print op_token) (Value.show right))
      | _ ->
          runtime_error tree_walker line
            ("Invalid unary operator " ^ Token.print op_token))
  | Expr.Binary (left_expr, ({ tpe = op; line; _ } as op_token), right_expr)
    -> (
      evaluate_expr tree_walker left_expr >>= fun left ->
      evaluate_expr tree_walker right_expr >>= fun right ->
      match (op, left, right) with
      | Token.Plus, Value.Number ln, Value.Number rn ->
          return (Value.Number (ln +. rn))
      | Token.Plus, Value.String ls, Value.String rs ->
          return (Value.String (ls ^ rs))
      | Token.Minus, Value.Number ln, Value.Number rn ->
          return (Value.Number (ln -. rn))
      | Token.Asterisk, Value.Number ln, Value.Number rn ->
          return (Value.Number (ln *. rn))
      | Token.Slash, Value.Number ln, Value.Number rn ->
          return (Value.Number (ln /. rn))
      | Token.Greater, Value.Number ln, Value.Number rn ->
          let open Float in
          return (Value.Boolean (ln > rn))
      | Token.GreaterEqual, Value.Number ln, Value.Number rn ->
          let open Float in
          return (Value.Boolean (ln >= rn))
      | Token.Less, Value.Number ln, Value.Number rn ->
          let open Float in
          return (Value.Boolean (ln < rn))
      | Token.LessEqual, Value.Number ln, Value.Number rn ->
          let open Float in
          return (Value.Boolean (ln <= rn))
      | Token.EqualEqual, _, _ ->
          return (Value.Boolean (Value.equal left right))
      | Token.BangEqual, _, _ ->
          return (Value.Boolean (not (Value.equal left right)))
      | Token.Plus, _, _
      | Token.Minus, _, _
      | Token.Asterisk, _, _
      | Token.Slash, _, _
      | Token.Greater, _, _
      | Token.GreaterEqual, _, _
      | Token.Less, _, _
      | Token.LessEqual, _, _ ->
          runtime_error tree_walker line
            (Printf.sprintf "Cannot apply binary operator %s to values %s, %s"
               (Token.print op_token) (Value.show left) (Value.show right))
      | _ ->
          runtime_error tree_walker line
            ("Invalid binary operator " ^ Token.print op_token))
  | Expr.Assign (name_tok, body) ->
      evaluate_expr tree_walker body >>= assign_var tree_walker name_tok
  | unknown_form ->
      runtime_error tree_walker (-1)
        ("Unknown expression form: " ^ Printer.print_sexpr unknown_form)

let rec run_program tree_walker =
  List.fold ~init:(return ()) ~f:(fun r stmt ->
      r >>= fun () -> execute_stmt tree_walker stmt)

and execute_stmt tree_walker = function
  | Stmt.Expression expr -> evaluate_expr tree_walker expr >>| ignore
  | Stmt.Print expr ->
      evaluate_expr tree_walker expr >>| fun value ->
      Stdio.print_endline (Value.to_string value)
  | Stmt.Var (name, None) -> return (declare_var tree_walker name Value.Nil)
  | Stmt.Var (name, Some init_expr) ->
      evaluate_expr tree_walker init_expr >>| declare_var tree_walker name
  | Stmt.Block stmts ->
      execute_block tree_walker stmts (Env.create_from tree_walker.environment)

and execute_block tree_walker stmts env =
  let previous = tree_walker.environment in
  tree_walker.environment <- env;
  let result = run_program tree_walker stmts in
  tree_walker.environment <- previous;
  result

let run_repl tree_walker = function
  | Either.First stmts -> run_program tree_walker stmts >>| fun _ -> Value.Nil
  | Either.Second expr -> evaluate_expr tree_walker expr
