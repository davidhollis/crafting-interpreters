open Base
open Option
open Ast

type t = { error_reporter : Errors.t }

let create error_reporter = { error_reporter }

let runtime_error tree_walker line =
  Errors.report ~runtime:true tree_walker.error_reporter line
    "in `Tree_walker.evaluate'"

let rec evaluate_expr tree_walker = function
  | Expr.Literal { tpe = Token.True; _ } -> Some (Value.Boolean true)
  | Expr.Literal { tpe = Token.False; _ } -> Some (Value.Boolean false)
  | Expr.Literal { tpe = Token.Nil; _ } -> Some Value.Nil
  | Expr.Literal { tpe = Token.Number num; _ } -> Some (Value.Number num)
  | Expr.Literal { tpe = Token.String str; _ } -> Some (Value.String str)
  | Expr.Grouping subexpr -> evaluate_expr tree_walker subexpr
  | Expr.Unary (({ tpe = op; line; _ } as op_token), right_expr) -> (
      evaluate_expr tree_walker right_expr >>= fun right ->
      match (op, right) with
      | Token.Minus, Value.Number n -> Some (Value.Number (-.n))
      | Token.Bang, _ -> Some (Value.Boolean (not (Value.is_truthy right)))
      | Token.Minus, _ ->
          runtime_error tree_walker line
            (Printf.sprintf "Cannot apply unary operator %s to value %s"
               (Token.print op_token) (Value.show right));
          None
      | _ ->
          runtime_error tree_walker line
            ("Invalid unary operator " ^ Token.print op_token);
          None)
  | Expr.Binary (left_expr, ({ tpe = op; line; _ } as op_token), right_expr)
    -> (
      evaluate_expr tree_walker left_expr >>= fun left ->
      evaluate_expr tree_walker right_expr >>= fun right ->
      match (op, left, right) with
      | Token.Plus, Value.Number ln, Value.Number rn ->
          Some (Value.Number (ln +. rn))
      | Token.Plus, Value.String ls, Value.String rs ->
          Some (Value.String (ls ^ rs))
      | Token.Minus, Value.Number ln, Value.Number rn ->
          Some (Value.Number (ln -. rn))
      | Token.Asterisk, Value.Number ln, Value.Number rn ->
          Some (Value.Number (ln *. rn))
      | Token.Slash, Value.Number ln, Value.Number rn ->
          Some (Value.Number (ln /. rn))
      | Token.Greater, Value.Number ln, Value.Number rn ->
          let open Float in
          Some (Value.Boolean (ln > rn))
      | Token.GreaterEqual, Value.Number ln, Value.Number rn ->
          let open Float in
          Some (Value.Boolean (ln >= rn))
      | Token.Less, Value.Number ln, Value.Number rn ->
          let open Float in
          Some (Value.Boolean (ln < rn))
      | Token.LessEqual, Value.Number ln, Value.Number rn ->
          let open Float in
          Some (Value.Boolean (ln <= rn))
      | Token.EqualEqual, _, _ -> Some (Value.Boolean (Value.equal left right))
      | Token.BangEqual, _, _ ->
          Some (Value.Boolean (not (Value.equal left right)))
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
               (Token.print op_token) (Value.show left) (Value.show right));
          None
      | _ ->
          runtime_error tree_walker line
            ("Invalid binary operator " ^ Token.print op_token);
          None)
  | unknown_form ->
      Errors.report ~runtime:true tree_walker.error_reporter (-1) "evaluate"
        ("Unknown expression form: " ^ Printer.print_sexpr unknown_form);
      None
