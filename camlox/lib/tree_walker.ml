open Base
open Option

type t = { error_reporter : Errors.t }

let create error_reporter = { error_reporter }

let runtime_error tree_walker line =
  Errors.report ~runtime:true tree_walker.error_reporter line
    "in `Tree_walker.evaluate'"

let rec evaluate tree_walker = function
  | Expr.Literal { tpe = Token.True; _ } -> Some (Expr.Value.Boolean true)
  | Expr.Literal { tpe = Token.False; _ } -> Some (Expr.Value.Boolean false)
  | Expr.Literal { tpe = Token.Nil; _ } -> Some Expr.Value.Nil
  | Expr.Literal { tpe = Token.Number num; _ } -> Some (Expr.Value.Number num)
  | Expr.Literal { tpe = Token.String str; _ } -> Some (Expr.Value.String str)
  | Expr.Grouping subexpr -> evaluate tree_walker subexpr
  | Expr.Unary (({ tpe = op; line; _ } as op_token), right_expr) -> (
      evaluate tree_walker right_expr >>= fun right ->
      match (op, right) with
      | Token.Minus, Expr.Value.Number n -> Some (Expr.Value.Number (-.n))
      | Token.Bang, _ ->
          Some (Expr.Value.Boolean (not (Expr.Value.is_truthy right)))
      | Token.Minus, _ ->
          runtime_error tree_walker line
            (Printf.sprintf "Cannot apply unary operator %s to value %s"
               (Token.print op_token) (Expr.Value.show right));
          None
      | _ ->
          runtime_error tree_walker line
            ("Invalid unary operator " ^ Token.print op_token);
          None)
  | Expr.Binary (left_expr, ({ tpe = op; line; _ } as op_token), right_expr)
    -> (
      evaluate tree_walker left_expr >>= fun left ->
      evaluate tree_walker right_expr >>= fun right ->
      match (op, left, right) with
      | Token.Plus, Expr.Value.Number ln, Expr.Value.Number rn ->
          Some (Expr.Value.Number (ln +. rn))
      | Token.Plus, Expr.Value.String ls, Expr.Value.String rs ->
          Some (Expr.Value.String (ls ^ rs))
      | Token.Minus, Expr.Value.Number ln, Expr.Value.Number rn ->
          Some (Expr.Value.Number (ln -. rn))
      | Token.Asterisk, Expr.Value.Number ln, Expr.Value.Number rn ->
          Some (Expr.Value.Number (ln *. rn))
      | Token.Slash, Expr.Value.Number ln, Expr.Value.Number rn ->
          Some (Expr.Value.Number (ln /. rn))
      | Token.Greater, Expr.Value.Number ln, Expr.Value.Number rn ->
          let open Float in
          Some (Expr.Value.Boolean (ln > rn))
      | Token.GreaterEqual, Expr.Value.Number ln, Expr.Value.Number rn ->
          let open Float in
          Some (Expr.Value.Boolean (ln >= rn))
      | Token.Less, Expr.Value.Number ln, Expr.Value.Number rn ->
          let open Float in
          Some (Expr.Value.Boolean (ln < rn))
      | Token.LessEqual, Expr.Value.Number ln, Expr.Value.Number rn ->
          let open Float in
          Some (Expr.Value.Boolean (ln <= rn))
      | Token.EqualEqual, _, _ ->
          Some (Expr.Value.Boolean (Expr.Value.equal left right))
      | Token.BangEqual, _, _ ->
          Some (Expr.Value.Boolean (not (Expr.Value.equal left right)))
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
               (Token.print op_token) (Expr.Value.show left)
               (Expr.Value.show right));
          None
      | _ ->
          runtime_error tree_walker line
            ("Invalid binary operator " ^ Token.print op_token);
          None)
  | unknown_form ->
      Errors.report ~runtime:true tree_walker.error_reporter (-1) "evaluate"
        ("Unknown expression form: " ^ Expr.Printer.print_sexpr unknown_form);
      None
