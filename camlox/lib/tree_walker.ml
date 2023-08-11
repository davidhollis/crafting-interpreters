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
  | Expr.Binary (left_expr, { tpe = Token.And; _ }, right_expr) ->
      evaluate_expr tree_walker left_expr >>= fun left ->
      if not (Value.is_truthy left) then return left
      else evaluate_expr tree_walker right_expr
  | Expr.Binary (left_expr, { tpe = Token.Or; _ }, right_expr) ->
      evaluate_expr tree_walker left_expr >>= fun left ->
      if Value.is_truthy left then return left
      else evaluate_expr tree_walker right_expr
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
  | Expr.Call (callee_expr, { line; _ }, arg_expr_list) ->
      evaluate_expr tree_walker callee_expr >>= fun callee ->
      arg_expr_list |> List.map ~f:(evaluate_expr tree_walker) |> Result.all
      >>= fun arg_list -> call_value tree_walker line arg_list callee
  | unknown_form ->
      runtime_error tree_walker (-1)
        ("Unknown expression form: " ^ Printer.print_sexpr unknown_form)

and run_program tree_walker =
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
  | Stmt.If (cond_expr, then_stmt, else_stmt) -> (
      evaluate_expr tree_walker cond_expr >>= fun cond_value ->
      if Value.is_truthy cond_value then execute_stmt tree_walker then_stmt
      else
        match else_stmt with
        | Some actual_else_stmt -> execute_stmt tree_walker actual_else_stmt
        | None -> return ())
  | Stmt.While (cond_expr, body_stmt) ->
      execute_while tree_walker cond_expr body_stmt
  | Stmt.Function (name, params, body) ->
      return
        (declare_var tree_walker name
           (Value.Function (Some (Token.print name), params, body)))
  | Stmt.Return expr ->
        Option.(expr >>| evaluate_expr tree_walker) |> Option.value ~default:(return Value.Nil)
        >>= fun retval -> fail (`Return retval)

and execute_block tree_walker stmts env =
  let previous = tree_walker.environment in
  tree_walker.environment <- env;
  let result = run_program tree_walker stmts in
  tree_walker.environment <- previous;
  result

and execute_while tree_walker cond_expr body_stmt =
  let last_result = ref (return true)
  and should_keep_going = function Ok true -> true | _ -> false in
  while should_keep_going !last_result do
    last_result :=
      evaluate_expr tree_walker cond_expr >>= fun cond_value ->
      if Value.is_truthy cond_value then
        execute_stmt tree_walker body_stmt >>| fun () -> true
      else return false
  done;
  !last_result >>| fun _ -> ()

and call_value tree_walker line args = function
  | Value.NativeFunction (arity, name) -> (
      if not (phys_equal (List.length args) arity) then
        runtime_error tree_walker line
          (Printf.sprintf
             "Wrong number of arguments for native function %s (expected: %d; \
              got: %d)"
             (Native.to_string name) arity (List.length args))
      else
        Native_impl.eval name args |> function
        | Error `BadNativeCall ->
            runtime_error tree_walker line
              (Printf.sprintf "Bad call to native function %s"
                 (Native.to_string name))
        | Ok _ as result -> result)
  | Value.Function (name, params, body) ->
      if not (phys_equal (List.length args) (List.length params)) then
        runtime_error tree_walker line
          (Printf.sprintf
             "Wrong number of arguments for %s (expected: %d; got: %d)"
             (Option.value name ~default:"<anonymous function>")
             (List.length params) (List.length args))
      else
        let function_env = Env.create_from tree_walker.environment in
        ignore
          List.(
            zip_exn params args >>| fun (argname, argval) ->
            Env.declare function_env argname argval);
        (match execute_block tree_walker body function_env with
        | Ok () -> return Value.Nil
        | Error (`Return v) -> return v
        | Error `RuntimeError -> fail `RuntimeError
        )
  | v ->
      runtime_error tree_walker line
        (Printf.sprintf "Value %s is not callable" (Value.to_string v))

let handle_bad_return tree_walker = function
    | Ok v -> return v
    | Error (`Return _) -> runtime_error tree_walker (-1) "Attempted to return from outside of a function or method scope"
    | Error `RuntimeError -> fail `RuntimeError

let run_repl tree_walker code =
  (match code with
    | Either.First stmts -> run_program tree_walker stmts >>| fun _ -> Value.Nil
    | Either.Second expr -> evaluate_expr tree_walker expr)
  |> handle_bad_return tree_walker

  let run_program_toplevel tree_walker code = run_program tree_walker code |> handle_bad_return tree_walker
