open Base
open Result
open Ast

type t = {
  error_reporter : Errors.t;
  scopes : (string, bool) Base.Hashtbl.t Base.Stack.t;
  resolutions : (Token.t, int) Base.Hashtbl.t;
}

let create error_reporter =
  {
    error_reporter;
    scopes = Base.Stack.create ();
    resolutions = Base.Hashtbl.create (module Token);
  }

let resolution_error ?at_token resolver message =
  (match at_token with
  | Some tok ->
      Errors.report resolver.error_reporter tok.Token.line
        (Printf.sprintf "at '%s'" (Token.describe tok))
        message
  | None ->
      Errors.report resolver.error_reporter (-1)
        "while resolving variable references" message);
  fail `ResolutionError

let rec visit_statement_sequence resolver =
  List.fold ~init:(return ()) ~f:(fun r stmt ->
      r >>= fun () -> visit_statement resolver stmt)

and visit_statement resolver = function
  | Stmt.Expression expr -> visit_expr resolver expr
  | Stmt.Print expr -> visit_expr resolver expr
  | Stmt.Var (name, initilizer) ->
      declare resolver name;
      (match initilizer with
      | Some expr -> visit_expr resolver expr
      | None -> return ())
      >>= fun () ->
      define resolver name;
      return ()
  | Stmt.Block stmts ->
      begin_scope resolver >>= fun () ->
      visit_statement_sequence resolver stmts >>= fun () -> end_scope resolver
  | Stmt.If (cond, then_branch, else_branch) -> (
      visit_expr resolver cond >>= fun () ->
      visit_statement resolver then_branch >>= fun () ->
      match else_branch with
      | Some else_branch -> visit_statement resolver else_branch
      | None -> return ())
  | Stmt.While (cond, body) ->
      visit_expr resolver cond >>= fun () -> visit_statement resolver body
  | Stmt.Function (name, args, body) ->
      define resolver name;
      begin_scope resolver >>= fun () ->
      List.iter args ~f:(fun argname -> define resolver argname);
      visit_statement_sequence resolver body >>= fun () -> end_scope resolver
  | Stmt.Return (Some expr) -> visit_expr resolver expr
  | Stmt.Return None -> return ()

and visit_expr resolver = function
  | Expr.Binary (left, _, right) ->
      visit_expr resolver left >>= fun () -> visit_expr resolver right
  | Expr.Call (callee, _, args) ->
      visit_expr resolver callee >>= fun () ->
      List.fold args ~init:(return ()) ~f:(fun r arg ->
          r >>= fun () -> visit_expr resolver arg)
  | Expr.Grouping subexpr -> visit_expr resolver subexpr
  | Expr.Literal ({ tpe = Token.Identifier; lexeme; _ } as name) ->
      let is_valid_access =
        Option.(
          Base.Stack.top resolver.scopes
          >>= (fun scope -> Base.Hashtbl.find scope lexeme)
          |> value ~default:true)
      in
      if is_valid_access then return (resolve_local resolver name)
      else
        resolution_error ~at_token:name resolver
          "Can't reference a variable inside its own initializer."
  | Expr.Literal _ -> return ()
  | Expr.Unary (_, right) -> visit_expr resolver right
  | Expr.Assign (name, rexpr) ->
      visit_expr resolver rexpr >>| fun () -> resolve_local resolver name

and begin_scope resolver =
  return (Base.Stack.push resolver.scopes (Base.Hashtbl.create (module String)))

and end_scope resolver =
  match Base.Stack.pop resolver.scopes with
  | Some _ -> return ()
  | None ->
      resolution_error resolver
        "Attempted to end a scope while no scopes were open"

and declare resolver name =
  match Base.Stack.top resolver.scopes with
  | Some scope -> Base.Hashtbl.set scope ~key:name.lexeme ~data:false
  | None -> ()

and define resolver name =
  match Base.Stack.top resolver.scopes with
  | Some scope -> Base.Hashtbl.set scope ~key:name.lexeme ~data:true
  | None -> ()

and resolve_local resolver name =
  let nearest_scope =
    Base.List.findi (Base.Stack.to_list resolver.scopes) ~f:(fun _ scope ->
        Base.Option.is_some (Base.Hashtbl.find scope Token.(name.lexeme)))
  in
  match nearest_scope with
  | Some (depth, _) ->
      Base.Hashtbl.set resolver.resolutions ~key:name ~data:depth
  | None -> ()

and resolve resolver stmts =
  visit_statement_sequence resolver stmts >>| fun () -> resolver.resolutions