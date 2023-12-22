open Base
open Result
open Ast

type resolved_symbols = (Token.t, int) Hashtbl.t

let lookup (table : resolved_symbols) symbol = Hashtbl.find table symbol
let empty_table () = Hashtbl.create (module Token)

type function_type = NoFunction | Function | Method | Initializer
type class_type = NoClass | Class

type t = {
  error_reporter : Errors.t;
  scopes : (string, bool) Hashtbl.t Stack.t;
  resolutions : resolved_symbols;
  mutable current_function_type : function_type;
  mutable current_class_type : class_type;
}

let create error_reporter =
  {
    error_reporter;
    scopes = Stack.create ();
    resolutions = Hashtbl.create (module Token);
    current_function_type = NoFunction;
    current_class_type = NoClass;
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
      declare resolver name >>= fun () ->
      (match initilizer with
      | Some expr -> visit_expr resolver expr
      | None -> return ())
      >>= fun () -> define resolver name
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
  | Stmt.Function ((name, _, _) as fn) ->
      declare resolver name >>= fun () ->
      define resolver name >>= fun () ->
      resolve_function resolver fn ~function_type:Function
  | Stmt.Return (return_token, expr) -> (
      match resolver.current_function_type with
      | NoFunction ->
          resolution_error resolver "Illegal 'return' from top-level code."
            ~at_token:return_token
      | Initializer ->
          if Option.is_some expr then
            resolution_error resolver
              "cannot return a value from an initializer" ~at_token:return_token
          else return ()
      | _ -> (
          match expr with
          | Some expr -> visit_expr resolver expr
          | None -> return ()))
  | Stmt.Class (name, superclass, methods) ->
      let enclosing = resolver.current_class_type in
      resolver.current_class_type <- Class;
      declare resolver name >>= fun () ->
      (match superclass with
      | Some superclass_expr -> visit_expr resolver superclass_expr
      | None -> return ())
      >>= fun () ->
      define resolver name >>= fun () ->
      begin_scope resolver >>= fun () ->
      Option.iter (Stack.top resolver.scopes) ~f:(fun new_scope ->
          Hashtbl.set new_scope ~key:"this" ~data:true);
      List.fold methods ~init:(return ())
        ~f:(fun r ((method_name, _, _) as method_defn) ->
          r >>= fun () ->
          resolve_function resolver method_defn
            ~function_type:
              (if String.(method_name.lexeme = "init") then Initializer
               else Method))
      >>= fun () ->
      end_scope resolver >>| fun () -> resolver.current_class_type <- enclosing

and visit_expr resolver = function
  | Expr.Binary (left, _, right) ->
      visit_expr resolver left >>= fun () -> visit_expr resolver right
  | Expr.Call (callee, _, args) ->
      visit_expr resolver callee >>= fun () ->
      List.fold args ~init:(return ()) ~f:(fun r arg ->
          r >>= fun () -> visit_expr resolver arg)
  | Expr.Get (referent, _) -> visit_expr resolver referent
  | Expr.Grouping subexpr -> visit_expr resolver subexpr
  | Expr.Literal ({ tpe = Token.Identifier; lexeme; _ } as name) ->
      let is_valid_access =
        Option.(
          Stack.top resolver.scopes
          >>= (fun scope -> Hashtbl.find scope lexeme)
          |> value ~default:true)
      in
      if is_valid_access then return (resolve_local resolver name)
      else
        resolution_error ~at_token:name resolver
          "Can't reference a variable inside its own initializer."
  | Expr.Literal ({ tpe = Token.This; _ } as name) -> (
      match resolver.current_class_type with
      | NoClass ->
          resolution_error ~at_token:name resolver
            "illegal use of 'this' outside of a class."
      | Class -> return (resolve_local resolver name))
  | Expr.Literal _ -> return ()
  | Expr.Unary (_, right) -> visit_expr resolver right
  | Expr.Assign (name, rexpr) ->
      visit_expr resolver rexpr >>| fun () -> resolve_local resolver name
  | Expr.Set (lvalue, _, rvalue) ->
      visit_expr resolver rvalue >>= fun () -> visit_expr resolver lvalue

and resolve_function resolver (_, args, body) ~function_type =
  let enclosing = resolver.current_function_type in
  resolver.current_function_type <- function_type;
  begin_scope resolver >>= fun () ->
  List.fold args ~init:(return ()) ~f:(fun r argname ->
      r >>= fun () -> define resolver argname)
  >>= fun () ->
  visit_statement_sequence resolver body >>= fun () ->
  end_scope resolver >>| fun () -> resolver.current_function_type <- enclosing

and begin_scope resolver =
  return (Stack.push resolver.scopes (Hashtbl.create (module String)))

and end_scope resolver =
  match Stack.pop resolver.scopes with
  | Some _ -> return ()
  | None ->
      resolution_error resolver
        "Attempted to end a scope while no scopes were open"

and declare resolver name =
  match Stack.top resolver.scopes with
  | Some scope -> (
      match Hashtbl.find scope name.lexeme with
      | Some _ ->
          resolution_error ~at_token:name resolver
            (Printf.sprintf "attempted to redeclare variable '%s'" name.lexeme)
      | None -> return (Hashtbl.set scope ~key:name.lexeme ~data:false))
  | None -> return ()

and define resolver name =
  match Stack.top resolver.scopes with
  | Some scope -> return (Hashtbl.set scope ~key:name.lexeme ~data:true)
  | None -> return ()

and resolve_local resolver name =
  let nearest_scope =
    Base.List.findi (Stack.to_list resolver.scopes) ~f:(fun _ scope ->
        Base.Option.is_some (Hashtbl.find scope Token.(name.lexeme)))
  in
  match nearest_scope with
  | Some (depth, _) -> Hashtbl.set resolver.resolutions ~key:name ~data:depth
  | None -> ()

and resolve_program resolver stmts =
  visit_statement_sequence resolver stmts >>| fun () ->
  (stmts, resolver.resolutions)

and resolve_repl resolver = function
  | Either.First stmts ->
      visit_statement_sequence resolver stmts >>| fun () ->
      (Either.first stmts, resolver.resolutions)
  | Either.Second expr ->
      visit_expr resolver expr >>| fun () ->
      (Either.second expr, resolver.resolutions)
