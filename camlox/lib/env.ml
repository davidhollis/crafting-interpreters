open Base
open Result
open Ast

let global_bindings =
  let g = Hashtbl.create (module String) in
  Hashtbl.set g ~key:"clock" ~data:(Value.NativeFunction (0, Native.Clock));
  g

type t = {
  enclosing : t option;
  bindings : (string, Value.t) Hashtbl.t;
  error_reporter : Errors.t;
}

let create_global error_reporter =
  { enclosing = None; bindings = global_bindings; error_reporter }

let create_from enclosing =
  {
    enclosing = Some enclosing;
    bindings = Hashtbl.create (module String);
    error_reporter = enclosing.error_reporter;
  }

let create error_reporter = create_from (create_global error_reporter)

let runtime_error env line message =
  Errors.report ~runtime:true env.error_reporter line "in `Env'" message;
  fail `RuntimeError

let rec get env name_token =
  let name_str = Token.print name_token in
  match Hashtbl.find env.bindings name_str with
  | Some value -> return value
  | None -> (
      match env.enclosing with
      | Some parent -> get parent name_token
      | None ->
          runtime_error env name_token.line
            (Printf.sprintf "found reference to undefined variable '%s'"
               name_str))

let rec assign env name_token value =
  let name_str = Token.print name_token in
  match Hashtbl.find env.bindings name_str with
  | Some _ ->
      Hashtbl.set env.bindings ~key:name_str ~data:value;
      return value
  | None -> (
      match env.enclosing with
      | Some parent -> assign parent name_token value
      | None ->
          runtime_error env name_token.line
            (Printf.sprintf "cannot assign to undeclared variable '%s'" name_str)
      )

let declare env name_token value =
  Hashtbl.set env.bindings ~key:(Token.print name_token) ~data:value
