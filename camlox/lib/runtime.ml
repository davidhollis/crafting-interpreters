open Base
open Result

module rec Value : sig
  type t =
    | Number of float
    | String of string
    | Boolean of bool
    | NativeFunction of int * Native.t
    | Function of string option * Token.t list * Ast.Stmt.t list * Environment.t
    | Nil

  val is_truthy : t -> bool
  val equal : t -> t -> bool
  val to_string : t -> string
  val pp : Stdlib.Format.formatter -> t -> unit
  val show : t -> string
end = struct
  type t =
    | Number of float
    | String of string
    | Boolean of bool
    | NativeFunction of int * Native.t
    | Function of string option * Token.t list * Ast.Stmt.t list * Environment.t
    | Nil
  [@@deriving show]

  let is_truthy = function Boolean false | Nil -> false | _ -> true

  let equal a b =
    match (a, b) with
    | Number na, Number nb ->
        let open Float in
        na = nb
    | String sa, String sb ->
        let open String in
        sa = sb
    | Boolean ba, Boolean bb ->
        let open Bool in
        ba = bb
    | Nil, Nil -> true
    | _ -> false

  let to_string = function
    | Number n -> Printf.sprintf "%f" n
    | String s -> s
    | Boolean true -> "true"
    | Boolean false -> "false"
    | NativeFunction (arity, fn_name) ->
        Printf.sprintf "<native function %s/%d>" (Native.to_string fn_name)
          arity
    | Function (Some name, params, _, _) ->
        Printf.sprintf "<function %s/%d>" name (List.length params)
    | Function (None, params, _, _) ->
        Printf.sprintf "<anonymous %d-ary function>" (List.length params)
    | Nil -> "nil"
end

and Environment : sig
  type t

  val create : Errors.t -> t
  val create_from : t -> t
  val get : t -> Token.t -> (Value.t, [> `RuntimeError ]) Result.t

  val get_resolved :
    t -> Token.t -> depth:int -> (Value.t, [> `RuntimeError ]) Result.t

  val assign : t -> Token.t -> Value.t -> (Value.t, [> `RuntimeError ]) Result.t

  val assign_resolved :
    t ->
    Token.t ->
    Value.t ->
    depth:int ->
    (Value.t, [> `RuntimeError ]) Result.t

  val declare : t -> Token.t -> Value.t -> unit
  val pp : Stdlib.Format.formatter -> t -> unit
end = struct
  let global_bindings =
    let g = Hashtbl.create (module String) in
    Hashtbl.set g ~key:"clock" ~data:(Value.NativeFunction (0, Native.Clock));
    g

  type t = {
    enclosing : t option;
    bindings : (string, Value.t) Hashtbl.t;
    error_reporter : Errors.t;
  }

  let pp ppf _env = Stdlib.Format.fprintf ppf "<environment>"

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

  let rec ancestor env ~depth ~(looking_for : Token.t) =
    if depth = 0 then return env
    else
      match env.enclosing with
      | Some parent -> ancestor parent ~depth:(depth - 1) ~looking_for
      | None ->
          runtime_error env looking_for.line
            (Printf.sprintf "couldn't find the resolved scope for '%s'"
               looking_for.lexeme)

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

  let get_resolved env name_token ~depth =
    let name_str = Token.print name_token in
    ancestor env ~depth ~looking_for:name_token >>= fun resolved_env ->
    match Hashtbl.find resolved_env.bindings name_str with
    | Some value -> return value
    | None ->
        runtime_error resolved_env name_token.line
          (Printf.sprintf
             "couldn't find a binding for '%s' in the expected scope" name_str)

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
              (Printf.sprintf "cannot assign to undeclared variable '%s'"
                 name_str))

  let assign_resolved env name_token value ~depth =
    let name_str = Token.print name_token in
    ancestor env ~depth ~looking_for:name_token >>= fun resolved_env ->
    match Hashtbl.find resolved_env.bindings name_str with
    | Some _ ->
        Hashtbl.set resolved_env.bindings ~key:name_str ~data:value;
        return value
    | None ->
        runtime_error resolved_env name_token.line
          (Printf.sprintf
             "couldn't find a binding for '%s' in the expected scope" name_str)

  let declare env name_token value =
    Hashtbl.set env.bindings ~key:(Token.print name_token) ~data:value
end
