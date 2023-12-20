open Base

module Expr = struct
  type t =
    | Binary of t * Token.t * t (* <expr> <op> <expr> *)
    | Call of t * Token.t * t list (* <expr> (<expr-list,>) *)
    | Get of t * Token.t (* <expr> . <identifier> *)
    | Grouping of t (* (<expr>) *)
    | Literal of Token.t (* <string>|<number>|<identifier> *)
    | Unary of Token.t * t (* <op> <expr> *)
    | Assign of Token.t * t (* <identifier> = <expr> *)
    | Set of t * Token.t * t (* <expr> . <identifier> = <expr> *)
  [@@deriving show]
end

module Stmt = struct
  type function_definition = Token.t * Token.t list * t list

  and t =
    | Expression of Expr.t (* <expr> *)
    | Print of Expr.t (* print <expr> *)
    | Var of Token.t * Expr.t option (* var <identifier> [= <expr>] *)
    | Block of t list (* { <stmt-list;> } *)
    | If of Expr.t * t * t option (* if (<expr>) <stmt> [else <stmt>] *)
    | While of Expr.t * t (* while (<expr>) <stmt> *)
    | Function of function_definition
      (* fun <identifier> (<identifier-list,>) { <stmt-list;> } *)
    | Return of Token.t * Expr.t option (* return [<expr>] *)
    | Class of
        Token.t
        * function_definition list (* class <identifier> { <method-list> } *)
  [@@deriving show]
end

module Printer = struct
  type t = {
    item_start : string;
    item_end : string;
    body_start : string;
    body_end : string;
    child_sep : string;
    indent : string;
  }

  let configure ?(item_start = "") ?(item_end = "") ?(body_start = "")
      ?(body_end = "") ?(child_sep = "") ?(indent = "") () =
    { item_start; item_end; body_start; body_end; child_sep; indent }

  let dup str n =
    let rec dup' acc n = if n > 0 then dup' (str ^ acc) (n - 1) else acc in
    dup' "" n

  let print config name children ~render =
    let rec print_item buffer depth name children =
      let indent_here = dup config.indent depth in
      Buffer.add_string buffer indent_here;
      Buffer.add_string buffer config.item_start;
      Buffer.add_string buffer name;
      if List.length children > 0 then (
        Buffer.add_string buffer config.body_start;
        List.iter children ~f:(fun child ->
            render
              (fun name children ->
                ignore (print_item buffer (depth + 1) name children))
              child;
            Buffer.add_string buffer config.child_sep);
        Buffer.add_string buffer indent_here;
        Buffer.add_string buffer config.body_end);
      Buffer.add_string buffer config.item_end;
      buffer
    in
    Buffer.contents (print_item (Buffer.create 256) 0 name children)

  let name = function
    | Expr.Binary (_, op, _) -> Token.print op
    | Expr.Call _ -> "call"
    | Expr.Get (_, name) -> "get " ^ Token.print name
    | Expr.Grouping _ -> "group"
    | Expr.Literal tok -> Token.print tok
    | Expr.Unary (op, _) -> "unary " ^ Token.print op
    | Expr.Assign (name, _) -> "def " ^ Token.print name
    | Expr.Set (_, name, _) -> "set " ^ Token.print name

  let children = function
    | Expr.Binary (left, _, right) -> [ left; right ]
    | Expr.Call (callee, _, args) -> callee :: args
    | Expr.Get (referent, _) -> [ referent ]
    | Expr.Grouping expr -> [ expr ]
    | Expr.Literal _ -> []
    | Expr.Unary (_, expr) -> [ expr ]
    | Expr.Assign (_, body) -> [ body ]
    | Expr.Set (lvalue, _, rvalue) -> [ lvalue; rvalue ]

  let render_expr p expr = p (name expr) (children expr)

  let print_sexpr expr =
    print
      (configure ~item_start:"(" ~item_end:")" ~body_start:" " ~child_sep:" " ())
      (name expr) (children expr) ~render:render_expr

  let print_braces expr =
    print
      (configure ~body_start:" {\n" ~body_end:"}" ~child_sep:"\n" ~indent:"    "
         ())
      (name expr) (children expr) ~render:render_expr
end
