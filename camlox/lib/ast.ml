open Base

module Expr = struct
  type t =
    | Binary of t * Token.t * t (* <expr> <op> <expr> *)
    | Call of t * Token.t * t list (* <expr> (<expr-list,>) *)
    | Grouping of t (* (<expr>) *)
    | Literal of Token.t (* <string>|<number>|<identifier> *)
    | Unary of Token.t * t (* <op> <expr> *)
    | Assign of Token.t * t (* <identifier> = <expr> *)
  [@@deriving show]
end

module Stmt = struct
  type t =
    | Expression of Expr.t
    | Print of Expr.t
    | Var of Token.t * Expr.t option
    | Block of t list
    | If of Expr.t * t * t option
    | While of Expr.t * t
    | Function of Token.t * Token.t list * t list
    | Return of Expr.t option
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
    | Expr.Grouping _ -> "group"
    | Expr.Literal tok -> Token.print tok
    | Expr.Unary (op, _) -> "unary " ^ Token.print op
    | Expr.Assign (name, _) -> "def " ^ Token.print name

  let children = function
    | Expr.Binary (left, _, right) -> [ left; right ]
    | Expr.Call (callee, _, args) -> callee :: args
    | Expr.Grouping expr -> [ expr ]
    | Expr.Literal _ -> []
    | Expr.Unary (_, expr) -> [ expr ]
    | Expr.Assign (_, body) -> [ body ]

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
