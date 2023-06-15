type token_type =
  | (* Single-character tokens *)
    LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Asterisk
  | (* Logical operators *)
    Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | (* Literals *)
    Identifier
  | String
  | Number
  | (* Keywords *)
    And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | (* Other *)
    EOF
[@@deriving show]

type t = { tpe : token_type; lexeme : string; line : int }

let create tpe lexeme line = { tpe; lexeme; line }
let eof line = { tpe = EOF; lexeme = ""; line }

let describe tok =
  Printf.sprintf "Token(%a: %s @ line %d)"
    (fun () -> show_token_type)
    tok.tpe tok.lexeme tok.line
