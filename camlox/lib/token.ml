open Base

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
  | String of string
  | Number of float
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

let type_of_word = function
  | "and" -> And
  | "class" -> Class
  | "else" -> Else
  | "false" -> False
  | "fun" -> Fun
  | "for" -> For
  | "if" -> If
  | "nil" -> Nil
  | "or" -> Or
  | "print" -> Print
  | "return" -> Return
  | "super" -> Super
  | "this" -> This
  | "true" -> True
  | "var" -> Var
  | "while" -> While
  | _ -> Identifier

let describe tok =
  Printf.sprintf "Token(%a: %s @ line %d)"
    (fun () -> show_token_type)
    tok.tpe tok.lexeme tok.line
