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

let print tok = tok.lexeme

let has_type tok tpe =
  match (tok, tpe) with
  | { tpe = LeftParen; _ }, LeftParen -> true
  | { tpe = RightParen; _ }, RightParen -> true
  | { tpe = LeftBrace; _ }, LeftBrace -> true
  | { tpe = RightBrace; _ }, RightBrace -> true
  | { tpe = Comma; _ }, Comma -> true
  | { tpe = Dot; _ }, Dot -> true
  | { tpe = Minus; _ }, Minus -> true
  | { tpe = Plus; _ }, Plus -> true
  | { tpe = Semicolon; _ }, Semicolon -> true
  | { tpe = Slash; _ }, Slash -> true
  | { tpe = Asterisk; _ }, Asterisk -> true
  | { tpe = Bang; _ }, Bang -> true
  | { tpe = BangEqual; _ }, BangEqual -> true
  | { tpe = Equal; _ }, Equal -> true
  | { tpe = EqualEqual; _ }, EqualEqual -> true
  | { tpe = Greater; _ }, Greater -> true
  | { tpe = GreaterEqual; _ }, GreaterEqual -> true
  | { tpe = Less; _ }, Less -> true
  | { tpe = LessEqual; _ }, LessEqual -> true
  | { tpe = Identifier; _ }, Identifier -> true
  | { tpe = String _; _ }, String _ -> true
  | { tpe = Number _; _ }, Number _ -> true
  | { tpe = And; _ }, And -> true
  | { tpe = Class; _ }, Class -> true
  | { tpe = Else; _ }, Else -> true
  | { tpe = False; _ }, False -> true
  | { tpe = Fun; _ }, Fun -> true
  | { tpe = For; _ }, For -> true
  | { tpe = If; _ }, If -> true
  | { tpe = Nil; _ }, Nil -> true
  | { tpe = Or; _ }, Or -> true
  | { tpe = Print; _ }, Print -> true
  | { tpe = Return; _ }, Return -> true
  | { tpe = Super; _ }, Super -> true
  | { tpe = This; _ }, This -> true
  | { tpe = True; _ }, True -> true
  | { tpe = Var; _ }, Var -> true
  | { tpe = While; _ }, While -> true
  | { tpe = EOF; _ }, EOF -> true
  | _, _ -> false
