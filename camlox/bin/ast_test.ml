open Camlox

let tree =
  Ast.Expr.Binary
    ( Ast.Expr.Unary
        ( Token.create Token.Minus "-" (-1),
          Ast.Expr.Literal (Token.create (Token.Number 123.) "123" (-1)) ),
      Token.create Token.Asterisk "*" (-1),
      Ast.Expr.Grouping
        (Ast.Expr.Literal (Token.create (Token.Number 45.67) "45.67" (-1))) )
;;

match Sys.argv with
| [| _; "sexpr" |] -> print_endline (Ast.Printer.print_sexpr tree)
| _ -> print_endline (Ast.Printer.print_braces tree)
