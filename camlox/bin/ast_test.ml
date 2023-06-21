open Camlox

let tree =
  Expr.Binary
    ( Expr.Unary
        ( Token.create Token.Minus "-" (-1),
          Expr.Literal (Token.create (Token.Number 123.) "123" (-1)) ),
      Token.create Token.Asterisk "*" (-1),
      Expr.Grouping
        (Expr.Literal (Token.create (Token.Number 45.67) "45.67" (-1))) )
;;

match Sys.argv with
| [| _; "sexpr" |] -> print_endline (Expr.Printer.print_sexpr tree)
| _ -> print_endline (Expr.Printer.print_braces tree)
