(* Main *)

let _ =
    Hashtbl.add Parser.binop_precedence ':' 2;
    Hashtbl.add Parser.binop_precedence '<' 10;
    Hashtbl.add Parser.binop_precedence '+' 20;
    Hashtbl.add Parser.binop_precedence '-' 20;
    Hashtbl.add Parser.binop_precedence '*' 40;

    print_string "cmd> "; flush stdout;
    let stream = Lexer.lex (Stream.of_channel stdin) in Parser.parse stream;
;;
