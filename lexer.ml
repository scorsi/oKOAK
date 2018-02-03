(* Lexer *)

let rec lex = parser
    (* whitespaces *)
    | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
    (* identifier: [a-zA-Z][a-zA-Z0-9] *)
    | [< ' ('a' .. 'z' | 'A' .. 'Z' | '_' as id); stream >] ->
        let buffer = Buffer.create 1 in Buffer.add_char buffer id;
        lexer_identifier buffer stream (* Return a Token.Identifier or Token.Def or Token.Extern *)
    (* number: [0-9.]+ *)
    | [< ' ('0' .. '9' as num); stream  >] ->
        let buffer = Buffer.create 1 in Buffer.add_char buffer num;
        lexer_integer buffer stream (* Return a Token.Integer or Token.Double *)
    (* double: \.[0-9]+ *)
    | [< ' ('.' as num); stream >] ->
        let buffer = Buffer.create 1 in Buffer.add_char buffer num;
        lexer_double buffer stream (* Return a Token.Double *)
    (* char: '.' *)
    | [< ' ('''); 'any; ' ('''); stream >] -> [< 'Token.Char (int_of_char any); lex stream >]
    (* string: "." *)
    (*
    | [< ' ('"'); stream >] -> 
        let buffer = Buffer.create 0 in
        lexer_string buffer stream (* Return a Token.String *)
    *)
    (* any *)
    | [< 'any; stream >] -> [< 'Token.Any any; lex stream >]
    (* end of stream *)
    | [< >] -> [< >]

and lexer_integer buffer = parser
    | [< ' ('0' .. '9' as num); stream >] ->
        Buffer.add_char buffer num;
        lexer_integer buffer stream
    | [< ' ('.' as num); stream >] ->
        Buffer.add_char buffer num;
        lexer_double buffer stream
    | [< stream = lex >] -> [< 'Token.Integer (int_of_string (Buffer.contents buffer)); stream >]

and lexer_double buffer = parser
    | [< ' ('0' .. '9' as num); stream >] ->
        Buffer.add_char buffer num;
        lexer_double buffer stream
    | [< stream = lex >] -> [< 'Token.Double (float_of_string (Buffer.contents buffer)); stream >]

(*
and lexer_string buffer = parser
    | [< ' ('"'); stream >] -> [< 'Token.String (Buffer.contents buffer); stream >]
    | [< 'any; stream >] ->
        Buffer.add_char buffer num;
        lexer_string buffer stream
*)
and lexer_identifier buffer = parser
    | [< ' ('a' .. 'z' | 'A' .. 'Z' | '_' as id); stream >] ->
        Buffer.add_char buffer id;
        lexer_identifier buffer stream
    | [< stream = lex >] ->
        match Buffer.contents buffer with
        | "def" -> [< 'Token.Def; stream >]
        | "extern" -> [< 'Token.Extern; stream >]
        | "true" -> [< 'Token.Boolean 1; stream >]
        | "false" -> [< 'Token.Boolean 0; stream >]
        | "if" -> [< 'Token.If; stream >]
        | "then" -> [< 'Token.Then; stream >]
        | "else" -> [< 'Token.Else; stream >]
        | "for" -> [< 'Token.For; stream >]
        | "in" -> [< 'Token.In; stream >]
        | id -> [< 'Token.Identifier id; stream >]
