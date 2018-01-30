(* Lexer *)

let rec lex = parser
    (* whitespaces *)
    | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream
    (* identifier: [a-zA-Z][a-zA-Z0-9] *)
    | [< ' ('a' .. 'z' | 'A' .. 'Z' as id); stream >] ->
        let buffer = Buffer.create 1 in Buffer.add_char buffer id;
        lexer_identifier buffer stream (* Return a Token.Identifier or Token.Def or Token.Extern *)
    (* number: [0-9.]+ *)
    | [< ' ('0' .. '9' as num); stream  >] ->
        let buffer = Buffer.create 1 in Buffer.add_char buffer num;
        lexer_number buffer stream (* Return a Token.Number *)
    (* any *)
    | [< 'any; stream >] -> [< 'Token.Any any; lex stream >]
    (* end of stream *)
    | [< >] -> [< >]

and lexer_number buffer = parser
    | [< ' ('0' .. '9' as num); stream >] ->
        Buffer.add_char buffer num;
        lexer_number buffer stream
    | [< stream = lex >] -> [< 'Token.Number (float_of_string (Buffer.contents buffer)); stream >]

and lexer_identifier buffer = parser
    | [< ' ('a' .. 'z' | 'A' .. 'Z' as id); stream >] ->
        Buffer.add_char buffer id;
        lexer_identifier buffer stream
    | [< stream = lex >] ->
        match Buffer.contents buffer with
        | "def" -> [< 'Token.Def; stream >]
        | "extern" -> [< 'Token.Extern; stream >]
        | id -> [< 'Token.Identifier id; stream >]
