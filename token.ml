(* Lexer *)

type token =
    | Def
    | Extern
    | Identifier of string
    | Number of float
    | Any of char
