(* Lexer *)

type token =
    | Def
    | Extern
    | Identifier of string
    | Integer of int
    | Double of float
    | Any of char
