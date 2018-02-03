(* Lexer *)

type token =
    | Def
    | Extern
    | If
    | Then
    | Else
    | For
    | In
    | Identifier of string
    | Integer of int
    | Double of float
    | Char of int
    (* | String of string *)
    | Boolean of int
    | Any of char
