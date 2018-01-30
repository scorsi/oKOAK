(*===----------------------------------------------------------------------===
 * Lexer Tokens
 *===----------------------------------------------------------------------===*)

(* The lexer returns these 'Any' if it is an unknown character, otherwise one of
 * these others for known things. *)
type token =
  (* commands *)
  | Def
  | Extern

  (* primary *)
  | Identifier of string
  | Number of float

  (* unknown *)
  | Any of char