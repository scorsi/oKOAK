(* AST *)

type expr =
    | Double of float
    | Integer of int
    | Char of int
    (* | String of string *)
    | Boolean of int
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array
    (* if then else *)
    | If of expr * expr * expr

(* Argument : Symbol Type *)
type arg = Argument of string * string

(* Prototype : Symbol Arguments Type *)
type proto = Prototype of string * arg array * string

(* Function : Prototype Expr *)
type func = Function of proto * expr
