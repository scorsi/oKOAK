(* AST *)

type expr =
    | Double of float
    | Integer of int
    | Char of int
    (* | String of string *)
    | Boolean of int
    | Variable of string
    | Binary of char * expr * expr
    | Unary of char * expr
    | Call of string * expr array
    (* if then else *)
    | If of expr * expr * expr
    (* for in *)
    | For of string * expr * expr * expr option * expr

(* Argument : Symbol Type *)
type arg = Argument of string * string

type proto =
    (* Prototype : Symbol Arguments Type *)
    | Prototype of string * arg array * string
    (* Unary is a Prototype *)
    | BinaryPrototype of string * arg array * int * string

(* Function : Prototype Expr *)
type func = Function of proto * expr
