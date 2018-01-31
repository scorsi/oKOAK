(* Parser *)

let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

(* primary
 *  ::= identifier
 *  ::= numberexpr
 *  ::= parenexpr *)
let rec parse_primary = parser
    (* numberexpr ::= number *)
    | [< 'Token.Integer number >] -> Ast.Integer number
    | [< 'Token.Double number >] -> Ast.Double number
    (* parenexpr ::= '(' expression ')' *)
    | [< 'Token.Any '('; expr = parse_expr; 'Token.Any ')' ?? "Expected ')'" >] -> expr
    (* identifierexpr
     *  ::= identifier
     *  ::= identifier '(' argumentexpr ')' *)
    | [< 'Token.Identifier id; stream >] ->
        let rec parse_args args = parser
            | [< arg = parse_expr; stream >] ->
                begin parser
                    | [< 'Token.Any ','; args' = parse_args (arg :: args) >] -> args'
                    | [<  >] -> arg :: args
                end stream
            | [< >] -> args
        in let rec parse_identifier id = parser
            | [< 'Token.Any '('; args = parse_args []; 'Token.Any ')' ?? "Expected ')'" >] -> Ast.Call (id, Array.of_list (List.rev args))
            | [< >] -> Ast.Variable id
        in parse_identifier id stream
    | [< >] -> raise (Stream.Error "Unknown token when expecting an expression.")

(* binoprhs ::= (#binop primary)* *)
and parse_binop_rhs expr_prec lhs stream =
    match Stream.peek stream with
    | Some (Token.Any op) when Hashtbl.mem binop_precedence op ->
        let token_prec = precedence op in
        if token_prec < expr_prec
        then lhs
        else begin
            Stream.junk stream;
            let rhs = parse_primary stream in
            let rhs =
                match Stream.peek stream with
                | Some (Token.Any op2) ->
                    let next_prec = precedence op2 in
                    if token_prec < next_prec
                    then parse_binop_rhs (token_prec + 1) rhs stream
                    else rhs
                | _ -> rhs
            in let lhs = Ast.Binary (op, lhs, rhs) in
            parse_binop_rhs expr_prec lhs stream
        end
    | _ -> lhs

(* expression ::= primary binoprhs *)
and parse_expr = parser
    | [< lhs = parse_primary; stream >] -> parse_binop_rhs 0 lhs stream

(* prototype ::= identifier '(' (identifier ':' identifier)* ')' *)
let parse_prototype =
    let rec parse_args args = parser
        | [< 'Token.Identifier id; 'Token.Any ':' ?? "Expected ':' in prototype argument"; 'Token.Identifier argtype ?? "Expected argument type in prototype argument"; args' = parse_args ((Ast.Argument (id, argtype)) :: args) >] -> args'
        | [< >] -> args
    in parser
    | [< 'Token.Identifier id; 'Token.Any '(' ?? "Expected '(' in prototype"; args = parse_args []; 'Token.Any ')' ?? "Expected ')' in prototype"; 'Token.Any ':' ?? "Expected ':' in prototype"; 'Token.Identifier funtype ?? "Expected function type in prototype" >] ->
        Ast.Prototype (id, Array.of_list (List.rev args), funtype)
    | [< >] -> raise (Stream.Error "Expected function name in prototype")

(* definition ::= 'def' prototype expression *)
let parse_definition = parser
    | [< 'Token.Def; proto = parse_prototype; expr = parse_expr >] -> Ast.Function (proto, expr)

(* topexpr ::= expression *)
let parse_topexpr = parser
    | [< expr = parse_expr >] -> Ast.Function (Ast.Prototype ("", [||], "double"), expr)

(* external ::= 'extern' prototype *)
let parse_extern = parser
    | [< 'Token.Extern; proto = parse_prototype >] -> proto
