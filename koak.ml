(* Main *)

open Llvm

(* top ::= definition | external | expression | ';' *)
let rec main_loop stream =
    match Stream.peek stream with
    | None -> ()
    | Some (Token.Any ';') ->
        Stream.junk stream;
        main_loop stream
    | Some token ->
        begin
            try match token with
            | Token.Def ->
                let e = Parser.parse_definition stream in
                dump_value (Codegenerator.codegen_func e);
            | Token.Extern ->
                let e = Parser.parse_extern stream in
                dump_value (Codegenerator.codegen_proto e);
            | _ ->
                let e = Parser.parse_topexpr stream in
                dump_value (Codegenerator.codegen_func e);
            with
            | Stream.Error s ->
                Stream.junk stream;
                print_endline s;
        end;
        print_string "ready> "; flush stdout;
        main_loop stream

let _ =
    Hashtbl.add Parser.binop_precedence ':' 2;
    Hashtbl.add Parser.binop_precedence '<' 10;
    Hashtbl.add Parser.binop_precedence '+' 20;
    Hashtbl.add Parser.binop_precedence '-' 20;
    Hashtbl.add Parser.binop_precedence '*' 40;

    print_endline "KOAK, compiler/interpreter";
    print_string "ready> "; flush stdout;
    let stream = Lexer.lex (Stream.of_channel stdin) in main_loop stream;
    dump_module Codegenerator.kmodule
;;
