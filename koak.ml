(* Main *)

open Llvm
open Llvm_target
open Llvm_scalar_opts
open Llvm_bitwriter

(* top ::= definition | external | expression | ';' *)
let rec main_loop optimizer stream =
    match Stream.peek stream with
    | None -> ()
    | Some (Token.Any ';') ->
        Stream.junk stream;
        main_loop optimizer stream
    | Some token ->
        begin
            try
                match token with
                | Token.Def ->
                    let e = Parser.parse_definition stream in
                    dump_value (Codegenerator.codegen_func optimizer e);
                | Token.Extern ->
                    let e = Parser.parse_extern stream in
                    dump_value (Codegenerator.codegen_proto e);
                | _ ->
                    let e = Parser.parse_topexpr stream in
                    dump_value (Codegenerator.codegen_func optimizer e);
            with
            | Stream.Error s ->
                Stream.junk stream;
                print_endline s;
        end;
        print_string "ready> "; flush stdout;
        main_loop optimizer stream

let start stream =
    let lexer = Lexer.lex stream in
    let optimizer = PassManager.create_function Codegenerator.kmodule in
    begin
        add_instruction_combination optimizer;
        add_reassociation optimizer;
        add_gvn optimizer;
        add_cfg_simplification optimizer;
        ignore (PassManager.initialize optimizer);
    end;
    main_loop optimizer lexer;
    dump_module Codegenerator.kmodule;
    (*
    let target = Target.first () in
    match target with
    | Some target ->
        let targetmachine = TargetMachine.create (Target.default_triple ()) target in
        TargetMachine.emit_to_file Codegenerator.kmodule CodeGenFileType.ObjectFile "a.out" targetmachine
    | None -> raise (Error "Can't compile file")
    *)
    write_bitcode_file Codegenerator.kmodule "a.out"
;;

let _ =
    (*
    Hashtbl.add Parser.binop_precedence '|' 40;
    Hashtbl.add Parser.binop_precedence '^' 50;
    Hashtbl.add Parser.binop_precedence '&' 60;
    *)

    Hashtbl.add Parser.binop_precedence '<' 80;
    (*Hashtbl.add Parser.binop_precedence '>' 80;*)

    Hashtbl.add Parser.binop_precedence '+' 100;
    Hashtbl.add Parser.binop_precedence '-' 100;
    Hashtbl.add Parser.binop_precedence '*' 110;
    Hashtbl.add Parser.binop_precedence '/' 110;
    (*Hashtbl.add Parser.binop_precedence '%' 110;*)

    if (Array.length Sys.argv) > 1 
    then
        let stream = Stream.of_channel (open_in Sys.argv.(1)) in
        start stream
    else
        begin
            print_endline "KOAK, compiler/interpreter";
            print_string "ready> "; flush stdout;
            let stream = Stream.of_channel stdin in
            start stream
        end
