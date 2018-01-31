(* Code Generator *)

open Llvm
open Llvm_analysis

exception Error of string

let context = global_context ()
let kmodule = create_module context "koak"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec codegen_expr = function
    | Ast.Number n -> const_float double_type n
    | Ast.Variable name ->
        (try Hashtbl.find named_values name
        with
        | Not_found -> raise (Error "Unknown variable name"))
    | Ast.Binary (op, lhs, rhs) ->
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        begin
            match op with
            | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
            | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
            | '*' -> build_fmul lhs_val rhs_val "multmp" builder
            | '<' ->
                let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
                build_uitofp i double_type "booltmp" builder
            | _ -> raise (Error "Invalid binary operator")
        end
    | Ast.Call (id, args) ->
        let id =
            match lookup_function id kmodule with
            | Some id -> id
            | _ -> raise (Error "Unknown function reference")
        in let params = params id in
        if Array.length params == Array.length args
        then ()
        else raise (Error "Incorrect number of arguments passed");
        let args = Array.map codegen_expr args in
        build_call id args "calltmp" builder

let codegen_proto = function
    | Ast.Prototype (name, args) ->
        let doubles = Array.make (Array.length args) double_type in
        let ft = function_type double_type doubles in
        let f =
            match lookup_function name kmodule with
            | None -> declare_function name ft kmodule
            | Some f ->
                if block_begin f <> At_end f
                then raise (Error "Redefinition of function");
                if element_type (type_of f) <> ft
                then raise (Error "Redifinition of function with different number of arguments");
                f
        in
        Array.iteri (fun i a ->
            let n = args.(i) in
            set_value_name n a;
            Hashtbl.add named_values n a;
        ) (params f);
        f

let codegen_func = function
    | Ast.Function (proto, body) ->
        Hashtbl.clear named_values;
        let func = codegen_proto proto in
        let bb = append_block context "entry" func in
        position_at_end bb builder;
        try
            let ret_val = codegen_expr body in
            let _ = build_ret ret_val builder in
            assert_valid_function func;
            func
        with
        | e ->
            delete_function func;
            raise e
