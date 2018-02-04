(* Code Generator *)

open Llvm
open Llvm_analysis

exception Error of string

let context = global_context ()
let kmodule = create_module context "koak"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let double_type = double_type context
let i64_type = i64_type context
let void_type = void_type context
let i8_type = i8_type context
let i1_type = i1_type context

let codegen_type = function
    | "double" -> double_type
    | "int" -> i64_type
    | "char" -> i8_type
    (* | "string" -> string_type *)
    | "bool" -> i1_type
    | "void" -> void_type
    | _ -> raise (Error "Invalid type")

let rec codegen_expr = function
    | Ast.Double n -> const_float double_type n
    | Ast.Integer n -> const_int i64_type n
    | Ast.Char c -> const_int i8_type c
    (* | Ast.String s -> const_string string_type s *)
    | Ast.Boolean b -> const_int i1_type b
    | Ast.Variable name ->
        (try Hashtbl.find named_values name
        with
        | Not_found -> raise (Error "Unknown variable name"))
    (*
    | Ast.Binary (op, lhs, rhs) ->
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        begin
            if (type_of lhs_val) = (type_of rhs_val)
            then 
                match string_of_lltype (type_of lhs_val) with
                | "double" ->
                    begin
                        match op with
                        | '+' -> build_fadd lhs_val rhs_val "addtmp" builder
                        | '-' -> build_fsub lhs_val rhs_val "subtmp" builder
                        | '*' -> build_fmul lhs_val rhs_val "multmp" builder
                        | '<' ->
                            let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
                            build_uitofp i double_type "boolcasttmp" builder
                        | _ -> raise (Error "Invalid binary operator")
                    end
                | "i8" ->
                    begin
                        match op with
                        | '+' -> build_add lhs_val rhs_val "addtmp" builder
                        | '-' -> build_sub lhs_val rhs_val "subtmp" builder
                        | '*' -> build_mul lhs_val rhs_val "multmp" builder
                        | '<' ->
                            let i = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
                            build_intcast i i8_type "boolcasttmp" builder
                        | _ -> raise (Error "Invalid binary operator")
                    end
                | "i64" ->
                    begin
                        match op with
                        | '+' -> build_add lhs_val rhs_val "addtmp" builder
                        | '-' -> build_sub lhs_val rhs_val "subtmp" builder
                        | '*' -> build_mul lhs_val rhs_val "multmp" builder
                        | '<' ->
                            let i = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
                            build_intcast i i64_type "boolcasttmp" builder
                        | _ -> raise (Error "Invalid binary operator")
                    end
                | _ -> raise (Error "Unknown error: type unknown")
            else raise (Error "Mismatch type")
        end
    *)
    | Ast.Binary (op, lhs, rhs) ->
        let lhs_val = codegen_expr lhs in
        let rhs_val = codegen_expr rhs in
        begin
            match op with
            | '+' -> build_add lhs_val rhs_val "addtmp" builder
            | '-' -> build_sub lhs_val rhs_val "subtmp" builder
            | '*' -> build_mul lhs_val rhs_val "multmp" builder
            | '<' ->
                let i = build_icmp Icmp.Slt lhs_val rhs_val "cmptmp" builder in
                build_intcast i i64_type "boolcasttmp" builder
            | _ ->
                let call = "binary" ^ (String.make 1 op) in
                let call =
                    match lookup_function call kmodule with
                    | Some call -> call
                    | None -> raise (Error "Invalid binary operator")
                in
                build_call call [| lhs_val; rhs_val |] "binop" builder
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
    | Ast.If (cond, else_expr, then_expr) ->
        let cond = codegen_expr cond in
        let cond_val =
            match string_of_lltype (type_of cond) with
            | "double" -> build_fcmp Fcmp.One cond (const_float double_type 0.0) "ifcond" builder
            | "i8" -> build_icmp Icmp.Ne cond (const_int i8_type 0) "ifcond" builder
            | "i64" -> build_icmp Icmp.Ne cond (const_int i64_type 0) "ifcond" builder
            | _ -> raise (Error "Unknown error: type unknown")
        in
        let start_block = insertion_block builder in
        let func = block_parent start_block in
        
        let then_block = append_block context "then" func in
        position_at_end then_block builder;
        let then_val = codegen_expr then_expr in
        let new_then_block = insertion_block builder in

        let else_block = append_block context "else" func in
        position_at_end else_block builder;
        let else_val = codegen_expr else_expr in
        let new_else_block = insertion_block builder in

        let merge_block = append_block context "ifcont" func in
        position_at_end merge_block builder;
        let incoming = [(then_val, new_then_block); (else_val, new_else_block)] in
        let phi = build_phi incoming "iftmp" builder in
        position_at_end start_block builder;
        ignore(build_cond_br cond_val then_block else_block builder);

        position_at_end new_then_block builder; ignore (build_br merge_block builder);
        position_at_end new_else_block builder; ignore (build_br merge_block builder);

        position_at_end merge_block builder;

        phi
    | Ast.For (identifier, assign, cond, step, body) ->
        let assign_val = codegen_expr assign in
        let preheader_block = insertion_block builder in
        let func = block_parent preheader_block in
        let loop_block = append_block context "loop" func in
        ignore(build_br loop_block builder);

        position_at_end loop_block builder;

        let variable = build_phi [(assign_val, preheader_block)] identifier builder in
        let old_val =
            try
                Some (Hashtbl.find named_values identifier)
            with
                Not_found -> None
        in
        Hashtbl.add named_values identifier variable;

        ignore (codegen_expr body);

        let step_val =
            match step with
            | Some step -> codegen_expr step
            | None ->
                match string_of_lltype (type_of assign_val) with
                | "double" -> const_float double_type 0.0
                | "i8" -> const_int i8_type 0
                | "i64" -> const_int i64_type 0
                | _ -> raise (Error "Unknown error: type unknown")
        in
        let next_var = build_add variable step_val "nextval" builder in
        let end_cond = codegen_expr cond in
        let end_cond =
            match string_of_lltype (type_of step_val) with
            | "double" -> build_fcmp Fcmp.One end_cond (const_float double_type 0.0) "loopcond" builder
            | "i8" -> build_icmp Icmp.Ne end_cond (const_int i8_type 0) "loopcond" builder
            | "i64" -> build_icmp Icmp.Ne end_cond (const_int i64_type 0) "loopcond" builder
            | _ -> raise (Error "Unknown error: type unknown")
        in
        let loop_end_block = insertion_block builder in 
        let after_block = append_block context "afterloop" func in
        ignore(build_cond_br end_cond loop_block after_block builder);

        position_at_end after_block builder;

        add_incoming (next_var, loop_end_block) variable;

        begin
            match old_val with
            | Some old_val -> Hashtbl.add named_values identifier old_val
            | None -> ()
        end;

        match string_of_lltype (type_of assign_val) with
        | "double" -> const_float double_type 0.0
        | "i8" -> const_int i8_type 0
        | "i64" -> const_int i64_type 0
        | _ -> raise (Error "Unknown error: type unknown")

let codegen_proto = function
    | Ast.Prototype (name, arguments, funtype) | Ast.BinaryPrototype (name, arguments, _, funtype) ->
        let rec create_arguments_array index args =
            if (Array.length arguments) > index
            then
                let argtype =
                    match arguments.(index) with
                    | Ast.Argument (_, argtype') -> argtype'
                in
                create_arguments_array (index + 1) ((codegen_type argtype) :: args)
            else args
        in
        let arguments' = Array.of_list (List.rev (create_arguments_array 0 [])) in
        let ft = function_type (codegen_type funtype) arguments' in
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
            let n =
                match arguments.(i) with
                | Ast.Argument (n', _) -> n'
            in
            set_value_name n a;
            Hashtbl.add named_values n a;
        ) (params f);
        f

let codegen_func optimizer = function
    | Ast.Function (proto, body) ->
        Hashtbl.clear named_values;
        let func = codegen_proto proto in

        begin
            match proto with
            | Ast.BinaryPrototype (name, _, precedence, _) ->
                let op = name.[(String.length name) - 1] in
                Hashtbl.add Parser.binop_precedence op precedence;
            | _ -> ()
        end;

        let block = append_block context "entry" func in
        position_at_end block builder;
        try
            let ret_val = codegen_expr body in
            let _ = build_ret ret_val builder in
            assert_valid_function func;
            let _ = PassManager.run_function func optimizer in
            func
        with
        | e ->
            delete_function func;
            raise e
