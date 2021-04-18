module L = Llvm
module A = Ast
open Sast 
module E = Exceptions
module StringMap = Map.Make(String)

type var_table =
{ lvariables: L.llvalue StringMap.t;
  parent: var_table ref option}
  
let translate (functions, statements) =
  let main_func =
    {styp= A.Int; sfname= "main"; sformals= []; sbody= statements}
  in
  let functions = [main_func] @ functions in
  let context = L.global_context () in
  let i32_t = L.i32_type context
  and char_t = L.i8_type context
  and i8_t = L.i8_type context
  and void_t = L.void_type context
  and float_t = L.double_type context
  and i1_t = L.i1_type context
  and string_t = L.pointer_type (L.i8_type context)
  and the_module = L.create_module context "TEAM" in
  let list_struct_type = L.named_struct_type context "list_item" in
  let list_struct_ptr = L.pointer_type list_struct_type in
  let _ =
    L.struct_set_body list_struct_type
      [|L.pointer_type i8_t; list_struct_ptr|]
      true
  in
  (* Convert MicroC types to LLVM types *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.Char -> char_t
    | A.Unknown -> void_t
    | A.Func (args_t, ret_t) -> func_ty args_t ret_t
    | A.List _ -> list_struct_ptr
    | _ -> void_t
  and func_ty args_t ret_t =
    let llret_type = ltype_of_typ ret_t in
    let llargs =
      Array.map (fun t -> ltype_of_typ t) (Array.of_list args_t)
    in
    L.pointer_type (L.function_type llret_type llargs)
  in
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [|L.pointer_type i8_t|]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  let pow_t : L.lltype = L.function_type float_t [|float_t; float_t|] in
  let pow_func : L.llvalue = L.declare_function "pow" pow_t the_module in
  let var_table = {lvariables= StringMap.empty; parent= None} in
  let globals = ref var_table in
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list
          (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in
  let build_function_body scope fdecl =
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in
    let rec find_variable sc n =
      try StringMap.find n !sc.lvariables
      with Not_found -> (
        match !sc.parent with
        | None -> raise (E.NotFound n)
        | Some t -> find_variable t n )
    in
    let formals =
      let add_formal m (t, n) p =
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n builder in
        let _ = L.build_store p local builder in
        StringMap.add n local m
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function))
    in

    let scope =
      match fdecl.sfname with
      | "main" -> scope
      | _ -> ref {lvariables= formals; parent= Some scope}
    in

    let get_list_inner_typ = function
      A.List(t) -> t
      | _         -> raise(Failure "Internal error: Not a list type")
    in

    let rec expr sc builder ((t, e) : sexpr) =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c -> L.const_int char_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "string" builder
      | SId n -> L.build_load (find_variable sc n) n builder
      | SListLit l -> build_list t l sc builder
      | SSliceExpr (lexpr, slice) ->
        (let (lt, _) = lexpr in
        let l = expr sc builder lexpr in
        match lt with
        | A.String -> (
          match slice with
          | SIndex i ->
              let ptr =
                L.build_gep l [|expr sc builder i|] "get_char_ptr" builder
              in
              L.build_load ptr "get_char" builder
          | SSlice (i, j) ->
              let ptr =
                L.build_gep l [|expr sc builder i|] "get_char_ptr" builder
              in
              let length =
                L.build_sub (expr sc builder j) (expr sc builder i) "subb"
                  builder
              in
              let length_w_nul =
                L.build_add length ((L.const_int i8_t) 1) "length_w_nul" builder
              in
              let new_str =
                L.build_array_malloc i8_t length_w_nul "new_string" builder
              in
              let nul = L.build_gep new_str [|length|] "string_term" builder in
              let _ = L.build_store ((L.const_int i8_t) 0) nul builder in
              let mmcpy_t =
                L.function_type void_t
                  [|L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t|]
              in
              let mmcpy = L.declare_function "llvm.memcpy" mmcpy_t the_module in
              let _ =
                L.build_call mmcpy
                  [|new_str; ptr; length; (L.const_int i1_t) 1|]
                  "" builder
              in
              new_str )
        | _ -> 
          (match slice with
          | SIndex i ->
              let la_func = build_access_function () in
              let item_ptr =
                L.build_call la_func
                  [|l; expr sc builder i|]
                  "_result" builder
              in
              let data_ptr_ptr =
                L.build_struct_gep item_ptr 0 "data_ptr_ptr" builder
              in
              let dat_ptr = L.build_load data_ptr_ptr "data_ptr" builder in
              let type_casted =
                L.build_bitcast dat_ptr
                  (L.pointer_type (ltype_of_typ t))
                  "cast_data_ptr" builder
              in
              L.build_load type_casted "data" builder
          | SSlice (i, j) ->
              let la_func = build_access_function () in
              let i = expr sc builder i in
              let item_ptr =
                L.build_call la_func [|l; i|] "start" builder
              in
              let j =
                match j with
                | _, SEnd -> L.const_int i32_t (-1)
                | _ -> L.build_sub (expr sc builder j) i "difference" builder
              in
              let lc_func = build_copy_function t in
              let new_list_ptr =
                L.build_malloc list_struct_ptr "new_list_ptr" builder
              in
              let _ =
                L.build_call lc_func [|item_ptr; j; new_list_ptr|] "" builder
              in
              L.build_load new_list_ptr "new_string" builder ))
      
      | SBinop (e1, op, e2) ->
          let t1, _ = e1
          and t2, _ = e2
          and e1' = expr sc builder e1
          and e2' = expr sc builder e2 in
          if t1 = A.Float && t2 = A.Int then
            let cast_e2' = L.const_sitofp e2' float_t in
            match op with
            | A.Add -> L.build_fadd e1' cast_e2' "tmp" builder
            | A.Sub -> L.build_fsub e1' cast_e2' "tmp" builder
            | A.Mult -> L.build_fmul e1' cast_e2' "tmp" builder
            | A.Div -> L.build_fdiv e1' cast_e2' "tmp" builder
            | A.Exp -> L.build_call pow_func [|e1'; cast_e2'|] "exp" builder
            | A.Equal -> L.build_fcmp L.Fcmp.Oeq e1' cast_e2' "tmp" builder
            | A.Neq -> L.build_fcmp L.Fcmp.One e1' cast_e2' "tmp" builder
            | A.Less -> L.build_fcmp L.Fcmp.Olt e1' cast_e2' "tmp" builder
            | A.Leq -> L.build_fcmp L.Fcmp.Ole e1' cast_e2' "tmp" builder
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' cast_e2' "tmp" builder
            | A.Geq -> L.build_fcmp L.Fcmp.Oge e1' cast_e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else if t1 = A.Int && t2 = A.Float then
            let cast_e1' = L.const_sitofp e1' float_t in
            match op with
            | A.Add -> L.build_fadd cast_e1' e2' "tmp" builder
            | A.Sub -> L.build_fsub cast_e1' e2' "tmp" builder
            | A.Mult -> L.build_fmul cast_e1' e2' "tmp" builder
            | A.Div -> L.build_fdiv cast_e1' e2' "tmp" builder
            | A.Exp -> L.build_call pow_func [|cast_e1'; e2'|] "exp" builder
            | A.Equal -> L.build_fcmp L.Fcmp.Oeq cast_e1' e2' "tmp" builder
            | A.Neq -> L.build_fcmp L.Fcmp.One cast_e1' e2' "tmp" builder
            | A.Less -> L.build_fcmp L.Fcmp.Olt cast_e1' e2' "tmp" builder
            | A.Leq -> L.build_fcmp L.Fcmp.Ole cast_e1' e2' "tmp" builder
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt cast_e1' e2' "tmp" builder
            | A.Geq -> L.build_fcmp L.Fcmp.Oge cast_e1' e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else if t1 = A.Float && t2 = A.Float then
            match op with
            | A.Add -> L.build_fadd e1' e2' "tmp" builder
            | A.Sub -> L.build_fsub e1' e2' "tmp" builder
            | A.Mult -> L.build_fmul e1' e2' "tmp" builder
            | A.Div -> L.build_fdiv e1' e2' "tmp" builder
            | A.Exp -> L.build_call pow_func [|e1'; e2'|] "exp" builder
            | A.Equal -> L.build_fcmp L.Fcmp.Oeq e1' e2' "tmp" builder
            | A.Neq -> L.build_fcmp L.Fcmp.One e1' e2' "tmp" builder
            | A.Less -> L.build_fcmp L.Fcmp.Olt e1' e2' "tmp" builder
            | A.Leq -> L.build_fcmp L.Fcmp.Ole e1' e2' "tmp" builder
            | A.Greater -> L.build_fcmp L.Fcmp.Ogt e1' e2' "tmp" builder
            | A.Geq -> L.build_fcmp L.Fcmp.Oge e1' e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else if t1 = A.Int && t2 = A.Int then
            match op with
            | A.Add -> L.build_add e1' e2' "tmp" builder
            | A.Sub -> L.build_sub e1' e2' "tmp" builder
            | A.Mult -> L.build_mul e1' e2' "tmp" builder
            | A.Div -> L.build_sdiv e1' e2' "tmp" builder
            | A.Mod -> L.build_srem e1' e2' "tmp" builder
            | A.Exp ->
                let cast_e1' = L.const_sitofp e1' float_t
                and cast_e2' = L.const_sitofp e2' float_t in
                let result =
                  L.build_call pow_func [|cast_e1'; cast_e2'|] "exp" builder
                in
                L.build_fptosi result i32_t "result" builder
            | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | A.Less -> L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder
            | A.Leq -> L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder
            | A.Greater -> L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder
            | A.Geq -> L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder
            | A.Range -> raise (Failure "Not Yet Implemented")
            | _ -> raise E.InvalidIntBinop
          else if t1 = A.Bool && t2 = A.Bool then
            match op with
            | A.And -> L.build_and e1' e2' "tmp" builder
            | A.Or -> L.build_or e1' e2' "tmp" builder
            | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else raise (Failure "Not Yet Implemented")
      | SUnop (op, e) ->
          let t, _ = e in
          let e' = expr sc builder e in
          ( match op with
          | A.Neg when t = A.Float -> L.build_fneg
          | A.Neg -> L.build_neg
          | A.Not -> L.build_not )
            e' "tmp" builder
      | SAssign (le, re) ->
          let re' = expr sc builder re in
          let _ = match le with
            | (_, SId s) -> update_variable sc s re' builder
            | (_, SSliceExpr ((lst, ls), slc)) ->
                (match lst with
                  | A.List ilst -> let lis = expr sc builder (lst, ls) in
                      build_asn_list sc builder ilst lis slc re' 
                  | A.String -> raise (Failure "Not implemented yet")
                  | _-> raise (Failure "Internal Error"))   
            | _ -> raise (Failure "Internal Error")  
          in
          re'
      | SAssignOp (s, op, e) -> L.const_int i32_t 0
          (* expr sc builder (t, SAssign (s, (t, SBinop ((t, SId s), op, e)))) *)
          (* For testing purposes only, will need to combine into one
             function *)
      | SCall ("print", [e]) -> (
          let t, _ = e in
          match t with
          | A.String ->
              L.build_call printf_func [|expr sc builder e|] "printf" builder
          | A.Bool ->
              L.build_call printf_func
                [|int_format_str; expr sc builder e|]
                "printf" builder
          | A.Float ->
              L.build_call printf_func
                [|float_format_str; expr sc builder e|]
                "printf" builder
          | A.Int ->
              L.build_call printf_func
                [|int_format_str; expr sc builder e|]
                "printf" builder
          | _ ->
              raise
                (Failure
                   ( "Print for type " ^ A.string_of_typ t
                   ^ " not supported yet" ) ) )
      | SCall (f, args) ->
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (expr sc builder) (List.rev args))
          in
          let result =
            match fdecl.styp with A.Void -> "" | _ -> f ^ "_result"
          in
          L.build_call fdef (Array.of_list llargs) result builder
      | SEnd -> raise (Failure "Not Yet Implemented")
      | SNoexpr -> L.const_int i32_t 0
    
    and build_asn_list sc builder ilst lis slc re'  = match slc with
      | SIndex i ->
          let la_func = build_access_function () in
          let item_ptr =
            L.build_call la_func
              [|lis; expr sc builder i|]
              "result" builder
          in
          let data_ptr_ptr =
            L.build_struct_gep item_ptr 0 "data_ptpt" builder
          in
          let copy_data_ptr =
            L.build_malloc (ltype_of_typ ilst) "copy_ptr" builder
          in
          let _ = L.build_store re' copy_data_ptr builder in
          let type_casted_copy =
            L.build_bitcast copy_data_ptr (L.pointer_type i8_t) "ccopy"
              builder
          in let _ = L.build_store type_casted_copy data_ptr_ptr builder
          in ()
      | SSlice (i, j) ->
          let la_func = build_access_function () in
          let lc_func = build_copy_function (A.List ilst) in 
          let item_ptr =
            L.build_call la_func [|lis; expr sc builder i|] "result" builder
          in
          let end_ptr = L.build_call la_func [|lis; expr sc builder j|] "result" builder in
          let new_data_ptr_ptr = L.build_struct_gep item_ptr 0 "new" builder in
          let old_data_ptr = L.build_struct_gep re' 0 "old" builder in
          let _ = L.build_store (L.build_load old_data_ptr "t" builder) new_data_ptr_ptr builder in
          let renext = L.build_load (L.build_struct_gep re' 1 "n" builder) "sf" builder in
          let linext = L.build_struct_gep item_ptr 1 "adsf" builder in 
          let copy_end = L.build_call lc_func  [|renext; L.const_int i32_t (-1); linext|] "copied" builder in
          let _ = L.build_store end_ptr copy_end builder in ()

    and add_variable_to_scope sc n v =
      sc := {lvariables= StringMap.add n v !sc.lvariables; parent= !sc.parent}

    and update_variable sc (n:string) (e':L.llvalue) builder =
      let l_var =
        try find_variable sc n with Not_found -> raise (E.NotFound n)
      in
      let _ = L.build_store e' l_var builder in
      sc :=
        {lvariables= StringMap.add n l_var !sc.lvariables; parent= !sc.parent}

    and build_copy_function typ =
    let t = get_list_inner_typ typ in
    let func_name = "list_copy_" ^ A.string_of_typ t in
    match L.lookup_function func_name the_module with
    | Some func -> func
    | None ->
        let lc_func_t =
          L.function_type (L.pointer_type list_struct_ptr)
            [|list_struct_ptr; i32_t; L.pointer_type list_struct_ptr|]
        in
        let lc_func = L.define_function func_name lc_func_t the_module in
        let lc_builder = L.builder_at_end context (L.entry_block lc_func) in
        let i_cond =
          L.build_icmp L.Icmp.Eq (L.param lc_func 1) (L.const_int i32_t 0)
            "is_zero" lc_builder
        in
        let n_cond =
          L.build_is_null (L.param lc_func 0) "ptr_is_null" lc_builder
        in
        let bool_val = L.build_or i_cond n_cond "or_conds" lc_builder in
        let then_bb = L.append_block context "then" lc_func in
        let _ = L.build_ret (L.param lc_func 2) (L.builder_at_end context then_bb) in
        let else_bb = L.append_block context "else" lc_func in
        let else_builder = L.builder_at_end context else_bb in
        let new_struct_ptr =
          L.build_malloc list_struct_type "new_struct_ptr" else_builder
        in
        let _ = L.build_store (L.const_null list_struct_type)
           new_struct_ptr else_builder in
        let data_ptr = L.build_malloc (ltype_of_typ t) "ltyp" else_builder in
        let old_data_ptr_ptr =
          L.build_struct_gep (L.param lc_func 0) 0 "old_data_ptr_ptr"
            else_builder
        in
        let old_data_ptr =
          L.build_load old_data_ptr_ptr "old_data_ptr" else_builder
        in
        let old_data_ptr =
          L.build_bitcast old_data_ptr
            (L.pointer_type (ltype_of_typ t))
            "cast_old_data_ptr" else_builder
        in
        let old_data = L.build_load old_data_ptr "old_data" else_builder in
        let _ = L.build_store old_data data_ptr else_builder in
        let data_ptr_cast =
          L.build_bitcast data_ptr (L.pointer_type i8_t) "data_ptr_cast"
            else_builder
        in
        let _ =
          L.build_store data_ptr_cast
            (L.build_struct_gep new_struct_ptr 0 "store_new_data" else_builder)
            else_builder
        in
        let _ =
          L.build_store new_struct_ptr (L.param lc_func 2) else_builder
        in
        let ptr_ptr =
          L.build_struct_gep new_struct_ptr 1 "next" else_builder
        in
        let next_ptr =
          L.build_struct_gep (L.param lc_func 0) 1 "next_ptr" else_builder
        in
        let next = L.build_load next_ptr "next" else_builder in
        let sub =
          L.build_sub (L.param lc_func 1) (L.const_int i32_t 1) "sub"
            else_builder
        in
        let ret = L.build_call lc_func [|next; sub; ptr_ptr|] "" else_builder in
        let _ = L.build_ret ret else_builder in
        let _ = L.build_cond_br bool_val then_bb else_bb lc_builder in
        lc_func

    and build_access_function () =
      match L.lookup_function "list_access" the_module with
      | Some func -> func
      | None ->
          let la_func_t =
            L.function_type list_struct_ptr [|list_struct_ptr; i32_t|]
          in
          let la_func = L.define_function "list_access" la_func_t the_module in
          let la_builder = L.builder_at_end context (L.entry_block la_func) in
          let bool_val =
            L.build_icmp L.Icmp.Eq (L.param la_func 1) (L.const_int i32_t 0)
              "is_zero" la_builder
          in
          let then_bb = L.append_block context "then" la_func in
          let _ =
            L.build_ret (L.param la_func 0) (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" la_func in
          let else_builder = L.builder_at_end context else_bb in
          let next_ptr =
            L.build_struct_gep (L.param la_func 0) 1 "next_ptr" else_builder
          in
          let next = L.build_load next_ptr "next" else_builder in
          let sub =
            L.build_sub (L.param la_func 1) (L.const_int i32_t 1) "sub"
              else_builder
          in
          let ret = L.build_call la_func [|next; sub|] "result" else_builder in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb la_builder in
          la_func
  
    and build_list list_typ lis (scope : var_table ref) builder =
      let typ = get_list_inner_typ list_typ in
      let ltyp = ltype_of_typ typ in
      let build_link prev data =
        let entry_ptr = L.build_malloc list_struct_type "list_item" builder in
        let _ = L.build_store (L.const_null list_struct_type) entry_ptr
            builder in
        let data_ptr = L.build_malloc ltyp "copied" builder in
        let _ = L.build_store (expr scope builder data) data_ptr builder in
        let typcast_ptr =
          L.build_bitcast data_ptr (L.pointer_type i8_t) "cast_ptr" builder
        in
        let data_ptr_container =
          L.build_struct_gep entry_ptr 0 "data_ptr_container" builder
        in
        let _ = L.build_store typcast_ptr data_ptr_container builder in
        let next = L.build_struct_gep entry_ptr 1 "next" builder in
        let _ = L.build_store prev next builder in
        entry_ptr
      in
      let null_ptr = L.const_pointer_null list_struct_ptr in
      List.fold_left build_link null_ptr (List.rev lis)
    in

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in

    (* Statements *)
    let rec build_stmt sc builder stmt loop fdecl =
      match stmt with
      | SBlock sl ->
          let new_scope =
            ref {lvariables= StringMap.empty; parent= Some sc}
          in
          List.fold_left
            (fun b s -> build_stmt new_scope b s loop fdecl)
            builder sl
      | SExpr e ->
          let _ = expr sc builder e in
          builder
      | SReturn e ->
          let _ =
            match fdecl.styp with
            | A.Void -> L.build_ret_void builder
            | _ -> L.build_ret (expr scope builder e) builder
          in
          builder
      | SIf (predicate, then_stmts, else_if_stmts, else_stmts) ->
          (* Removing the elseifs by recursively replacing the
             else_statements *)
          let rec remove_elif (_, _, else_if_stmts, else_stmts) =
            match else_if_stmts with
            | SBlock (hd :: tl) ->
                let new_predicate, new_then =
                  match hd with
                  | SElif (predicate, stmts) -> (predicate, stmts)
                  | _ -> raise (Failure "Corrupted tree - Elseif problem")
                in
                let new_else_ifs = SBlock tl in
                let new_else =
                  remove_elif
                    (new_predicate, new_then, new_else_ifs, else_stmts)
                in
                SIf (new_predicate, new_then, new_else_ifs, new_else)
            | SBlock [] -> else_stmts
            | _ -> else_stmts
          in
          let new_else_stmts =
            remove_elif (predicate, then_stmts, else_if_stmts, else_stmts)
          in
          let bool_val = expr sc builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          (* Emit 'then' value. *)
          let then_bb = L.append_block context "then" the_function in
          let then_builder =
            build_stmt sc
              (L.builder_at_end context then_bb)
              then_stmts loop fdecl
          in
          let () = add_terminal then_builder (L.build_br merge_bb) in
          (* Emit 'else' value. *)
          let else_bb = L.append_block context "else" the_function in
          let else_builder =
            build_stmt sc
              (L.builder_at_end context else_bb)
              new_else_stmts loop fdecl
          in
          let () = add_terminal else_builder (L.build_br merge_bb) in
          (* Add the conditional branch. *)
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          L.builder_at_end context merge_bb
      | SElif _ -> raise E.ImpossibleElif
      | SFor _ -> builder
      (* | SFor (s, (t, e), sl) ->
          let list_identifier = "for_list" in
          let list_expr = (t, SId list_identifier) in
          let s_ty =
            match t with
            | A.List ty -> ty
            | _ -> raise (Failure "internal error")
          in
          let len_call = (A.Int, SCall ("length", [list_expr])) in
          let index_expr = (A.Int, SId "for_index") in
          let while_cond = (A.Bool, SBinop (index_expr, A.Less, len_call)) in
          let equivalent =
            SBlock
              [ SDeclaration (A.Int, "for_index", (A.Int, SIntLit 0))
              ; SDeclaration (t, list_identifier, (t, e))
              ; SDeclaration (s_ty, s, (s_ty, SNoexpr))
              ; SWhile
                  ( while_cond
                  , SBlock
                      [ SExpr
                          ( s_ty
                          , SAssign
                              ( s
                              , ( s_ty
                                , SSliceExpr
                                    (list_identifier, SIndex index_expr) ) )
                          )
                      ; SExpr
                          ( A.Int
                          , SAssign
                              ( "for_index"
                              , ( A.Int
                                , SBinop
                                    (index_expr, A.Add, (A.Int, SIntLit 1))
                                ) ) )
                      ; sl ] ) ]
          in
          build_stmt sc builder equivalent loop fdecl *)
      | SDeclaration (t, n, e) ->
          let _ =
            match fdecl.sfname with
            | "main" ->
                let global =
                  L.define_global n
                    (L.const_null (ltype_of_typ t))
                    the_module
                in
                let _ = L.build_store (expr sc builder e) global builder in
                add_variable_to_scope sc n global
            | _ ->
                let local = L.build_alloca (ltype_of_typ t) n builder in
                let _ = L.build_store (expr sc builder e) local builder in
                add_variable_to_scope sc n local
          in
          builder
      | SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          let _ = L.build_br pred_bb builder in
          let merge_bb = L.append_block context "merge" the_function in
          let body_bb = L.append_block context "while_body" the_function in
          let while_builder =
            build_stmt sc
              (L.builder_at_end context body_bb)
              body
              ((pred_bb, merge_bb) :: loop)
              fdecl
          in
          let () = add_terminal while_builder (L.build_br pred_bb) in
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr sc pred_builder predicate in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          L.builder_at_end context merge_bb
      | SBreak ->
          let () = add_terminal builder (L.build_br (snd (List.hd loop))) in
          builder
      | SContinue ->
          let () = add_terminal builder (L.build_br (fst (List.hd loop))) in
          builder
    in
    let builder =
      List.fold_left
        (fun b s -> build_stmt scope b s [] fdecl)
        builder fdecl.sbody
    in
    add_terminal builder
      ( match fdecl.styp with
      | A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0) )
  in
  let functions' =
    try List.iter (build_function_body globals) functions
    with e -> E.handle_error e
  in
  functions' ; the_module
