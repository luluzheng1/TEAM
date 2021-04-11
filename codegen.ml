module L = Llvm
module A = Ast
open Sast 
module E = Exceptions
module StringMap = Map.Make(String)

type var_table =
{ lvariables: L.llvalue StringMap.t;
  parent: var_table ref option}
  
let translate (functions, statements) =
  let main_func = {styp = A.Int; sfname = "main"; sformals = []; sbody = statements} in
  let functions = [main_func]@functions in
  let context    = L.global_context () in

  let i32_t      = L.i32_type     context
  and char_t     = L.i8_type      context
  and i8_t       = L.i8_type      context
  and void_t     = L.void_type    context 
  and float_t    = L.double_type  context
  and i1_t       = L.i1_type      context
  and string_t   = L.pointer_type (L.i8_type context)

  and the_module = L.create_module context "TEAM" in

  let list_struct_type = L.named_struct_type context "list_item" in
  let list_struct_ptr = L.pointer_type list_struct_type in
  let _ = L.struct_set_body list_struct_type [|L.pointer_type i8_t; list_struct_ptr|] true in

  (* Convert TEAM types to LLVM types *)
  let ltype_of_typ = function
      A.Int     -> i32_t
    | A.String  -> string_t
    | A.Bool    -> i1_t
    | A.Float   -> float_t
    | A.Void    -> void_t
    | A.Char    -> char_t
    | A.Unknown -> void_t
    | A.List(_) -> list_struct_ptr
    | _         -> raise (Failure "Type is not implemented yet!")
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in
  
  let var_table = {lvariables = StringMap.empty; parent = None} in
  let globals = ref var_table in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  let build_function_body scope fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let rec lookup sc n = try StringMap.find n !sc.lvariables
      with Not_found -> (match !sc.parent with
          None -> raise (Failure "internal error: variable not in scope")
        | Some t -> lookup t n)
    in

    let add_variable_to_scope sc n v = sc :=
        {lvariables = StringMap.add n v !sc.lvariables; parent = !sc.parent} in

    let formals =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
	      let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
	      StringMap.add n local m 
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function)) 
    in

    let scope = match fdecl.sfname with
        "main" -> scope
      | _      -> ref {lvariables = formals; parent= Some scope }
    in 

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None   -> ignore (instr builder) 
    in

    let get_list_inner_typ = function
        A.List(t) -> t
      | _         -> raise(Failure "Internal error: Not a list type")
    in

    (* TODO: make sure build_access_function is only defined once *)
    let rec expr sc builder ((t, e) : sexpr) = match e with
        SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SNoexpr -> L.const_int i32_t 0
      | SStringLit s ->  L.build_global_stringptr s "string" builder
      | SCall ("print", [e]) ->
          L.build_call printf_func [| (expr sc builder e) |] "printf" builder
      | SCall (f, args) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
          let llargs = List.rev (List.map (expr sc builder) (List.rev args)) in
          let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
          L.build_call fdef (Array.of_list llargs) result builder 
      | SId n -> L.build_load (lookup sc n) n builder
      | SListLit l -> build_list t l sc builder
      | SSliceExpr (id, slice) -> (
        match t with
        | A.String -> (
          match slice with
          | SIndex i ->
              let str = L.build_load (lookup sc id) "get_string" builder in
              let ptr =
                L.build_gep str [|expr sc builder i|] "get_char_ptr" builder
              in
              L.build_load ptr "get_char" builder
          | SSlice (i, j) ->
              let str = L.build_load (lookup sc id) "get_string" builder in
              let ptr =
                L.build_gep str [|expr sc builder i|] "get_char_ptr" builder
              in
              let length =
                L.build_sub (expr sc builder j) (expr sc builder i) "subb"
                  builder
              in
              let length_w_nul =
                L.build_add length ((L.const_int i8_t) 1) "length_w_nul" builder
              in
              let new_str =
                L.build_array_alloca i8_t length_w_nul "new_string" builder
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
        | _ -> (
          match slice with
          | SIndex i ->
              let la_func = build_access_function () in
              let lis = L.build_load (lookup sc id) "get_list" builder in
              let item_ptr =
                L.build_call la_func
                  [|lis; expr sc builder i|]
                  (id ^ "_result") builder
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
              let lis = L.build_load (lookup sc id) "get_list" builder in
              let i = expr sc builder i in
              let item_ptr =
                L.build_call la_func [|lis; i|] (id ^ "start") builder
              in
              let j =
                match j with
                | _, SEnd -> L.const_int i32_t (-1)
                | _ -> L.build_sub (expr sc builder j) i "difference" builder
              in
              let lc_func = build_copy_function t in
              let new_list_ptr =
                L.build_alloca list_struct_ptr "new_list_ptr" builder
              in
              let _ =
                L.build_call lc_func [|item_ptr; j; new_list_ptr|] "" builder
              in
              L.build_load new_list_ptr "new_string" builder )
        | tes ->
            let _ = print_endline (string_of_sexpr (t, e)) in
            raise (Failure "yppppppp") )
      | SListAssign (id, i, value) ->
          let inner_t = get_list_inner_typ t in
          let la_func = build_access_function () in
          let lis = L.build_load (lookup sc id) "get_list" builder in
          let item_ptr =
            L.build_call la_func
              [|lis; expr sc builder i|]
              (id ^ "_result") builder
          in
          let data_ptr_ptr =
            L.build_struct_gep item_ptr 0 "data_ptr_ptr" builder
          in
          let copy_data_ptr =
            L.build_alloca (ltype_of_typ inner_t) "copy_ptr" builder
          in
          let data = expr sc builder value in
          let _ = L.build_store data copy_data_ptr builder in
          let type_casted_copy =
            L.build_bitcast copy_data_ptr (L.pointer_type i8_t) "cast_copy"
              builder
          in
          let _ = L.build_store type_casted_copy data_ptr_ptr builder in
          L.const_int i32_t 0
      | _ -> L.const_int i32_t 0

    and build_copy_function typ =
      let t = get_list_inner_typ typ in
      let func_name = "list_copy_" ^ A.string_of_typ t in
      match L.lookup_function func_name the_module with
      | Some func -> func
      | None ->
          let lc_func_t =
            L.function_type void_t
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
          let _ = L.build_ret_void (L.builder_at_end context then_bb) in
          let else_bb = L.append_block context "else" lc_func in
          let else_builder = L.builder_at_end context else_bb in
          let new_struct_ptr =
            L.build_alloca list_struct_type "new_struct_ptr" else_builder
          in
          (* let _ = L.build_store (L.const_null list_struct_type)
             new_struct_ptr else_builder in *)
          let data_ptr = L.build_alloca (ltype_of_typ t) "ltyp" else_builder in
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
          let _ = L.build_call lc_func [|next; sub; ptr_ptr|] "" else_builder in
          let _ = L.build_ret_void else_builder in
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
        let entry_ptr = L.build_alloca list_struct_type "list_item" builder in
        (* let _ = L.build_store (L.const_null list_struct_type) entry_ptr
            builder in *)
        let data_ptr = L.build_alloca ltyp "copied" builder in
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
    
    let rec stmt sc builder = function
	      SBlock sl -> 
          let new_scope = ref {lvariables = StringMap.empty; parent = Some sc} in
          List.fold_left (stmt new_scope) builder sl
      | SExpr e -> let _ = expr sc builder e in builder 
      | SDeclaration (t, n, s) ->
        let _ =
          match fdecl.sfname with
          | "main" ->
              let global =
                L.define_global n (L.const_null (ltype_of_typ t)) the_module
              in
              let _ = L.build_store (expr sc builder s) global builder in
              add_variable_to_scope sc n global
          | _ ->
              let local = L.build_alloca (ltype_of_typ t) n builder in
              let _ = L.build_store (expr sc builder s) local builder in
              add_variable_to_scope sc n local
        in
        builder
      | SWhile (predicate, body) ->
        let pred_bb = L.append_block context "while" the_function in
        let _ = L.build_br pred_bb builder in

        let body_bb = L.append_block context "while_body" the_function in
        let while_builder = stmt sc (L.builder_at_end context body_bb) body in
        let () = add_terminal while_builder (L.build_br pred_bb) in

        let pred_builder = L.builder_at_end context pred_bb in
        let bool_val = expr sc pred_builder predicate in

        let merge_bb = L.append_block context "merge" the_function in
        let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
        L.builder_at_end context merge_bb
      | SReturn e -> let _ = match fdecl.styp with
            A.Void -> L.build_ret_void builder 
          | _ -> L.build_ret (expr sc builder e) builder 
        in builder

      | _ -> builder

    in
    let builder = List.fold_left (stmt scope) builder fdecl.sbody
  in

    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter (build_function_body globals) functions;
  the_module