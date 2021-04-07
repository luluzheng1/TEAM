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
      | _ -> ref {lvariables = formals; parent= Some scope }
    in 

    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) 
    in

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

      | SSliceExpr (id, slice) -> (match slice with
            SIndex i -> let f = build_access_function t in
                        let lis = L.build_load (lookup sc id) "temp" builder in
                        L.build_call f [|lis; expr sc builder i|] (id ^ "_result") builder
          | SSlice (index_a, index_b) -> raise(Failure("Nooooo."))
          | _ -> raise(Failure("Invalid types while accessing")))
          
      | _ -> L.const_int i32_t 0
    
    and build_access_function ty =
      let la_function_t = 
        (L.function_type (ltype_of_typ ty) [|list_struct_ptr; i32_t|])
      in
      let la_function =
        L.define_function "list_access" la_function_t the_module
      in
      let la_builder = L.builder_at_end context (L.entry_block la_function) in
      let curr_ptr = L.build_alloca list_struct_ptr "curr_pointer" la_builder in
      let _ = L.build_store (L.param la_function 0) curr_ptr la_builder in
      let target_idx = L.build_alloca i32_t "index" la_builder in
      let _ = L.build_store (L.param la_function 1) target_idx la_builder in

      let pred_bb = L.append_block context "while" la_function in
      let _ = L.build_br pred_bb la_builder in

      let body_bb = L.append_block context "while_body" la_function in
      let while_builder = L.builder_at_end context body_bb in

      let aasdf = L.build_load curr_ptr "asdf" while_builder in 
      let next_ptr_ptr = L.build_struct_gep aasdf 1 "next" while_builder in
      let next_ptr = L.build_load next_ptr_ptr "struct" while_builder in
      let _ = L.build_store next_ptr curr_ptr while_builder in
      let sub = L.build_sub (L.build_load target_idx "asdfasdf" while_builder) (L.const_int i32_t 1) "tmp" while_builder in
      let _ = L.build_store sub target_idx while_builder in
      let () = add_terminal while_builder (L.build_br pred_bb) in

      let pred_builder = L.builder_at_end context pred_bb in

      let bool_val = L.build_icmp L.Icmp.Ne (L.build_load target_idx "axcf" pred_builder) (L.const_int i32_t 0) "tmp" pred_builder in
      let merge_bb = L.append_block context "merge" la_function in
      let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in 

      let la_builder = L.builder_at_end context merge_bb in
      let cat = L.build_struct_gep (L.build_load curr_ptr "asdfasdf" la_builder) 0 "data" la_builder in
      let dat = L.build_load cat "temp" la_builder in
      let hat = L.build_bitcast dat (L.pointer_type (ltype_of_typ ty)) "awewre" la_builder in
      let mat = L.build_load hat "yolo" la_builder in

      let _ = L.build_ret mat la_builder in
    la_function

    (* MARK: has to double check this function *)
    (* TODO: change new_entry *)
    and build_list list_typ lis (scope: var_table ref) builder =
      let A.List(typ) = list_typ in
      let ltyp = ltype_of_typ typ in
      let build_link prev data =
        let new_entry = 
          L.const_named_struct list_struct_type 
            [| L.const_pointer_null void_t; L.const_pointer_null void_t |]
        in
        let entry_ptr = L.build_alloca list_struct_type "LIST_ITEM" builder in
        let _ = L.build_store new_entry entry_ptr builder in

        let item_ptr = L.build_alloca ltyp "COPIED" builder in
        let _ = L.build_store (expr scope builder data) item_ptr builder in
        let typcast_ptr = L.build_bitcast item_ptr (L.pointer_type i8_t) "werte" builder in
        let ewt = L.build_struct_gep entry_ptr 0 "werytu" builder in
        let _ = L.build_store typcast_ptr ewt builder in

        let next = L.build_struct_gep entry_ptr 1 "NEXT" builder in
        let _ = L.build_store prev next builder
      in entry_ptr
      in let null_ptr = L.const_pointer_null list_struct_ptr 
    in List.fold_left build_link null_ptr (List.rev lis)
    in
    
	
    let rec stmt sc builder = function
	      SBlock sl -> 
          let new_scope = ref {lvariables = StringMap.empty; parent = Some sc} in
          List.fold_left (stmt new_scope) builder sl
      | SExpr e -> let _ = expr sc builder e in builder 
      | SDeclaration (t, n, s) -> let _ = (match fdecl.sfname with
          "main" -> add_variable_to_scope sc n (L.define_global n (expr sc builder s) the_module)
        | _ -> let local = L.build_alloca (ltype_of_typ t) n builder in
               let _  = L.build_store (expr sc builder s) local builder in 
               add_variable_to_scope sc n local)
        in builder
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