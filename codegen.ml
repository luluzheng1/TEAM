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

  let int_list_item_struct = L.named_struct_type context "int_list_item" in
  let float_list_item_struct = L.named_struct_type context "float_list_item" in
  let bool_list_item_struct = L.named_struct_type context "bool_list_item" in
  let string_list_item_struct = L.named_struct_type context "string_list_item" in 
  let char_list_item_struct = L.named_struct_type context "char_list_item" in 
  let unknown_list_item_struct = L.named_struct_type context "unknown_list_item" in

  let int_list_item_pointer =
    L.pointer_type int_list_item_struct
  in
  let float_list_item_pointer = 
    L.pointer_type float_list_item_struct
  in
  let bool_list_item_pointer =
    L.pointer_type bool_list_item_struct
  in
  let string_list_item_pointer =
    L.pointer_type string_list_item_struct
  in
  let char_list_item_pointer = 
    L.pointer_type char_list_item_struct
  in
  let unknown_list_item_pointer = 
    L.pointer_type unknown_list_item_struct
  in

  let pack_struct struct_type arg_list =
    L.struct_set_body struct_type (Array.of_list arg_list) true
  in

  let () =
    pack_struct int_list_item_struct [i32_t; int_list_item_pointer; i1_t]
  in
  let () = 
    pack_struct float_list_item_struct [float_t; float_list_item_pointer; i1_t]
  in
  let () =
    pack_struct bool_list_item_struct [i1_t; bool_list_item_pointer; i1_t]
  in
  let () =
    pack_struct string_list_item_struct [string_t; string_list_item_pointer; i1_t]
  in
  let () = 
    pack_struct char_list_item_struct [char_t; char_list_item_pointer; i1_t]
  in
  let () = 
    pack_struct unknown_list_item_struct [void_t; unknown_list_item_pointer; i1_t]
  in


  let get_list_type ty = match ty with
      A.Int     -> int_list_item_struct
    | A.Float   -> float_list_item_struct
    | A.Bool    -> bool_list_item_struct
    | A.String  -> string_list_item_struct
    | A.Char    -> char_list_item_struct
    | A.Unknown -> unknown_list_item_struct
    | _ as t    -> raise(E.InvalidListType t)
  in

  let get_list_pointer_type ty = match ty with
      A.Int     -> int_list_item_pointer
    | A.Float   -> float_list_item_pointer
    | A.Bool    -> bool_list_item_pointer
    | A.String  -> string_list_item_pointer
    | A.Char    -> char_list_item_pointer
    | A.Unknown -> unknown_list_item_pointer 
    | _ as t    -> raise(E.InvalidListType t)
  in

  (* Convert TEAM types to LLVM types *)
  let ltype_of_typ = function
      A.Int     -> i32_t
    | A.String  -> string_t
    | A.Bool    -> i1_t
    | A.Float   -> float_t
    | A.Void    -> void_t
    | A.Char    -> char_t
    | A.Unknown -> void_t
    | A.List(t) -> get_list_pointer_type t
    | _         -> raise (Failure "Type is not implemented yet!")
  in

  let list_end_item ty e1 =
    let list_item_struct = get_list_type ty in
    L.const_named_struct list_item_struct
      (Array.of_list [e1; 
                      L.const_pointer_null (L.pointer_type list_item_struct); 
                      L.const_int i1_t 0])
  in

  let list_terminator ty =
    let list_item_struct = get_list_type ty in
    L.const_named_struct list_item_struct 
      (Array.of_list [L.const_null (ltype_of_typ ty); 
                      L.const_pointer_null (L.pointer_type list_item_struct); 
                      L.const_int i1_t 1])
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

    let raw_list (_, e) = match e with
          SListLit s -> s
        | _ -> raise(Failure("invalide argument passed."))
    in

    let list_inner_type l = match l with
          A.List(ty) -> ty
        | _ as t     -> raise(E.InvalidListType t)
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
      | SListLit _ -> build_list (list_inner_type t) (t, e) sc builder
      | _ -> L.const_int i32_t 0

    (* MARK: has to double check this function *)
    and build_list t e (scope: var_table ref) builder =
      let list_item_struct = get_list_type t in
      let build_link temp_var b =
        let front_item = list_end_item t (expr scope builder b) in
        let head_var = L.build_alloca list_item_struct "LIST_ITEM" builder in
        let () =
          ignore(L.build_store front_item head_var builder)
        in
        let head_pointer =
          L.build_struct_gep head_var 1 "TEMP" builder
        in
        let () =
          ignore(L.build_store temp_var head_pointer builder )
        in
        head_var
      in
      let stripped_list = raw_list e
      in
      (* build an empty item to terminate the list *)
      let empty_expr =
        list_terminator t
      in
      let empty_var = L.build_alloca list_item_struct "TEMP" builder in
      let () =
        ignore(L.build_store empty_expr empty_var builder)
      in
      List.fold_left build_link empty_var (List.rev stripped_list)
    in
    
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) 
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