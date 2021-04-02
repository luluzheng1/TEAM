module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

type var_table =
{ variables: L.llvalue StringMap.t;
  parent: var_table ref option}
  
let translate (functions, statements) =
  let main_func = {styp = Int; sfname = "main"; sformals = []; sbody = statements} in
  let functions = [main_func]@functions in
  let context    = L.global_context () in

  let i32_t      = L.i32_type    context
  and string_t   = L.pointer_type (L.i8_type context)
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and void_t     = L.void_type   context 

  and the_module = L.create_module context "TEAM" in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.String -> string_t
    | A.Void  -> void_t
    | A.Bool  -> i1_t
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in
  
  let var_table = {variables = StringMap.empty; parent = None} in
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

    let rec lookup sc n = try StringMap.find n !sc.variables
      with Not_found -> (match !sc.parent with
          None -> raise (Failure "internal error: variable not in scope")
        | Some t -> lookup t n)
    in

    let add_variable_to_scope sc n v = sc :=
        {variables = StringMap.add n v !sc.variables; parent = !sc.parent} in

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
      | _ -> ref {variables = formals; parent= Some scope }
    in 

    let rec expr sc builder ((_, e) : sexpr) = match e with
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
      | _ -> L.const_int i32_t 0
    in
    
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) 
    in
	
    let rec stmt sc builder = function
	      SBlock sl -> 
          let new_scope = ref {variables = StringMap.empty; parent = Some sc} in
          List.fold_left (stmt new_scope) builder sl
      | SExpr e -> let _ = expr sc builder e in builder 
      | SDeclaration (t, n, s) -> let _ = (match fdecl.sfname with
          "main" -> add_variable_to_scope sc n (L.define_global n (expr sc builder s) the_module)
        | _ -> let local = L.build_alloca (ltype_of_typ t) n builder in
               let _  = L.build_store (expr sc builder s) local builder in 
               add_variable_to_scope sc n local)
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
