module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)
let translate (functions, statements) =
  let main_func = {styp = Int; sfname = "main"; sformals = []; sbody = statements} in
  let functions = functions@[main_func] in
  let context    = L.global_context () in

  let i32_t      = L.i32_type    context
  and string_t   = L.pointer_type (L.i8_type context)
  and i8_t       = L.i8_type     context
  and void_t     = L.void_type   context 

  and the_module = L.create_module context "TEAM" in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.String -> string_t
    | A.Void  -> void_t
  in

  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in

  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
	      Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let rec expr builder ((_, e) : sexpr) = match e with
        SIntLit i -> L.const_int i32_t i
      | SNoexpr -> L.const_int i32_t 0
      | SStringLit s ->  L.build_global_stringptr s "string" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| (expr builder e) |] "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (expr builder) (List.rev args)) in
        let result = (match fdecl.styp with 
                       A.Void -> ""
                     | _ -> f ^ "_result") in
        L.build_call fdef (Array.of_list llargs) result builder 
      (* | SDeclaration (t, n, s) -> builder Gotta handle global variables*)
      | _ -> L.const_int i32_t 0
    in
    
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) 
    in
	
    let rec stmt builder = function
	      SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> let _ = expr builder e in builder 
      | _ -> builder
    in

    let builder = stmt builder (SBlock fdecl.sbody) 
  in

    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in
  List.iter build_function_body functions;
  the_module
