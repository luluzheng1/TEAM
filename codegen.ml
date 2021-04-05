module L = Llvm
module A = Ast
module E = Exceptions
open Sast
module StringMap = Map.Make (String)

type var_table =
  {lvariables: L.llvalue StringMap.t; parent: var_table ref option}

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
  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.Char -> char_t
    | A.Unknown -> void_t
    | _ -> void_t
  in
  let printf_t : L.lltype = L.function_type i32_t [|L.pointer_type i8_t|] in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  let substring_t = L.function_type string_t [|string_t; i32_t; i32_t|] in
  let substring_func =
    L.declare_function "substring" substring_t the_module
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
    let rec expr sc builder ((t, e) : sexpr) =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c -> L.const_int char_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "string" builder
      | SListLit _ -> raise (Failure "Not Yet Implemented")
      | SId n -> L.build_load (find_variable sc n) n builder
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
      | SAssign (s, e) ->
          let _ = update_variable sc s e builder in
          expr sc builder (t, SId s)
      | SListAssign _ -> raise (Failure "Not Yet Implemented")
      | SAssignOp (s, op, e) ->
          expr sc builder (t, SAssign (s, (t, SBinop ((t, SId s), op, e))))
      | SCall ("print", [e]) ->
          L.build_call printf_func [|expr sc builder e|] "printf" builder
      | SCall ("printb", [e]) ->
          L.build_call printf_func
            [|int_format_str; expr sc builder e|]
            "printf" builder
      | SCall ("printf", [e]) ->
          L.build_call printf_func
            [|float_format_str; expr sc builder e|]
            "printf" builder
      | SCall (f, args) ->
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (expr sc builder) (List.rev args))
          in
          let result =
            match fdecl.styp with A.Void -> "" | _ -> f ^ "_result"
          in
          L.build_call fdef (Array.of_list llargs) result builder
      | SSliceExpr (s, slce) -> (
        match t with
        | A.String -> (
          match slce with
          | SIndex e -> raise (Failure "Not Yet Implemented")
          | SSlice (e1, e2) ->
              let s' = expr sc builder (t, SId s)
              and e1' = expr sc builder e1
              and e2' = expr sc builder e2 in
              L.build_call substring_func [|s'; e1'; e2'|] "substring"
                builder )
        | A.List _ -> raise (Failure "Not Yet Implemented")
        | _ -> raise (Failure "Internal Error") )
      | SNoexpr -> L.const_int i32_t 0
      | _ -> L.const_int i32_t 0
    and add_variable sc t n e builder =
      let e' = expr sc builder e in
      let _ = L.set_value_name n e' in
      let ltype = ltype_of_typ t in
      let l_var = L.build_alloca ltype n builder in
      let _ = L.build_store e' l_var builder in
      sc :=
        {lvariables= StringMap.add n l_var !sc.lvariables; parent= !sc.parent}
    and update_variable sc n e builder =
      let e' = expr sc builder e in
      let l_var =
        try find_variable sc n with Not_found -> raise (E.NotFound n)
      in
      let _ = L.build_store e' l_var builder in
      sc :=
        {lvariables= StringMap.add n l_var !sc.lvariables; parent= !sc.parent}
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    let rec stmt sc builder = function
      | SBlock sl ->
          let new_scope =
            ref {lvariables= StringMap.empty; parent= Some sc}
          in
          List.fold_left (stmt new_scope) builder sl
      | SExpr e ->
          let _ = expr sc builder e in
          builder
      | SDeclaration (t, n, e) ->
          let _ = add_variable sc t n e builder in
          builder
      | SWhile (predicate, body) ->
          let pred_bb = L.append_block context "while" the_function in
          let _ = L.build_br pred_bb builder in
          let body_bb = L.append_block context "while_body" the_function in
          let while_builder =
            stmt sc (L.builder_at_end context body_bb) body
          in
          let () = add_terminal while_builder (L.build_br pred_bb) in
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val = expr sc pred_builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
          L.builder_at_end context merge_bb
      | _ -> builder
    in
    let builder = List.fold_left (stmt scope) builder fdecl.sbody in
    add_terminal builder
      ( match fdecl.styp with
      | A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0) )
  in
  let functions' =
    try List.iter (build_function_body globals) functions
    with e -> E.handle_error e
  in
  functions' ; the_module
