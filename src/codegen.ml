(* Authors: Naoki O., Yingjie L., Lulu Z., Saurav G. *)
module L = Llvm
module A = Ast
open Sast
module E = Exceptions
module StringMap = Map.Make (String)
(* module Array *)

type var_table =
  {lvariables: L.llvalue StringMap.t; parent: var_table ref option}

let translate (functions, statements) =
  (* defining main function *)
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
  and file_t = L.pointer_type (L.i8_type context)
  and the_module = L.create_module context "TEAM" in
  let list_struct_type = L.named_struct_type context "list_item" in
  let list_struct_ptr = L.pointer_type list_struct_type in
  (* list is a linked list with a ptr to the data and the next ptr *)
  let _ =
    L.struct_set_body list_struct_type
      [|L.pointer_type i8_t; list_struct_ptr|]
      true
  in
  (* Convert TEAM types to LLVM types *)
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.String -> string_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.Char -> char_t
    | A.Unknown -> i32_t
    | A.Func (args_t, ret_t) -> func_ty args_t ret_t
    | A.List _ -> L.pointer_type list_struct_ptr
    | A.File -> file_t
  (* get the ptr to the function *)
  and func_ty args_t ret_t =
    let llret_type = ltype_of_typ ret_t in
    let llargs = Array.map (fun t -> ltype_of_typ t) (Array.of_list args_t) in
    L.pointer_type (L.function_type llret_type llargs)
  in
  (* print function *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [|L.pointer_type i8_t|]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  (* power function *)
  let pow_t : L.lltype = L.function_type float_t [|float_t; float_t|] in
  let pow_func : L.llvalue = L.declare_function "pow" pow_t the_module in
  let open_t : L.lltype = L.function_type string_t [|string_t; string_t|] in
  let open_func : L.llvalue = L.declare_function "fopen" open_t the_module in
  let close_t : L.lltype = L.function_type i32_t [|file_t|] in
  let close_func : L.llvalue = L.declare_function "close" close_t the_module in
  let readline_t : L.lltype = L.function_type string_t [|file_t|] in
  let readline_func : L.llvalue =
    L.declare_function "readline" readline_t the_module
  in
  let write_t : L.lltype = L.function_type string_t [|file_t; string_t|] in
  let write_func : L.llvalue = L.declare_function "write" write_t the_module in
  let match_t : L.lltype = L.function_type i1_t [|string_t; string_t|] in
  let match_func : L.llvalue = L.declare_function "match" match_t the_module in
  let find_t : L.lltype = L.function_type string_t [|string_t; string_t|] in
  let find_func : L.llvalue = L.declare_function "find" find_t the_module in
  let replace_t : L.lltype =
    L.function_type string_t [|string_t; string_t; string_t; i32_t|]
  in
  let replace_func : L.llvalue =
    L.declare_function "replace" replace_t the_module
  in
  let replaceall_t : L.lltype =
    L.function_type string_t [|string_t; string_t; string_t|]
  in
  let replaceall_func : L.llvalue =
    L.declare_function "replace_all" replaceall_t the_module
  in
  let findall_t : L.lltype =
    L.function_type (L.pointer_type list_struct_ptr) [|string_t; string_t|]
  in
  let findall_func : L.llvalue =
    L.declare_function "find_all" findall_t the_module
  in
  let var_table = {lvariables= StringMap.empty; parent= None} in
  let globals = ref var_table in
  let function_decls : L.llvalue StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module) m
    in
    List.fold_left function_decl StringMap.empty functions
  in
  (* Generate code for a function *)
  let build_function_body scope fdecl =
    let the_function = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let rec find_variable sc n =
      try Some (StringMap.find n !sc.lvariables)
      with Not_found -> (
        match !sc.parent with None -> None | Some t -> find_variable t n )
    in
    (* Allocating space for function formal variables *)
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
      | A.List t -> t
      | _ -> raise (Failure "Internal error: Not a list type")
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    (* Function to generate code for a semantically checked expression *)
    let rec expr sc builder ((t, e) : sexpr) =
      match e with
      | SIntLit i -> L.const_int i32_t i
      | SFloatLit f -> L.const_float float_t f
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit c -> L.const_int char_t (Char.code c)
      | SStringLit s -> L.build_global_stringptr s "string" builder
      | SId n -> (
          let var = find_variable sc n in
          match var with
          | Some vv -> L.build_load vv n builder
          | None -> (
            (* Check if the variable is a function *)
            match t with
            | A.Func _ -> StringMap.find n function_decls
            | _ -> raise (E.NotFound n) ) )
      | SListLit l ->
          (* Build a Linked list *)
          let lst = L.build_malloc list_struct_ptr "list" builder in
          let _ = L.build_store (build_list t l sc builder) lst builder in
          lst
      | SSliceExpr (lexpr, slice) -> (
          let lt, _ = lexpr in
          let l = expr sc builder lexpr in
          match lt with
          | A.String -> (
            match slice with
            | SIndex i ->
                (* Getting a character indexed at i from a string *)
                let ptr =
                  L.build_gep l [|expr sc builder i|] "get_char_ptr" builder
                in
                L.build_load ptr "get_char" builder
            | SSlice (i, j) ->
                (* Getting a slice of a string *)
                let ptr =
                  L.build_gep l [|expr sc builder i|] "get_char_ptr" builder
                in
                let length =
                  L.build_sub (expr sc builder j) (expr sc builder i) "subb"
                    builder
                in
                let length_w_nul =
                  L.build_add length
                    ((L.const_int i32_t) 1)
                    "length_w_nul" builder
                in
                let new_str =
                  L.build_array_malloc i8_t length_w_nul "new_string" builder
                in
                let nul =
                  L.build_gep new_str [|length|] "string_term" builder
                in
                (* Adding null character to end of string *)
                let _ = L.build_store ((L.const_int i8_t) 0) nul builder in
                let mmcpy_t =
                  L.function_type void_t
                    [|L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t|]
                in
                let mmcpy =
                  L.declare_function "llvm.memcpy.p0i8.p0i8.i32" mmcpy_t
                    the_module
                in
                (* Copying the slice to newly allocated space *)
                let _ =
                  L.build_call mmcpy
                    [|new_str; ptr; length; (L.const_int i1_t) 1|]
                    "" builder
                in
                new_str )
          | A.List _ -> (
            match slice with
            | SIndex i ->
                (* Getting an element at index i *)
                let la_func = build_access_function () in
                let l = L.build_load l "ilist" builder in
                let item_ptr =
                  L.build_call la_func
                    [|l; expr sc builder i|]
                    "_result" builder
                in
                let data_ptr_ptr =
                  L.build_struct_gep item_ptr 0 "data_ptr_ptr" builder
                in
                let dat_ptr = L.build_load data_ptr_ptr "data_ptr" builder in
                (* Casting "void" pointer back to data pointer *)
                let type_casted =
                  L.build_bitcast dat_ptr
                    (L.pointer_type (ltype_of_typ t))
                    "cast_data_ptr" builder
                in
                L.build_load type_casted "data" builder
            | SSlice (i, j) ->
                (* Getting a list slice *)
                let la_func = build_access_function () in
                let l = L.build_load l "ilist" builder in
                let i = expr sc builder i in
                let item_ptr = L.build_call la_func [|l; i|] "start" builder in
                (* Checking for [i:] type of slice*)
                let j =
                  match j with
                  | _, SEnd -> L.const_int i32_t (-1)
                  | _ -> L.build_sub (expr sc builder j) i "difference" builder
                in
                let lc_func = build_copy_function t in
                let new_list_ptr_ptr =
                  L.build_malloc list_struct_ptr "new_list_ptr_ptr" builder
                in
                (* Copy the list slice into a new list *)
                let _ =
                  L.build_call lc_func
                    [|item_ptr; j; new_list_ptr_ptr|]
                    "" builder
                in
                new_list_ptr_ptr )
          | _ -> raise (Failure "Internal error: invalid slice") )
      | SBinop (e1, op, e2) ->
          let t1, _ = e1
          and t2, _ = e2
          and e1' = expr sc builder e1
          and e2' = expr sc builder e2 in
          (* Check for operand types *)
          if t1 = A.Float && t2 = A.Int then
            let cast_e2' = L.build_sitofp e2' float_t "cast" builder in
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
            let cast_e1' = L.build_sitofp e1' float_t "cast" builder in
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
                (* Casting integer to float *)
                let cast_e1' = L.build_sitofp e1' float_t "cast" builder
                and cast_e2' = L.build_sitofp e2' float_t "cast" builder in
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
            | A.Range ->
                (* Generating list for range operator *)
                let range_function = build_range_function () in
                let head_ptr_ptr =
                  L.build_malloc list_struct_ptr "head_ptr_ptr" builder
                in
                let _ =
                  L.build_store
                    (L.const_null list_struct_ptr)
                    head_ptr_ptr builder
                in
                L.build_call range_function
                  [|e1'; e2'; head_ptr_ptr; L.const_int i32_t 0|]
                  "range_list" builder
            | _ -> raise E.InvalidIntBinop
          else if t1 = A.Bool && t2 = A.Bool then
            match op with
            | A.And -> L.build_and e1' e2' "tmp" builder
            | A.Or -> L.build_or e1' e2' "tmp" builder
            | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else if t1 = A.Char && t2 = A.Char then
            match op with
            | A.Equal -> L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder
            | A.Neq -> L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder
            | _ -> raise E.InvalidFloatBinop
          else if t1 = A.String && t2 = A.String then
            match op with
            | A.Add ->
                (* Adding two strings together *)
                let sl_func = build_string_length_function () in
                let length1 =
                  L.build_call sl_func
                    [|e1'; L.const_int i32_t 0|]
                    "length" builder
                in
                let length2 =
                  L.build_call sl_func
                    [|e2'; L.const_int i32_t 0|]
                    "length" builder
                in
                let new_length =
                  L.build_add length1 length2 "new_length" builder
                in
                (* Adding the sum of lengths of strings with 1 to add null *)
                let new_length_w_null =
                  L.build_add new_length (L.const_int i32_t 1) "new_length_nul"
                    builder
                in
                let new_str =
                  L.build_array_malloc i8_t new_length_w_null "new_string"
                    builder
                in
                (* Copying both strings to newly allocated space *)
                let mmcpy_t =
                  L.function_type void_t
                    [|L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t|]
                in
                let mmcpy =
                  L.declare_function "llvm.memcpy.p0i8.p0i8.i32" mmcpy_t
                    the_module
                in
                let _ =
                  L.build_call mmcpy
                    [|new_str; e1'; length1; (L.const_int i1_t) 1|]
                    "" builder
                in
                let new_spot =
                  L.build_gep new_str [|length1|] "new_spot" builder
                in
                let _ =
                  L.build_call mmcpy
                    [|new_spot; e2'; length2; (L.const_int i1_t) 1|]
                    "" builder
                in
                let nul =
                  L.build_gep new_str [|new_length|] "string_term" builder
                in
                let _ = L.build_store ((L.const_int i8_t) 0) nul builder in
                new_str
            | A.Equal -> 
                (* Checking if strings are equal *)
                let strcmp_func = build_strcmp_function () in
                L.build_call strcmp_func [|e1'; e2'|] "strcmp_eq" builder
            | A.Neq -> 
                let strcmp_func = build_strcmp_function () in
                let bool_val = L.build_call strcmp_func [|e1'; e2'|] "strcmp_eq" builder in
                L.build_select bool_val (L.const_int i1_t 0) (L.const_int i1_t 1) "strcmp_neq" builder
            | _ -> raise E.InvalidStringBinop
          else if t1 = A.String && t2 = A.Char then
            match op with
            | A.Add ->
              (* Adding character to a string *)
              let sl_func = build_string_length_function () in
              let length1 =
                L.build_call sl_func
                  [|e1'; L.const_int i32_t 0|]
                  "length" builder
              in
              let length2 = 
                (L.const_int i32_t 1)
              in
              let new_length =
                L.build_add length1 length2 "new_length" builder
              in
              let new_length_w_null =
                L.build_add new_length (L.const_int i32_t 1) "new_length_nul"
                  builder
              in
              let new_str =
                L.build_array_malloc i8_t new_length_w_null "new_string"
                  builder
              in
              let mmcpy_t =
                L.function_type void_t
                  [|L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t|]
              in
              let mmcpy =
                L.declare_function "llvm.memcpy.p0i8.p0i8.i32" mmcpy_t
                  the_module
              in
              let _ =
                L.build_call mmcpy
                  [|new_str; e1'; length1; (L.const_int i1_t) 1|]
                  "" builder
              in
              let new_spot =
                L.build_gep new_str [|length1|] "new_spot" builder
              in
              let _ = L.build_store e2' new_spot builder in
              let nul =
                L.build_gep new_str [|new_length|] "string_term" builder
              in
              let _ = L.build_store ((L.const_int i8_t) 0) nul builder in
              new_str
            | _ -> raise E.InvalidStringBinop
          else if t1 = A.Char && t2 = A.String then
            (* Adding String to a character *)
            match op with
            | A.Add ->
              let sl_func = build_string_length_function () in
              let length1 = (L.const_int i32_t 1)
              in
              let length2 = 
                L.build_call sl_func
                  [|e2'; L.const_int i32_t 0|]
                  "length" builder
              in
              let new_length =
                L.build_add length1 length2 "new_length" builder
              in
              let new_length_w_null =
                L.build_add new_length (L.const_int i32_t 1) "new_length_nul"
                  builder
              in
              let new_str =
                L.build_array_malloc i8_t new_length_w_null "new_string"
                  builder
              in
              let mmcpy_t =
                L.function_type void_t
                  [|L.pointer_type i8_t; L.pointer_type i8_t; i32_t; i1_t|]
              in
              let mmcpy =
                L.declare_function "llvm.memcpy.p0i8.p0i8.i32" mmcpy_t
                  the_module
              in
              let _ = L.build_store e1' new_str builder in
              let new_spot =
                L.build_gep new_str [|length1|] "new_spot" builder
              in
              let _ =
                L.build_call mmcpy
                  [|new_spot; e2'; length2; (L.const_int i1_t) 1|]
                  "" builder
              in
              let nul =
                L.build_gep new_str [|new_length|] "string_term" builder
              in
              let _ = L.build_store ((L.const_int i8_t) 0) nul builder in
              new_str
            | _ -> raise E.InvalidStringBinop
          else if ((match t1 with A.List _ -> true | _ -> false)) then
            (* Concatenating two lists *)
            (* Allocating new list *)
            let new_list = L.build_malloc list_struct_ptr "new_list" builder in
            let _ = L.build_store (L.const_null list_struct_ptr) new_list builder in
            let slice = SSlice((A.Int, SIntLit 0), (A.Int, SIntLit 0)) in
            let ilst = get_list_inner_typ t1 in
            (* Copying the first list *)
            let _ = build_asn_list sc builder ilst new_list slice (expr sc builder e2) in
            (* Copying the second list *)
            let _ = build_asn_list sc builder ilst new_list slice (expr sc builder e1) in
            new_list
          else (
            print_endline (A.string_of_typ t1) ;
            print_endline (A.string_of_typ t2) ;
            raise (Failure "Not Yet Implemented") )
      | SUnop (op, e) ->
          (* Building unary operation code *)
          let t, _ = e in
          let e' = expr sc builder e in
          ( match op with
          | A.Neg when t = A.Float -> L.build_fneg
          | A.Neg -> L.build_neg
          | A.Not -> L.build_not )
            e' "tmp" builder
      | SAssign (le, re) ->
          let re' = expr sc builder re in
          let _ =
            match le with
            (* Updating regular variable assign *)
            | _, SId s -> update_variable sc s re' builder
            | _, SSliceExpr ((lst, ls), slc) -> (
              match lst with
              | A.List ilst ->
                  (* Assigning list/element to a list slice/index *)
                  let lis = expr sc builder (lst, ls) in
                  build_asn_list sc builder ilst lis slc re'
              | A.String -> raise (Failure "Cannot assign to string slice")
              | _ -> raise (Failure "Internal Error") )
            | _ -> raise (Failure "Internal Error")
          in
          re'
      | SCall ((_, SId "length"), [(A.List lt, lst)]) ->
          (* Building length function for list *)
          let ll_func = build_list_length_function () in
          let lst = expr sc builder (A.List lt, lst) in
          let lst = L.build_load lst "ilist" builder in
          L.build_call ll_func [|lst; L.const_int i32_t 0|] "length" builder
      | SCall ((_, SId "length"), [(A.String, st)]) ->
          (* Building length function for string *)
          let sl_func = build_string_length_function () in
          L.build_call sl_func
            [|expr sc builder (A.String, st); L.const_int i32_t 0|]
            "length" builder
      (* Declaring builtin functions which are written in C *)
      | SCall ((_, SId "open"), [(A.String, st); (A.String, st2)]) ->
          L.build_call open_func
            [|expr sc builder (A.String, st); expr sc builder (A.String, st2)|]
            "open" builder
      | SCall ((_, SId "close"), [(A.File, file)]) ->
          L.build_call close_func
            [|expr sc builder (A.File, file)|]
            "close" builder
      | SCall ((_, SId "readline"), [(A.File, file)]) ->
          L.build_call readline_func
            [|expr sc builder (A.File, file)|]
            "readline" builder
      | SCall ((_, SId "write"), [(A.File, file); (A.String, st)]) ->
          L.build_call write_func
            [|expr sc builder (A.File, file); expr sc builder (A.String, st)|]
            "write" builder
      (* Defining reverse function for lists *)
      | SCall ((_, SId "reverse"), [(A.List lt, lst)]) ->
          let reverse_func = build_list_reverse_function () in
          let list_ptr_ptr = expr sc builder (A.List lt, lst) in
          let list_ptr = L.build_load list_ptr_ptr "list_ptr" builder in
          let new_list_ptr_ptr =
            L.build_malloc list_struct_ptr "new_list_ptr_ptr" builder
          in
          let _ =
            L.build_store
              (L.const_null list_struct_ptr)
              new_list_ptr_ptr builder
          in
          let lc_func = build_copy_function (A.List lt) in
          let _ =
            L.build_call lc_func
              [|list_ptr; L.const_int i32_t (-1); new_list_ptr_ptr|]
              "last_node_ptr_ptr" builder
          in
          L.build_call reverse_func [|new_list_ptr_ptr|] "reversed_list" builder
      | SCall ((_, SId "print"), args) ->
          let eval_arg e =
            let t, _ = e in
            match t with
            (* Adding the special case for boolean *)
            | A.Bool ->
                let bool_val = expr sc builder e in
                let true_str =
                  L.build_global_stringptr "true" "string" builder
                in
                let false_str =
                  L.build_global_stringptr "false" "string" builder
                in
                let to_print =
                  L.build_select bool_val true_str false_str "bool_to_str"
                    builder
                in
                to_print
            | _ -> expr sc builder e
          in
          let arg_list = List.map eval_arg args in
          L.build_call printf_func (Array.of_list arg_list) "printf" builder
      (* Declaring regex functions which are defined in C *)
      | SCall ((_, SId "match"), [(A.String, st); (A.String, st2)]) ->
          L.build_call match_func
            [|expr sc builder (A.String, st); expr sc builder (A.String, st2)|]
            "match" builder
      | SCall ((_, SId "find"), [(A.String, st); (A.String, st2)]) ->
          L.build_call find_func
            [|expr sc builder (A.String, st); expr sc builder (A.String, st2)|]
            "find" builder
      | SCall
          ( (_, SId "replace")
          , [(A.String, st); (A.String, st2); (A.String, st3); (A.Int, i)] ) ->
          L.build_call replace_func
            [| expr sc builder (A.String, st)
             ; expr sc builder (A.String, st2)
             ; expr sc builder (A.String, st3)
             ; expr sc builder (A.Int, i) |]
            "replace" builder
      | SCall
          ( (_, SId "replaceall")
          , [(A.String, st); (A.String, st2); (A.String, st3)] ) ->
          L.build_call replaceall_func
            [| expr sc builder (A.String, st)
             ; expr sc builder (A.String, st2)
             ; expr sc builder (A.String, st3) |]
            "replaceall" builder
      | SCall ((_, SId "findall"), [(A.String, st); (A.String, st2)]) ->
          L.build_call findall_func
            [|expr sc builder (A.String, st); expr sc builder (A.String, st2)|]
            "findall" builder
      (* Appending to lists *)
      | SCall ((_, SId "append"), [(lt, lst); e]) ->
          let list_ptr_ptr = expr sc builder (lt, lst) in
          let list_ptr = L.build_load list_ptr_ptr "list_ptr" builder in
          let e' = expr sc builder e in
          let ll_func = build_list_length_function () in
          let length =
            L.build_call ll_func
              [|list_ptr; L.const_int i32_t 0|]
              "length" builder
          in
          (* Using insert function to insert at the end *)
          let insert_func = build_insert_function lt in
          L.build_call insert_func
            [|list_ptr_ptr; e'; length|]
            "list_ptr_ptr" builder
      (* Inserting to list at a certain index *)
      | SCall ((_, SId "insert"), [(lt, lst); e; i]) ->
          let list_ptr_ptr = expr sc builder (lt, lst) in
          let e' = expr sc builder e in
          let i' = expr sc builder i in
          let insert_func = build_insert_function lt in
          L.build_call insert_func [|list_ptr_ptr; e'; i'|] "list_ptr_ptr"
            builder
      (* Function call *)
      | SCall (f, args) ->
          (* Finding the function *)
          let fdef = expr sc builder f in
          (* Evaluating the function arguments *)
          let llarg = List.rev (List.map (expr sc builder) (List.rev args)) in
          let ret_type =
            match f with
            | A.Func (_, rett), _ -> rett
            | _ -> raise (Failure "Internal Error")
          in
          let result = match ret_type with A.Void -> "" | _ -> "_result" in
          L.build_call fdef (Array.of_list llarg) result builder
      | SEnd -> raise (Failure "Not Yet Implemented")
      | SNoexpr -> L.const_null i32_t
    (* Function to generate code for assigning to list *)
    and build_asn_list sc builder ilst lis slc re' =
      match slc with
      (* In case of assigning to index *)
      | SIndex i ->
          let la_func = build_access_function () in
          let lis = L.build_load lis "ilist" builder in
          (* Accessing the struct at the given index *)
          let item_ptr =
            L.build_call la_func [|lis; expr sc builder i|] "result" builder
          in
          (* Accessing the data field in the struct *)
          let data_ptr_ptr =
            L.build_struct_gep item_ptr 0 "data_ptpt" builder
          in
          (* Allocating new space for copying the data *)
          let copy_data_ptr =
            L.build_malloc (ltype_of_typ ilst) "copy_ptr" builder
          in
          let _ = L.build_store re' copy_data_ptr builder in
          (* Casting the data pointer to "void" pointer *)
          let type_casted_copy =
            L.build_bitcast copy_data_ptr (L.pointer_type i8_t) "ccopy" builder
          in
          (* Storing the pointer to data *)
          let _ = L.build_store type_casted_copy data_ptr_ptr builder in
          ()
      (* In case of assigning a list to a slice *)
      | SSlice (i, j) ->
          let lsti = L.build_load lis "ilist" builder in
          let rei = L.build_load re' "rei" builder in
          let la_func = build_access_function () in
          let lc_func = build_copy_function (A.List ilst) in
          (* Getting the struct at index j *)
          let end_ptr =
            match j with
            | _, SEnd -> L.const_null list_struct_ptr
            | _ ->
                L.build_call la_func
                  [|lsti; expr sc builder j|]
                  "result" builder
          in
          (* Allocating a dummy struct to change the length of list *)
          let temp = L.build_alloca list_struct_type "temp" builder in
          let next = L.build_struct_gep temp 1 "next" builder in
          let _ = L.build_store lsti next builder in
          (* Getting the struct at index i-1 *)
          let item_ptr =
            L.build_call la_func [|temp; expr sc builder i|] "result" builder
          in
          let item_next = L.build_struct_gep item_ptr 1 "item_next" builder in
          (* Copying the right hand list to the next pointer at struct i-1 *)
          let copy_end =
            L.build_call lc_func
              [|rei; L.const_int i32_t (-1); item_next|]
              "copied" builder
          in
          (* Copying the list after index j to the new list *)
          let _ = L.build_store end_ptr copy_end builder in
          let _ =
            L.build_store (L.build_load next "next" builder) lis builder
          in
          ()
    (* Function to add a variable to scope *)
    and add_variable_to_scope sc n v =
      sc := {lvariables= StringMap.add n v !sc.lvariables; parent= !sc.parent}
    and update_variable sc (n : string) (e' : L.llvalue) builder =
      let l_var =
        match find_variable sc n with
        | None -> raise (E.NotFound n)
        | Some t -> t
      in
      let _ = L.build_store e' l_var builder in
      sc :=
        {lvariables= StringMap.add n l_var !sc.lvariables; parent= !sc.parent}

    (* Function to compare strings *)
    and build_strcmp_function () = 
      match L.lookup_function "strcmp_function" the_module with
      | Some func -> func
      | None -> 
          (* returns 1 if the two strings are the same, 0 if otherwise *)
          let strcmp_func_t = 
            L.function_type i1_t [|string_t; string_t|]
          in
          let strcmp_func = L.define_function "strcmp_function" strcmp_func_t the_module in
          let strcmp_builder = 
            L.builder_at_end context (L.entry_block strcmp_func)
          in
          (* get the arguments *)
          let stringA = L.param strcmp_func 0 in
          let stringB = L.param strcmp_func 1 in

          let sl_func = build_string_length_function () in
          let length1 = L.build_call sl_func [|stringA; L.const_int i32_t 0|] "length" strcmp_builder in
          let length2 = L.build_call sl_func [|stringB; L.const_int i32_t 0|] "length" strcmp_builder in

          (* if lengths of the two strings are different, return false *)
          let bool_val = L.build_icmp L.Icmp.Ne length1 length2 "same_length" strcmp_builder in 
          let then_bb = L.append_block context "then" strcmp_func in 
          let _ = L.build_ret (L.const_int i1_t 0) (L.builder_at_end context then_bb) in
          let else_bb = L.append_block context "else" strcmp_func in 
          let else_builder = L.builder_at_end context else_bb in 
          let strcmp_helper_func = build_strcmp_helper_function () in 
          let last_index =
            L.build_sub length1 (L.const_int i32_t 1) "last_index" else_builder
          in
          (* call strcmp_helper_func and return its result *)
          let ret = 
            L.build_call strcmp_helper_func [|stringA; stringB; last_index; (L.const_int i32_t 0)|] 
            "res" else_builder
          in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb strcmp_builder in 
          strcmp_func
      
    (* helper function used by build_strcmp_function *)
    and build_strcmp_helper_function () = 
      match L.lookup_function "strcmp_helper_function" the_module with 
      | Some func -> func
      | None ->
          let strcmp_helper_func_t = 
            L.function_type i1_t [|string_t; string_t; i32_t; i32_t|]
          in
          let strcmp_helper_func = L.define_function "strcmp_helper_function" strcmp_helper_func_t the_module in
          let strcmp_helper_builder = 
            L.builder_at_end context (L.entry_block strcmp_helper_func)
          in

          (* get the arguments *)
          let stringA = L.param strcmp_helper_func 0 in
          let stringB = L.param strcmp_helper_func 1 in
          let last_index = L.param strcmp_helper_func 2 in
          let index = L.param strcmp_helper_func 3 in

          (* load the character at index *)
          let charA_ptr = L.build_gep stringA [|index|] "charA_ptr" strcmp_helper_builder in
          let charA = L.build_load charA_ptr "charA" strcmp_helper_builder in
          let charB_ptr = L.build_gep stringB [|index|] "charB_ptr" strcmp_helper_builder in
          let charB = L.build_load charB_ptr "charB" strcmp_helper_builder in

          (* if the character at index is not the same, return false *)
          let bool_not_same_val = L.build_icmp L.Icmp.Ne charA charB "not_same" strcmp_helper_builder in
          let then_not_same_bb = L.append_block context "then_not_same" strcmp_helper_func in
          let _ = L.build_ret (L.const_int i1_t 0) (L.builder_at_end context then_not_same_bb) in
          let else_same_bb = L.append_block context "else_same" strcmp_helper_func in
          let else_same_builder = L.builder_at_end context else_same_bb in

          (* if already at the last character, return true *)
          let bool_at_end_val = L.build_icmp L.Icmp.Eq index last_index "last_char" else_same_builder in 
          let then_end_bb = L.append_block context "then_end" strcmp_helper_func in
          let _ = L.build_ret (L.const_int i1_t 1) (L.builder_at_end context then_end_bb) in 
          let else_not_end_bb = L.append_block context "else_not_end" strcmp_helper_func in
          let else_not_end_builder = L.builder_at_end context else_not_end_bb in 
          let next_index = 
            L.build_add (L.const_int i32_t 1) index "next_index" else_not_end_builder
          in

          (* recursively call strcmp_helper_func to compare the rest of the string *)
          let ret = 
            L.build_call strcmp_helper_func [|stringA; stringB; last_index; next_index|]
            "res" else_not_end_builder
          in 
          let _ = L.build_ret ret else_not_end_builder in
          let _ = L.build_cond_br bool_at_end_val then_end_bb else_not_end_bb else_same_builder in
          let _ = L.build_cond_br bool_not_same_val then_not_same_bb else_same_bb strcmp_helper_builder in
          strcmp_helper_func

    (* Function to generate list from a range *)
    and build_range_function () =
      match L.lookup_function "range_function" the_module with
      | Some func -> func
      | None ->
          let range_func_t =
            L.function_type
              (L.pointer_type list_struct_ptr)
              [|i32_t; i32_t; L.pointer_type list_struct_ptr; i32_t|]
          in
          let range_func = L.define_function "range_function" range_func_t the_module in
          let range_builder =
            L.builder_at_end context (L.entry_block range_func)
          in

          (* get the arguments *)
          let s = L.param range_func 0 in
          let e = L.param range_func 1 in
          let head_ptr_ptr = L.param range_func 2 in
          let curr_length = L.param range_func 3 in

          (* return the list generated if the last element is generated *)
          let bool_val = L.build_icmp L.Icmp.Eq s e "is_last" range_builder in
          let then_bb = L.append_block context "then" range_func in
          let _ = L.build_ret head_ptr_ptr (L.builder_at_end context then_bb) in
          let else_bb = L.append_block context "else" range_func in
          let else_builder = L.builder_at_end context else_bb in

          (* call insert to append the next element *)
          let insert_func = build_insert_function (A.List A.Int) in
          let head_ptr_ptr =
            L.build_call insert_func
              [|head_ptr_ptr; s; curr_length|]
              "head_ptr_ptr" else_builder
          in
          let next_s =
            L.build_add (L.const_int i32_t 1) s "next_s" else_builder
          in
          let next_length =
            L.build_add (L.const_int i32_t 1) curr_length "next_length"
              else_builder
          in

          (* recursively call range_func to generate the rest of the elements *)
          let ret =
            L.build_call range_func
              [|next_s; e; head_ptr_ptr; next_length|]
              "" else_builder
          in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb range_builder in
          range_func

    (* Function to deep copy a list *)
    and build_copy_function typ =
      let t = get_list_inner_typ typ in
      let func_name = "list_copy_" ^ A.string_of_typ t in
      (* Checking if the function is already defined *)
      match L.lookup_function func_name the_module with
      | Some func -> func
      | None ->
          (* Function defined takes list, length to be copied and space
             where we want the list to be copied *)
          let lc_func_t =
            L.function_type
              (L.pointer_type list_struct_ptr)
              [|list_struct_ptr; i32_t; L.pointer_type list_struct_ptr|]
          in
          let lc_func = L.define_function func_name lc_func_t the_module in
          let lc_builder = L.builder_at_end context (L.entry_block lc_func) in
          (* Checking if the index is 0 which means list is done copying*)
          let i_cond =
            L.build_icmp L.Icmp.Eq (L.param lc_func 1) (L.const_int i32_t 0)
              "is_zero" lc_builder
          in
          (* Checking if the list being copied is a null pointer *)
          let n_cond =
            L.build_is_null (L.param lc_func 0) "ptr_is_null" lc_builder
          in
          let bool_val = L.build_or i_cond n_cond "or_conds" lc_builder in
          let then_bb = L.append_block context "then" lc_func in
          (* Base case of recursion: Return the pointer to the next pointer
             of the struct at the end *)
          let _ =
            L.build_ret (L.param lc_func 2) (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" lc_func in
          let else_builder = L.builder_at_end context else_bb in
          (* Allocating space for a new struct *)
          let new_struct_ptr =
            L.build_malloc list_struct_type "new_struct_ptr" else_builder
          in
          let _ =
            L.build_store
              (L.const_null list_struct_type)
              new_struct_ptr else_builder
          in
          (* Allocating space for the data *)
          let data_ptr = L.build_malloc (ltype_of_typ t) "ltyp" else_builder in
          (* Getting the old data to be copied *)
          let old_data_ptr_ptr =
            L.build_struct_gep (L.param lc_func 0) 0 "old_data_ptr_ptr"
              else_builder
          in
          let old_data_ptr =
            L.build_load old_data_ptr_ptr "old_data_ptr" else_builder
          in
          (* Casting the "void" pointer back to data pointer *)
          let old_data_ptr =
            L.build_bitcast old_data_ptr
              (L.pointer_type (ltype_of_typ t))
              "cast_old_data_ptr" else_builder
          in
          let old_data = L.build_load old_data_ptr "old_data" else_builder in
          (* Copy the old data into newly allocated space *)
          let _ = L.build_store old_data data_ptr else_builder in
          (* Casting the data pointer to "void" pointer *)
          let data_ptr_cast =
            L.build_bitcast data_ptr (L.pointer_type i8_t) "data_ptr_cast"
              else_builder
          in
          (* Storing pointer to copied data in the new struct *)
          let _ =
            L.build_store data_ptr_cast
              (L.build_struct_gep new_struct_ptr 0 "store_new_data" else_builder)
              else_builder
          in
          (* Storing the pointer to new struct in the previous struct's
             next struct pointer field *)
          let _ =
            L.build_store new_struct_ptr (L.param lc_func 2) else_builder
          in
          let ptr_ptr =
            L.build_struct_gep new_struct_ptr 1 "next" else_builder
          in
          (* Getting the pointer to next struct pointer field
             in current struct *)
          let next_ptr =
            L.build_struct_gep (L.param lc_func 0) 1 "next_ptr" else_builder
          in
          let next = L.build_load next_ptr "next" else_builder in
          (* Subtracting 1 from the index *)
          let sub =
            L.build_sub (L.param lc_func 1) (L.const_int i32_t 1) "sub"
              else_builder
          in
          (* Recursively calling the function for rest of the list *)
          let ret =
            L.build_call lc_func [|next; sub; ptr_ptr|] "" else_builder
          in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb lc_builder in
          lc_func

    and build_string_length_function () =
      match L.lookup_function "string_length" the_module with
      | Some func -> func
      | None ->
          (* Function takes in the string and current length *)
          let sl_func_t = L.function_type i32_t [|string_t; i32_t|] in
          let sl_func =
            L.define_function "string_length" sl_func_t the_module
          in
          let sl_builder = L.builder_at_end context (L.entry_block sl_func) in
          (* Checking if the string is just a null character *)
          let bool_val =
            L.build_is_null
              (L.build_load (L.param sl_func 0) "char" sl_builder)
              "ptr_is_null" sl_builder
          in
          let then_bb = L.append_block context "then" sl_func in
          (* Returning the current length after finding a null character *)
          let _ =
            L.build_ret (L.param sl_func 1) (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" sl_func in
          let else_builder = L.builder_at_end context else_bb in
          (* Getting pointer to next character in the string *)
          let next =
            L.build_gep (L.param sl_func 0)
              [|L.const_int i32_t 1|]
              "next_ptr" else_builder
          in
          (* Adding 1 to the current length *)
          let add =
            L.build_add (L.param sl_func 1) (L.const_int i32_t 1) "add"
              else_builder
          in
          (* Recursively calling length function with rest of the string*)
          let ret = L.build_call sl_func [|next; add|] "result" else_builder in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb sl_builder in
          sl_func

    (* list_reverse_helper is used by list_reverse *)
    and build_list_reverse_helper_function () =
      match L.lookup_function "list_reverse_helper" the_module with
      | Some func -> func
      | None ->
          let reverse_helper_func_t =
            L.function_type
              (L.pointer_type list_struct_ptr)
              [|L.pointer_type list_struct_ptr; L.pointer_type list_struct_ptr|]
          in
          let reverse_helper_func =
            L.define_function "list_reverse_helper" reverse_helper_func_t
              the_module
          in
          let reverse_helper_builder =
            L.builder_at_end context (L.entry_block reverse_helper_func)
          in

          (* get arguments *)
          let prev_node_ptr_ptr = L.param reverse_helper_func 0 in
          let curr_node_ptr_ptr = L.param reverse_helper_func 1 in

          (* get pointer to the previous node *)
          let prev_node_ptr =
            L.build_load prev_node_ptr_ptr "prev_node_ptr"
              reverse_helper_builder
          in

          (* get pointer to the current node *)
          let curr_node_ptr =
            L.build_load curr_node_ptr_ptr "curr_node_ptr"
              reverse_helper_builder
          in

          (* get next node *)
          let next_node_ptr_ptr =
            L.build_struct_gep curr_node_ptr 1 "next_node_ptr_ptr"
              reverse_helper_builder
          in
          let next_node_ptr =
            L.build_load next_node_ptr_ptr "next_node_ptr"
              reverse_helper_builder
          in

          (* reverse direction *)
          let temp_ptr_ptr =
            L.build_malloc list_struct_ptr "temp_ptr_ptr" reverse_helper_builder
          in
          let _ =
            L.build_store next_node_ptr temp_ptr_ptr reverse_helper_builder
          in

          (* return pointer to the new list if the last element is reached *)
          let bool_val =
            L.build_is_null next_node_ptr "ptr_is_null" reverse_helper_builder
          in
          let _ =
            L.build_store prev_node_ptr next_node_ptr_ptr reverse_helper_builder
          in
          let then_bb = L.append_block context "then" reverse_helper_func in
          let _ =
            L.build_ret curr_node_ptr_ptr (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" reverse_helper_func in
          let else_builder = L.builder_at_end context else_bb in

          (* recursively call reverse_helper_func to reverse the rest of the list *)
          let ret =
            L.build_call reverse_helper_func
              [|curr_node_ptr_ptr; temp_ptr_ptr|]
              "result" else_builder
          in
          let _ = L.build_ret ret else_builder in
          let _ =
            L.build_cond_br bool_val then_bb else_bb reverse_helper_builder
          in
          reverse_helper_func

    (* building reverse function *)
    and build_list_reverse_function () =
      match L.lookup_function "list_reverse" the_module with
      | Some func -> func
      | None ->
          let reverse_func_t =
            L.function_type
              (L.pointer_type list_struct_ptr)
              [|L.pointer_type list_struct_ptr|]
          in
          let reverse_func =
            L.define_function "list_reverse" reverse_func_t the_module
          in
          let reverse_builder =
            L.builder_at_end context (L.entry_block reverse_func)
          in

          (* get arguments *)
          let list_ptr_ptr = L.param reverse_func 0 in
          let list_ptr = L.build_load list_ptr_ptr "list_ptr" reverse_builder in

          (* return the list if the head is null *)
          let bool_val_is_head_null =
            L.build_is_null list_ptr "ptr_is_null" reverse_builder
          in
          let then_head_null_bb = L.append_block context "then" reverse_func in
          let _ =
            L.build_ret list_ptr_ptr
              (L.builder_at_end context then_head_null_bb)
          in
          let else_head_not_null_bb =
            L.append_block context "else" reverse_func
          in
          let else_head_not_null_builder =
            L.builder_at_end context else_head_not_null_bb
          in

          (* get pointer to the list starting from the next node *)
          let next_ptr_ptr =
            L.build_struct_gep list_ptr 1 "next_ptr_ptr"
              else_head_not_null_builder
          in
          let next_ptr =
            L.build_load next_ptr_ptr "next_ptr" else_head_not_null_builder
          in

          (* return the list if the list is a singleton *)
          let bool_val_is_next_null =
            L.build_is_null next_ptr "next_ptr_is_null"
              else_head_not_null_builder
          in
          let then_next_null_bb = L.append_block context "then_" reverse_func in
          let _ =
            L.build_ret list_ptr_ptr
              (L.builder_at_end context then_next_null_bb)
          in
          let else_next_not_null_bb =
            L.append_block context "else_" reverse_func
          in
          let else_next_not_null_builder =
            L.builder_at_end context else_next_not_null_bb
          in

          (* call reverse_helper_function to reverse the list *)
          let list_reverse_helper_function =
            build_list_reverse_helper_function ()
          in
          let head_ptr_ptr =
            L.build_call list_reverse_helper_function
              [|list_ptr_ptr; next_ptr_ptr|]
              "result" else_next_not_null_builder
          in

          (* the next pointer of the head now points to nothing *)
          let _ =
            L.build_store
              (L.const_null list_struct_ptr)
              next_ptr_ptr else_next_not_null_builder
          in
          let _ = L.build_ret head_ptr_ptr else_next_not_null_builder in
          let _ =
            L.build_cond_br bool_val_is_head_null then_head_null_bb
              else_head_not_null_bb reverse_builder
          in
          let _ =
            L.build_cond_br bool_val_is_next_null then_next_null_bb
              else_next_not_null_bb else_head_not_null_builder
          in
          reverse_func

    (* Building insert function *)
    and build_insert_function typ =
      let t = get_list_inner_typ typ in
      (* insert function has to be defined for different types *)
      let func_name = "insert_" ^ A.string_of_typ t in
      match L.lookup_function func_name the_module with
      | Some func -> func
      | None ->
          let ltype = ltype_of_typ t in
          let insert_func_t =
            L.function_type
              (L.pointer_type list_struct_ptr)
              [|L.pointer_type list_struct_ptr; ltype; i32_t|]
          in
          let insert_func =
            L.define_function func_name insert_func_t the_module
          in
          let insert_builder =
            L.builder_at_end context (L.entry_block insert_func)
          in

          (* get arguments *)
          let list_ptr_ptr = L.param insert_func 0 in
          let e' = L.param insert_func 1 in
          let i' = L.param insert_func 2 in
          let list_ptr = L.build_load list_ptr_ptr "list_ptr" insert_builder in

          (* malloc space for a new list *)
          let new_list_ptr_ptr =
            L.build_malloc list_struct_ptr "new_list_ptr_ptr" insert_builder
          in
          let _ =
            L.build_store
              (L.const_null list_struct_ptr)
              new_list_ptr_ptr insert_builder
          in

          (* get the length of the function *)
          let lc_func = build_copy_function typ in

          (* copy the list to a new one so that the old list is not mutated *)
          let _ =
            L.build_call lc_func
              [|list_ptr; L.const_int i32_t (-1); new_list_ptr_ptr|]
              "last_node_ptr_ptr" insert_builder
          in
          let new_list_ptr =
            L.build_load new_list_ptr_ptr "new_list_ptr" insert_builder
          in
          let la_func = build_access_function () in
          let temp = L.build_alloca list_struct_type "temp" insert_builder in
          let next = L.build_struct_gep temp 1 "next" insert_builder in
          let _ = L.build_store new_list_ptr next insert_builder in

          (* malloc space for the actual data *)
          let dat_struct =
            L.build_malloc list_struct_type "data_node" insert_builder
          in

          (* malloc space for the pointer to the data *)
          let dat_ptr = L.build_malloc ltype "data" insert_builder in
          let _ = L.build_store e' dat_ptr insert_builder in

          (* store the data pointer to the node struct *)
          let dat_ptr_ptr =
            L.build_struct_gep dat_struct 0 "dat" insert_builder
          in
          let type_casted =
            L.build_bitcast dat_ptr (L.pointer_type i8_t) "cast" insert_builder
          in
          let _ = L.build_store type_casted dat_ptr_ptr insert_builder in

          (* get the node specified by the index *)
          let item_ptr =
            L.build_call la_func [|temp; i'|] "result" insert_builder
          in
          let cur_next = L.build_struct_gep item_ptr 1 "test" insert_builder in

          (* keep a record of what next currently pointer to *)
          let _ =
            L.build_store
              (L.build_load cur_next "temp" insert_builder)
              (L.build_struct_gep dat_struct 1 "dat" insert_builder)
              insert_builder
          in

          (* insertion happends here *)
          let _ = L.build_store dat_struct cur_next insert_builder in

          (* connect old list to the new one *)
          let _ =
            L.build_store
              (L.build_load next "temp" insert_builder)
              new_list_ptr_ptr insert_builder
          in
          let _ = L.build_ret new_list_ptr_ptr insert_builder in
          insert_func
    (* Building function to get length of list *)
    and build_list_length_function () =
      match L.lookup_function "list_length" the_module with
      | Some func -> func
      | None ->
          (* Functiont takes a list and current length *)
          let ll_func_t = L.function_type i32_t [|list_struct_ptr; i32_t|] in
          let ll_func = L.define_function "list_length" ll_func_t the_module in
          let ll_builder = L.builder_at_end context (L.entry_block ll_func) in
          (* Checking if a list is null *)
          let bool_val =
            L.build_is_null (L.param ll_func 0) "ptr_is_null" ll_builder
          in
          let then_bb = L.append_block context "then" ll_func in
          (* Return the current length if list is null *)
          let _ =
            L.build_ret (L.param ll_func 1) (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" ll_func in
          let else_builder = L.builder_at_end context else_bb in
          (* Get the next struct in the list *)
          let next_ptr =
            L.build_struct_gep (L.param ll_func 0) 1 "next_ptr" else_builder
          in
          let next = L.build_load next_ptr "next" else_builder in
          (* Add 1 to the current length *)
          let add =
            L.build_add (L.param ll_func 1) (L.const_int i32_t 1) "add"
              else_builder
          in
          (* Recursively call the function with the rest of the list *)
          let ret = L.build_call ll_func [|next; add|] "result" else_builder in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb ll_builder in
          ll_func
    (* Function to access a node at certain index *)
    and build_access_function () =
      match L.lookup_function "list_access" the_module with
      | Some func -> func
      | None ->
          (* Function takes in a list and index *)
          let la_func_t =
            L.function_type list_struct_ptr [|list_struct_ptr; i32_t|]
          in
          let la_func = L.define_function "list_access" la_func_t the_module in
          let la_builder = L.builder_at_end context (L.entry_block la_func) in
          (* Check if we are at the end of the list by comparing with null *)
          let bool_val =
            L.build_icmp L.Icmp.Eq (L.param la_func 1) (L.const_int i32_t 0)
              "is_zero" la_builder
          in
          let then_bb = L.append_block context "then" la_func in
          (* Return the current struct if index is 0 *)
          let _ =
            L.build_ret (L.param la_func 0) (L.builder_at_end context then_bb)
          in
          let else_bb = L.append_block context "else" la_func in
          let else_builder = L.builder_at_end context else_bb in
          (* Get the next node in the list *)
          let next_ptr =
            L.build_struct_gep (L.param la_func 0) 1 "next_ptr" else_builder
          in
          let next = L.build_load next_ptr "next" else_builder in
          (* Subtract 1 from the index *)
          let sub =
            L.build_sub (L.param la_func 1) (L.const_int i32_t 1) "sub"
              else_builder
          in
          (* Recursively call with the rest of the list *)
          let ret = L.build_call la_func [|next; sub|] "result" else_builder in
          let _ = L.build_ret ret else_builder in
          let _ = L.build_cond_br bool_val then_bb else_bb la_builder in
          la_func

    (* Function to build a list *)
    and build_list list_typ lis (scope : var_table ref) builder =
      let typ = get_list_inner_typ list_typ in
      let ltyp = ltype_of_typ typ in
      (* Function to build an individual struct *)
      let build_link prev data =
        (* Allocate a single struct *)
        let entry_ptr = L.build_malloc list_struct_type "list_item" builder in
        (* Initialize the allocated struct with null *)
        let _ =
          L.build_store (L.const_null list_struct_type) entry_ptr builder
        in
        (* Allocate space for the data *)
        let data_ptr = L.build_malloc ltyp "copied" builder in
        (* Store data in the allocated space *)
        let _ = L.build_store (expr scope builder data) data_ptr builder in
        (* Cast data pointer to "void" pointer *)
        let typcast_ptr =
          L.build_bitcast data_ptr (L.pointer_type i8_t) "cast_ptr" builder
        in
        (* Get the pointer to data pointer in the struct *)
        let data_ptr_container =
          L.build_struct_gep entry_ptr 0 "data_ptr_container" builder
        in
        (* Store the typecasted data pointer in the struct *)
        let _ = L.build_store typcast_ptr data_ptr_container builder in
        (* Get the pointer to next struct field *)
        let next = L.build_struct_gep entry_ptr 1 "next" builder in
        (* Store the previous struct ptr in current struct's next field *)
        let _ = L.build_store prev next builder in
        entry_ptr
      in
      let null_ptr = L.const_pointer_null list_struct_ptr in
      List.fold_left build_link null_ptr (List.rev lis)

    (* Function to build code for Statements *)
    and build_stmt sc builder stmt loop =
      match stmt with
      | SBlock sl ->
          (* Create a new scope and build instruction for all statements *)
          let new_scope = ref {lvariables= StringMap.empty; parent= Some sc} in
          List.fold_left (fun b s -> build_stmt new_scope b s loop) builder sl
      | SExpr e ->
          (* Build instruction for expression *)
          let _ = expr sc builder e in
          builder
      | SReturn e ->
          (* Generate a return statement *)
          let _ =
            match fdecl.styp with
            | A.Void -> L.build_ret_void builder
            | _ -> L.build_ret (expr sc builder e) builder
          in
          builder
      | SIf (predicate, then_stmt, else_stmt) ->
          (* Same implementation as microc *)
          let bool_val = expr sc builder predicate in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in
          let then_bb = L.append_block context "then" the_function in
          let then_builder =
            build_stmt sc (L.builder_at_end context then_bb) then_stmt loop
          in
          let () = add_terminal then_builder branch_instr in
          let else_bb = L.append_block context "else" the_function in
          let else_builder =
            build_stmt sc (L.builder_at_end context else_bb) else_stmt loop
          in
          let () = add_terminal else_builder branch_instr in
          let _ = L.build_cond_br bool_val then_bb else_bb builder in
          L.builder_at_end context merge_bb
      | SFor (s, (t, e), sl) ->
          (* An equivalent for loop code written using while loop and
             and other statemtns *)
          let equivalent =
            match t with
            | A.List s_ty ->
                (* Get length of the list *)
                let len_call =
                  ( A.Int
                  , SCall
                      ((A.Func ([A.List A.Int], A.Int), SId "length"), [(t, e)])
                  )
                in
                (* Create a new variable for index *)
                let index_expr = (A.Int, SId "for_index") in
                let while_cond =
                  (A.Bool, SBinop (index_expr, A.Less, len_call))
                in
                (* Create a new variable to store the current list entry *)
                let element = (s_ty, SId s) in
                SBlock
                  (* Declare the index variable *)
                  [ SDeclaration (A.Int, "for_index", (A.Int, SIntLit 0))
                  ; SDeclaration (s_ty, s, (A.Void, SNoexpr))
                  ; SWhile
                      ( while_cond
                      , SBlock
                          [ SExpr
                              ( s_ty
                              , SAssign
                                  ( element
                                  , ( s_ty
                                    , SSliceExpr ((t, e), SIndex index_expr) )
                                  ) )
                          ; SExpr
                              ( A.Int
                              , SAssign
                                  ( index_expr
                                  , ( A.Int
                                    , SBinop
                                        (index_expr, A.Add, (A.Int, SIntLit 1))
                                    ) ) )
                          ; sl ] ) ]
            | A.String ->
                let len_call =
                  ( A.Int
                  , SCall ((A.Func ([A.String], A.Int), SId "length"), [(t, e)])
                  )
                in
                let index_expr = (A.Int, SId "for_index") in
                let while_cond =
                  (A.Bool, SBinop (index_expr, A.Less, len_call))
                in
                let element = (A.Char, SId s) in
                SBlock
                  [ SDeclaration (A.Int, "for_index", (A.Int, SIntLit 0))
                  ; SDeclaration (A.Char, s, (A.Void, SNoexpr))
                  ; SWhile
                      ( while_cond
                      , SBlock
                          [ SExpr
                              ( A.Char
                              , SAssign
                                  ( element
                                  , ( A.Char
                                    , SSliceExpr ((t, e), SIndex index_expr) )
                                  ) )
                          ; SExpr
                              ( A.Int
                              , SAssign
                                  ( index_expr
                                  , ( A.Int
                                    , SBinop
                                        (index_expr, A.Add, (A.Int, SIntLit 1))
                                    ) ) )
                          ; sl ] ) ]
            | _ -> raise (Failure "internal error")
          in
          build_stmt sc builder equivalent loop
      | SDeclaration (t, n, e) ->
          let e =
            match e with
            | A.Void, SNoexpr -> (
              match t with
              | A.List _ ->
                  let ptr_ptr =
                    L.build_malloc list_struct_ptr "ptr_ptr" builder
                  in
                  let _ =
                    L.build_store (L.const_null list_struct_ptr) ptr_ptr builder
                  in
                  ptr_ptr
              | A.String -> L.build_global_stringptr "" "string" builder
              | _ -> L.const_null (ltype_of_typ t) )
            | asd -> expr sc builder asd
          in
          let _ =
            match fdecl.sfname with
            | "main" ->
                let global =
                  L.define_global n (L.const_null (ltype_of_typ t)) the_module
                in
                let _ = L.build_store e global builder in
                add_variable_to_scope sc n global
            | _ ->
                let init_pos = L.instr_begin (L.entry_block the_function) in
                let new_builder = L.builder_at context init_pos in
                let local = L.build_alloca (ltype_of_typ t) n new_builder in
                let _ = L.build_store e local builder in
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
      List.fold_left (fun b s -> build_stmt scope b s []) builder fdecl.sbody
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
