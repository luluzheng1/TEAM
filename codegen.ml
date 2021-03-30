(* Code generation: translate takes a semantically checked AST and produces
   LLVM IR *)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)
module E = Exceptions

(* Data structure to represent the current scope and the parent table. *)
type sym_table =
  { names: L.llvalue StringMap.t
  ; (* Names bound in the current block*)
    parent: sym_table ref option (* Enclosing space *) }

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (functions, statements) =
  let context = L.global_context () in
  let llmem = L.MemoryBuffer.of_file "builtin.c" in
  let llm = Llvm_bitreader.parse_bitcode context llmem in
  (* Add types to the context so we can use them in our LLVM code *)
  let int_t = L.i32_type context (* int type *)
  and float_t = L.double_type context (* float type *)
  and bool_t = L.i1_type context (* bool type *)
  and str_t = L.pointer_type (L.i8_type context) (* string type *)
  (* TODO: need to add char type *)
  and i8_t = L.i8_type context (* pointer type *)
  and void_t = L.void_type context (* void type *)
  and lst_t =
    L.pointer_type
      ( match L.type_by_name llm "struct.List" with
      | None -> raise (Failure "Missing implementation for struct List")
      | Some t -> t )
  (* TODO: need to add function type *)
  (* Create an LLVM module -- this is a "container" into which we'll generate
     actual code *)
  and the_module = L.create_module context "TEAM" in
  (* Convert TEAM types to LLVM types *)
  let rec ltype_of_sfunc name sfunc_decl =
    let formal_types =
      List.map (fun (t, _) -> ltype_of_typ t) sfunc_decl.formals
    in
    L.pointer_type
      (L.function_type
         (ltype_of_typ sfunc_decl.typ)
         (Array.of_list formal_types) )
  and ltype_of_typ = function
    | A.Int -> int_t
    | A.Float -> float_t
    | A.Bool -> bool_t
    | A.String -> str_t
    | A.(List _) -> lst_t
    | A.Void -> void_t
    | A.Char -> i8_t
    | t -> raise (Failure "Not Yet Implemented")
  in
  let build_program_body statements =
    (* Define the global symbol table *)
    let symbol_table = {names= StringMap.empty; parent= None} in
    let global_scope = ref symbol_table in
    ()
  in
  build_program_body statements ;
  the_module
