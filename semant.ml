(* Semantic checking for the TEAM compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions, statements) =
  let rec expr = function
      IntLit l -> (Int, SIntLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | BoolLit l -> (Bool, SBoolLit l)
    | CharLit l -> (Char, SCharLit l)
    | StringLit l -> (String, SStringLit l)
    | _ -> (Void, SNoexpr)
  in

  let check_bool_expr e = 
    let (t', e') = expr e
    and err = "expected Boolean expression in " ^ string_of_expr e
    in if t' != Bool then raise (Failure err) else (t', e') 
  in

  let rec check_stmt = function
      Expr e -> SExpr (expr e)
    | If(p, b1, b2, b3) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2, check_stmt b3)
    | For(e1, e2, st) ->
        SFor(expr e1, expr e2, check_stmt st)
    | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
    | Block sl -> 
        let rec check_stmt_list = function
            Block sl :: ss  -> check_stmt_list (sl @ ss)
          | s :: ss         -> check_stmt s :: check_stmt_list ss
          | []              -> []
        in SBlock(check_stmt_list sl)
    | _ -> SExpr((Void, SNoexpr))
  in

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void; fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", String);
			                         ("open", String);
			                         ("close", String);
			                         ("len", String)]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    let formals' = check_binds "formal" func.formals in

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (globals' @ formals' )
    in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in
  

in ([], List.map check_stmt statements)