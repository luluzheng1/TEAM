(* team.ml: scan & parse & sematically analyze the input, pretty-print AST and
   SAST *)

type action = Ast | Sast | Resolve | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist =
    [ ("-a", Arg.Unit (set_action Ast), "Print the AST")
    ; ("-s", Arg.Unit (set_action Sast), "Print the SAST")
    ; ("-r", Arg.Unit (set_action Resolve), "Print the resolved SAST")
    ; ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR") ]
  in
  let usage_msg = "usage: ./team.native [-a|-s|-r|-l] [file.tm]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg ;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  let resolved_sast = Resolve.resolve sast in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | Sast -> print_string (Sast.string_of_sprogram sast)
  | Resolve -> print_string (Sast.string_of_sprogram resolved_sast)
  | LLVM_IR ->
      let m = Codegen.translate resolved_sast in
      Llvm_analysis.assert_valid_module m ;
      print_string (Llvm.string_of_llmodule m)
