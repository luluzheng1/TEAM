(* team.ml: scan & parse & sematically analyze the input, pretty-print AST and
   SAST *)

type action = Ast | Sast | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist =
    [ ("-a", Arg.Unit (set_action Ast), "Print the AST")
    ; ("-s", Arg.Unit (set_action Sast), "Print the SAST")
    ; ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR") ]
  in
  let usage_msg = "usage: ./team.native [-a|-s|-l] [file.tm]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg ;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  let sast = Semant.check ast in
  match !action with
  | Ast -> print_string (Ast.string_of_program ast)
  | Sast -> print_string (Sast.string_of_sprogram sast)
  | LLVM_IR ->
      let m = Codegen.translate sast in
      Llvm_analysis.assert_valid_module m ;
      print_string (Llvm.string_of_llmodule m)
