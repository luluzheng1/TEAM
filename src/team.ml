(* team.ml: scan & parse & sematically analyze the input, pretty-print AST and
   SAST *)
(* Authors: Naoki O., Yingjie L., Lulu Z., Saurav G. *)
type action = Ast | Sast | Resolve | LLVM_IR

let make_err err = raise (Failure err)

let scan_error lexbuf ch =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  let line = string_of_int start_p.Lexing.pos_lnum in
  let ch_start =
    string_of_int (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
  in
  let ch_end = string_of_int (end_p.Lexing.pos_cnum - end_p.Lexing.pos_bol) in
  make_err
    ( "Illegal character at line " ^ line ^ ", characters " ^ ch_start ^ "-"
    ^ ch_end ^ ": " ^ "\"" ^ ch ^ "\"" )

let parse_error lexbuf =
  let start_p = Lexing.lexeme_start_p lexbuf in
  let end_p = Lexing.lexeme_end_p lexbuf in
  let line = string_of_int start_p.Lexing.pos_lnum in
  let ch_start =
    string_of_int (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
  in
  let ch_end = string_of_int (end_p.Lexing.pos_cnum - end_p.Lexing.pos_bol) in
  let lexeme = Lexing.lexeme lexbuf in
  make_err
    ( "Syntax error at line " ^ line ^ ", characters " ^ ch_start ^ "-" ^ ch_end
    ^ ": " ^ lexeme )

let parse lexbuf =
  try Parser.program Scanner.token lexbuf with
  | Scanner.Scan_error ch -> scan_error lexbuf ch
  | Parsing.Parse_error -> parse_error lexbuf

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let set_channel channel filename =
    let file =
      try open_in filename
      with Sys_error s ->
        let _ = print_endline s in
        exit 1
    in
    channel := file
  in
  let speclist =
    [ ("-a", Arg.Unit (set_action Ast), "Print the AST")
    ; ("-s", Arg.Unit (set_action Resolve), "Print the Resolved SAST")
      (* ; ("-s", Arg.Unit (set_action Sast), "Print the SAST") *)
      (* ; ("-r", Arg.Unit (set_action Resolve), "Print the Resolved SAST") *)
    ; ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR") ]
  in
  let usage_msg = "usage: ./team.native [-a|-s|-l] [file.tm]" in
  (* get buffers/channel for standard library *)
  let string_channel = ref stdin in
  let list_channel = ref stdin in
  let channel = ref stdin in
  set_channel string_channel "standard_library/string.tm" ;
  set_channel list_channel "standard_library/list.tm" ;
  (* parser program and standard library *)
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg ;
  let lexbuf = Lexing.from_channel !channel
  and string_lexbuf = Lexing.from_channel !string_channel
  and list_lexbuf = Lexing.from_channel !list_channel in
  let ast = Parser.program Scanner.token lexbuf in
  let string_ast = parse string_lexbuf in
  let list_ast = parse list_lexbuf in
  (* prepend standard library to program *)
  (* let ast = (fst ast, snd ast) in *)
  let ast =
    ( fst string_ast @ fst list_ast @ fst ast
    , snd string_ast @ snd list_ast @ snd ast )
  in
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
