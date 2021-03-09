(* Ocamllex scanner for TEAM *)

{ 
  open Parser
  let unescape s =
    Scanf.sscanf ("\"" ^ s ^ "\"") "%S%!" (fun x -> x)
}

let digit = ['0' - '9']
let digits = digit+
let float = digits '.' digit*
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let char = ''' (ascii | digit) '''
let escaped_char = '\\' ['\\' ''' '"' 'n' 'r' 't']
let string = '"' ( (ascii | escaped_char)* as s) '"'

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }    (* Blocky Comments *)
| "//"     { slcomment lexbuf }    (* Single line Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '['      { LSQUARE }
| ']'      { RSQUARE } 
| ';'      { SEMI }
| ":"      { COLON }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'      { MOD }
| "+="     { ADDASN }
| "-="     { SUBASN }
| "*="     { MULASN }
| "/="     { DIVASN }
| "%="     { MODASN }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| ".."     { RANGE }
| "and"    { AND }
| "or"     { OR }
| "not"      { NOT }
| "if"     { IF }
| "elif"   { ELSEIF }
| "else"   { ELSE }
| "for"    { FOR }
| "in"     { IN }
| "do"     { DO }
| "while"  { WHILE }
| "break"  { BREAK }
| "continue" { CONTINUE }
| "return" { RETURN }
| "end"    { END }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "string" { STRING }
| "char"   { CHAR }
| "void"   { VOID }
| "true"   { BLIT(true) }
| "false"  { BLIT(false) }
| "list "   { LIST }
| "arrow" { ARROW }
(* | "import" { IMPORT }
| "as"     { AS } *)
| digits as lxm { LITERAL(int_of_string lxm) }
| float as lxm { FLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| char as lxm  { CLIT( String.get lxm 1 ) }
| string    { SLIT(unescape s) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and slcomment = parse
  '\n' { token lexbuf }
| _    { slcomment lexbuf }