/* Ocamlyacc parser for TEAM */

// %{
// open Ast
// let fst  (a, _, _) = a;;
// let snd  (_, a, _) = a;;
// let thrd (_, _, a) = a;;
// %} 

%{
open Ast
let fst  (a, _) = a;;
let snd  (_, a) = a;;
%} 

%token LPAREN RPAREN LSQUARE RSQUARE COMMA ARROW COLON
%token PLUS MINUS TIMES DIVIDE MOD EXP
%token ADDASN SUBASN MULASN DIVASN MODASN ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ RANGE AND OR 
%token IF ELSEIF ELSE FOR IN DO WHILE BREAK CONTINUE RETURN END 
%token INT FLOAT BOOL STRING CHAR VOID
%token LIST FILE
%token <bool> BLIT
%token <int> LITERAL
%token <float> FLIT
%token <string> ID
%token <char> CLIT
%token <string> SLIT
%token EOL EOF

%start program
%type <Ast.program> program

%left ARROW
%right ASSIGN ADDASN SUBASN MULASN DIVASN MODASN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%nonassoc RANGE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left EXP
%right NOT
%nonassoc LSQUARE

%%

program:
  decls EOF { $1 }

decls:
	// /* nothing */ { ([], [], []) }
	// | decls vdecl { (($2 :: fst $1), snd $1, thrd $1) }
	// | decls fdecl { (fst $1, ($2 :: snd $1), thrd $1) }
	// | decls stmt  { (fst $1, snd $1, ($2 :: thrd $1)) }

	// /* nothing */ { ([], []) }
	| decls fdecl { (($2 :: fst $1), snd $1) }
	| decls stmt  { (fst $1, ($2 :: snd $1)) }

fdecl:
	typ ID LPAREN formals_opt RPAREN EOL stmt_list END EOL
	{ {
		typ = $1;
		fname = $2;
		formals = $4;
		body = List.rev $7;
		
	} }

	// body = { vdecls = List.rev $7.vdecls; stmts = List.rev $7.stmts }

// fbody:
//   /* nothing */ { { vdecls = []; stmts = [] } }
//   | fbody vdecl { { vdecls = $2 :: $1.vdecls; stmts = $1.stmts; } }
// 	| fbody stmt  { { vdecls = $1.vdecls; stmts = $2 :: $1.stmts; } }

formals_opt:
  /* nothing */  { [] }
	| formals_list { List.rev $1 }

formals_list:
		typ ID { [($1, $2)] }
	| formals_list COMMA typ ID { ($3, $4) :: $1 }

typ: 
		INT    { Int }
	| FLOAT  { Float }
	| BOOL   { Bool }
	| CHAR   { Char }
	| STRING { String }
	| VOID   { Void }
	| LIST LT typ GT { List $3 }
	| LPAREN typ RPAREN { $2 }
	| typ ARROW typ { Func($1, $3) }

vdecl:
	typ ID ASSIGN expr EOL { Declaration($1, $2, $4) }

stmt_list: 
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  EOL { Nostmt }
	| vdecl { $1 } 
  | expr EOL { Expr $1 }
	| RETURN expr_opt EOL { Return $2 }
	| IF internal_if EOL { $2 } 
	| FOR expr IN expr DO stmt_list END EOL { For($2, $4, Block(List.rev $6)) }
	| WHILE expr DO stmt_list END EOL  { While($2, Block(List.rev $4)) }
	| BREAK EOL { Break }
	| CONTINUE EOL { Continue }

internal_if:
	expr DO EOL stmt_list elif_list else_list END { If($1, Block(List.rev $4), Block(List.rev $5), Block(List.rev $6))}

elif_list:
	/* nothing */ { [] }
	| elif elif_list { $1 :: $2 }

elif:
	ELSEIF expr EOL stmt_list { Elif($2, Block(List.rev $4)) }

else_list:
	/* nothing */ { [] }
	| ELSE EOL stmt_list { $3 }

expr_opt:
	/* nothing */ { Noexpr }
	| expr { $1 }

expr:
	  LITERAL { IntLit($1) }
  | BLIT    { BoolLit($1) }
  | CLIT    { CharLit($1) }
  | SLIT    {StringLit($1)}
	| ID { Id($1) }
  | LSQUARE list_literal RSQUARE { ListLit(List.rev $2) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
	| expr EXP    expr { Binop($1, Exp,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr RANGE  expr { Binop($1, Range, $3) }
	| FLIT    { FloatLit($1) }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT   expr           { Unop(Not, $2)      }
  | LPAREN expr RPAREN   { $2                 }
  | ID ASSIGN expr { Assign($1, $3) }
  | ID ADDASN expr { AssignOp($1, Add, $3) }
  | ID SUBASN expr { AssignOp($1, Sub, $3) }
  | ID MULASN expr { AssignOp($1, Mult, $3) }
  | ID DIVASN expr { AssignOp($1, Div, $3) }
  | ID MODASN expr { AssignOp($1, Mod, $3) }
	| ID LSQUARE expr RSQUARE ASSIGN expr { ListAssign($1, $3, $6)}
	| ID LPAREN args_opt RPAREN { Call($1, $3) }
	| ID LSQUARE expr RSQUARE { SliceExpr($1, Index($3)) }
	| ID LSQUARE expr COLON expr RSQUARE { SliceExpr($1, Slice($3, $5)) }
	| ID LSQUARE COLON expr RSQUARE { SliceExpr($1, Slice(IntLit 0, $4)) }
	| ID LSQUARE expr COLON RSQUARE { SliceExpr($1, Slice($3, End)) }

list_literal:
	/* nothing */ { [] }
	| expr        { [$1] }
	| list_literal COMMA expr { $3 :: $1 }

args_opt:
	/* nothing */ { [] }
	| args_list   { List.rev $1 }

args_list:
	  expr { [$1] }
	| args_list COMMA expr { $3 :: $1 }
