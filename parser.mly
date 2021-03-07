/* Ocamlyacc parser for TEAM */

%{
open Ast
let thrd (_,_,a) = a;;
%} 

%token = LPAREN RPAREN LSQUARE RSQUARE SEMI COMMA ARROW
%token = PLUS MINUS TIMES DIVIDE MODULUS
%token = ADDASN SUBASN MULASN DIVASN MODASN ASSIGN NOT
%token = EQ NEQ LT LEQ GT GEQ RANGE AND OR 
%token = IF ELSEIF ELSE FOR IN DO WHILE BREAK CONTINUE RETURN END 
%token = INT FLOAT BOOL STRING CHAR VOID
%token = <bool> BOOL_LITERAL
%token = <int> INT_LITERAL
%token = <float> FLOAT_LITERAL
%token = <string> ID
%token = <char> CHAR_LITERAL 
%token = <string> STRING_LITERAL
%token = EOL EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%right ADDASN SUBASN MULASN DIVASN MODASN
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MODULUS
%right NOT

%%

program:
  decls EOF { $1 }

decls:
	/* nothing */ { ([], [], []) }
	| decls vdecl { (($2 :: fst $1), snd $1, thrd $1) }
	| decls fdecl { (fst $1, ($2 :: snd $1), thrd $1) }
	| decls stmt  { (fst $1, snd $1, ($2 :: thrd $1)) }

fdecl:
	typ ID LPAREN formals_opt RPAREN EOL fbody END EOL
	{ {
		typ = $1;
		fname = $2;
		formals = $4;
		body = { vdecls = List.rev $7.vdecls; stmts = List.rev $7.stmts }
	} }

fbody:
  /* nothing */ { { vdecls = []; stmts = [] } }
	| fbody vdecl { { vdecls = $2 :: $1.vdecls; stmts = $1.stmts; } }
	| fbody stmt  { { vdecls = $1.vdecls; stmts = $2 :: $1.stmts; } }

formals_opt:
  /* nothing */  { [] }
	| formal_list { List.rev $1 }

formals_list:
		typ ID { [($1, $2)] }
	| formals_list COMMA typ ID { ($3, $4) :: $1 }

typ: 
		INT { Int }
	| FLOAT { Float }
	| BOOL { Bool }
	| STRING { String }
	| VOID { Void }
	| CHAR { Char }
	| LIST LT typ GT { List $3 }
	| LPAREN typ_list RPAREN { $2 }
	| typ_list ARROW typ { Func(List.rev $1, $3) }

typ_list:
		typ { [$1] }
	| typ_list ARROW typ { $3 :: $1 }

vdecl:
	typ ID ASSIGN expr EOL { $1, $2, $4 }

stmt_list: 
	/* nothing */ { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	EOL { Nostmt }
  | expr EOL { Expr $1 }
	| RETURN expr EOL { Return $2 }
	| IF internal_if EOL { $2 } 
	| FOR expr IN expr stmt_list END EOL { $2, $4, Block(List.rev $6) }
	| WHILE expr stmt_list END EOL  { $2, Block(List.rev $4) }
	| BREAK EOL { Break }
	| CONTINUE EOL { Continue }
	| vdecl EOL { Declaration($1) }

internal_if:
	expr EOL stmt_list elif_list else_list END { If($1, Block(List.rev $3), Block(List.rev $4), Block(List.rev $5))}

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
	INT_LITERAL { IntLit($1) }
  | FLOAT_LITERAL { FloatLit($1) }
  | BOOL_LITERAL { BoolLit($1) }
  | CHAR_LITERAL { CharLit($1) }
  | STRING_LITERAL {StringLit($1)}
  | LSQUARE list_literal RSQUARE { ListLit(List.rev $2) }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | LPAREN expr RPAREN { $2                   }
  | ID ASSIGN expr { Assign($1, $3) }
	| ID LPAREN args_opt RPAREN { Call($1, $3) }
	| expr LSQUARE slce RSQUARE { SliceExpr($1, $3) }

list_literal:
	/* nothing */ { [] }
	| expr        { [$1] }
	| list_literal COMMA expr { $3 :: $1 }

args_opt:
	/* nothing */ { [] }
	| args_list { List.rev $1 }

args_list:
	expr { [$1] }
	| args_list COMMA expr { $3 :: $1 }