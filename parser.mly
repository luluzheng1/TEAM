/* Ocamlyacc parser for TEAM */

%{
open Ast
%} 

%token LPAREN RPAREN LSQUARE RSQUARE COMMA ARROW COLON SEMI
%token PLUS MINUS TIMES DIVIDE MOD EXP
%token ADDASN SUBASN MULASN DIVASN MODASN ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ RANGE AND OR 
%token IF ELSEIF ELSE FOR IN WHILE BREAK CONTINUE RETURN END 
%token INT FLOAT BOOL STRING CHAR VOID
%token LIST FILE
%token <bool> BLIT
%token <int> LITERAL
%token <float> FLIT
%token <string> ID
%token <char> CLIT
%token <string> SLIT
%token EOF

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

%%

program:
  decls EOF { (List.rev (fst $1), List.rev (snd $1)) }

decls:
  /* nothing */ { ([], []) }
  | decls fdecl { (($2 :: fst $1), snd $1) }
  | decls stmt  { (fst $1, ($2 :: snd $1)) }

fdecl:
  typ ID LPAREN formals_opt RPAREN COLON stmt_list END
  { {
    typ = $1;
    fname = $2;
    formals = $4;
    body = List.rev $7;
  } }
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
  | FILE   { File }
  | LIST LT typ GT { List $3 }
  | typ_list ARROW typ { Func($1, $3) }

typ_list_helper:
    typ { [$1] }
  | typ_list_helper COMMA typ { $3 :: $1 }

typ_list:
    LPAREN RPAREN { [] }
  | LPAREN typ_list_helper RPAREN { List.rev $2 } 

vdecl:
    typ ID ASSIGN expr { Declaration($1, $2, $4) }
  | typ ID { Declaration($1, $2, Noexpr) }

stmt_list: 
  /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | vdecl SEMI { $1 } 
  | expr SEMI { Expr $1 }
  | RETURN expr_opt SEMI { Return $2 }
  | IF internal_if { $2 } 
  | FOR ID IN expr COLON stmt_list END { For($2, $4, Block(List.rev $6)) }
  | WHILE expr COLON stmt_list END  { While($2, Block(List.rev $4)) }
  | BREAK SEMI { Break }
  | CONTINUE SEMI { Continue }

internal_if:
  expr COLON stmt_list else_list END { If($1, Block(List.rev $3), $4) }

else_list:
  /* nothing */ { Block([]) }
  | ELSEIF expr COLON stmt_list else_list { If($2, Block(List.rev $4), $5) }
  | ELSE COLON stmt_list { Block($3)   }

expr_opt:
  /* nothing */ { Noexpr }
  | expr { $1 }

primary:
    LITERAL { IntLit($1) }
  | BLIT    { BoolLit($1) }
  | FLIT    { FloatLit($1) }
  | CLIT    { CharLit($1) }
  | SLIT    { StringLit($1) }
  | ID      { Id($1) }
  | LSQUARE list_literal RSQUARE { ListLit(List.rev $2) }
  | LPAREN expr RPAREN   { $2 }

bracket_expr:
    primary {$1}
  | bracket_expr LSQUARE index RSQUARE {SliceExpr($1, $3)}
  | bracket_expr LPAREN args_opt RPAREN { Call($1, $3) }

unary_expr:
    bracket_expr {$1}
  | MINUS unary_expr {Unop(Neg, $2)}
  | NOT unary_expr {Unop(Not, $2)}

expr:
    unary_expr {$1}
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
  | expr RANGE  expr { Binop($1, Range, $3)   }
  | expr ASSIGN expr { Assign($1, $3) }
  | expr ADDASN expr { Assign($1, Binop($1, Add, $3)) }
  | expr SUBASN expr { Assign($1, Binop($1, Sub, $3)) }
  | expr MULASN expr { Assign($1, Binop($1, Mult, $3))  }
  | expr DIVASN expr { Assign($1, Binop($1, Div, $3))  }
  | expr MODASN expr { Assign($1, Binop($1, Mod, $3))  }

index:
    expr { Index $1 }
  | expr COLON expr { Slice($1, $3) }
  | COLON expr { Slice(IntLit 0, $2) }
  | expr COLON { Slice($1, End) }
  | COLON { Slice(IntLit 0, End) }

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