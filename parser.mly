/* Ocamlyacc parser for TEAM */

%{
open Ast
%} 

%token = LPAREN RPAREN LSQUARE RSQUARE SEMI COMMA
%token = PLUS MINUS TIMES DIVIDE MODULUS
%token = ADDASN SUBASN MULASN DIVASN MODASN ASSIGN NOT
%token = EQ NEQ LT LEQ GT GEQ RANGE AND OR 
%token = IF ELSEIF ELSE FOR IN DO WHILE BREAK CONTINUE RETURN END 
%token = INT FLOAT BOOL STRING CHAR VOID
%token = LIST HASH
%token = <bool> BOOL_LITITERAL
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

