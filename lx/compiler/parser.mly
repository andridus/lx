%{
open Ast
%}

%token <string> IDENT
%token <string> STRING
%token <int> INT
%token LET EQ IN
%token EOF

%start <program> main
%%

main:
  | expr EOF { Expr $1 }

expr:
  | LET IDENT EQ expr IN expr { Let ($2, $4) }
  | IDENT { Var $1 }
  | INT { Literal (LInt $1) }
  | STRING { Literal (LString $1) }