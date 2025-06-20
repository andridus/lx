%{
open Ast
%}

(* Tokens *)
%token <string> IDENT UPPER_IDENT STRING ATOM
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NIL

(* Keywords *)
%token LET IN FUN CASE IF THEN ELSE FOR WHEN

(* OTP Keywords *)
%token WORKER SUPERVISOR STRATEGY CHILDREN
%token ONE_FOR_ONE ONE_FOR_ALL REST_FOR_ONE

(* Handler Keywords *)
%token INIT CALL CAST INFO TERMINATE

(* Spec Keywords *)
%token SPEC REQUIRES ENSURES MATCHES

(* Testing Keywords *)
%token DESCRIBE TEST ASSERT

(* Operators and Punctuation *)
%token EQ ARROW PIPE WILDCARD
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMICOLON CONS

%token EOF

(* Precedence and associativity *)
%right ARROW
%left PIPE
%left CONS
%left COMMA

%start <program> main

%on_error_reduce expr
%on_error_reduce simple_expr
%on_error_reduce function_def

%%

main:
  | items = module_item* EOF { { items } }

module_item:
  | f = function_def { Function f }
  | c = otp_component { OtpComponent c }
  | s = spec_def { Spec s }
  | t = test_block { Test t }
  | standalone_test = standalone_test_def { Test { name = "Standalone Tests"; tests = [standalone_test] } }

function_def:
  | FUN name = IDENT LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE body = expr RBRACE
    { { name; params; body } }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE RBRACE
    { { name; params; body = Literal LNil } }
  | FUN _name = IDENT LPAREN _params = separated_list(COMMA, IDENT) RPAREN error
    { failwith "Missing function body - expected '{' after parameter list" }
  | FUN _name = IDENT error
    { failwith "Missing parameter list - expected '(' after function name" }
  | FUN error
    { failwith "Missing function name after 'fun' keyword" }

otp_component:
  | w = worker_def { w }
  | s = supervisor_def { s }

worker_def:
  | WORKER name = IDENT LBRACE
      handlers = handler_def*
      functions = function_def*
      specs = spec_def*
    RBRACE
    { Worker { name; handlers; functions; specs } }

supervisor_def:
  | SUPERVISOR name = IDENT LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN children = separated_list(COMMA, IDENT)
    RBRACE
    { Supervisor { name; strategy; children } }

handler_def:
  | handler = otp_handler LBRACE body = function_def RBRACE
    { (handler, body) }

otp_strategy:
  | ONE_FOR_ONE { OneForOne }
  | ONE_FOR_ALL { OneForAll }
  | REST_FOR_ONE { RestForOne }

otp_handler:
  | INIT { Init }
  | CALL { Call }
  | CAST { Cast }
  | INFO { Info }
  | TERMINATE { Terminate }

spec_def:
  | SPEC name = IDENT LBRACE RBRACE
    { { name; requires = []; ensures = [] } }
  | SPEC name = IDENT LBRACE REQUIRES requires = separated_list(COMMA, spec_expr) RBRACE
    { { name; requires; ensures = [] } }
  | SPEC name = IDENT LBRACE ENSURES ensures = separated_list(COMMA, spec_expr) RBRACE
    { { name; requires = []; ensures } }
  | SPEC name = IDENT LBRACE
      REQUIRES requires = separated_list(COMMA, spec_expr)
      ENSURES ensures = separated_list(COMMA, spec_expr)
    RBRACE
    { { name; requires; ensures } }

spec_expr:
  | e = expr { e }

test_block:
  | DESCRIBE name = STRING LBRACE tests = test_def* RBRACE
    { { name; tests } }

test_def:
  | TEST name = STRING LBRACE body = expr RBRACE
    { { name; body } }

expr:
  | e = simple_expr { e }
  | LET name = IDENT EQ value = expr IN body = expr
    { Let (name, value, body) }
  | func = expr LPAREN args = separated_list(COMMA, expr) RPAREN
    { App (func, args) }
  | IF cond = expr THEN then_expr = expr ELSE else_expr = expr
    { If (cond, then_expr, Some else_expr) }
  | IF cond = expr THEN then_expr = expr
    { If (cond, then_expr, None) }
  | IF _cond = expr THEN error
    { failwith "Missing expression after 'then' in if statement" }
  | IF _cond = expr error
    { failwith "Missing 'then' keyword in if statement" }
  | IF error
    { failwith "Missing condition after 'if' keyword" }
  | CASE value = expr LBRACE cases = case_branch* RBRACE
    { Match (value, cases) }
  | FOR var = IDENT IN iterable = expr LBRACE body = expr RBRACE
    { For (var, iterable, body) }

simple_expr:
  | l = literal { Literal l }
  | name = IDENT { Var name }
  | LPAREN e = expr RPAREN { e }
  | LPAREN elements = separated_list(COMMA, expr) RPAREN
    { match elements with
      | [] -> Tuple []
      | [e] -> e  (* Single element in parentheses is just grouping *)
      | es -> Tuple es }
  | LBRACKET elements = separated_list(COMMA, expr) RBRACKET
    { List elements }

case_branch:
  | pattern = pattern ARROW body = expr
    { (pattern, body) }

pattern:
  | p = simple_pattern { p }
  | head = pattern CONS tail = pattern { PCons (head, tail) }

simple_pattern:
  | WILDCARD { PWildcard }
  | name = IDENT { PVar name }
  | atom = ATOM { PAtom atom }
  | l = literal { PLiteral l }
  | LPAREN patterns = separated_list(COMMA, pattern) RPAREN
    { match patterns with
      | [] -> PTuple []
      | [p] -> p
      | ps -> PTuple ps }
  | LBRACKET patterns = separated_list(COMMA, pattern) RBRACKET
    { PList patterns }

literal:
  | s = STRING { LString s }
  | i = INT { LInt i }
  | f = FLOAT { LFloat f }
  | b = BOOL { LBool b }
  | a = ATOM { LAtom a }
  | NIL { LNil }

standalone_test_def:
  | TEST name = STRING LBRACE body = expr RBRACE
    { { name; body } }