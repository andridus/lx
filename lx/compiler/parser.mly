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
%token FUN CASE IF THEN ELSE FOR WHEN IN

(* OTP Keywords *)
%token WORKER SUPERVISOR STRATEGY CHILDREN
%token ONE_FOR_ONE ONE_FOR_ALL REST_FOR_ONE

(* Spec Keywords *)
%token SPEC REQUIRES ENSURES MATCHES

(* Testing Keywords *)
%token DESCRIBE TEST ASSERT

(* Operators and Punctuation *)
%token EQ ARROW PIPE WILDCARD
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DOT_LBRACE
%token COMMA SEMICOLON CONS DOT

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
  (* New syntax for multiple arities *)
  | FUN name = IDENT LBRACE clauses = function_clause+ RBRACE
    { { name; clauses } }
  (* Backward compatibility: single clause *)
  | FUN name = IDENT LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE body = function_body RBRACE
    { make_single_clause_function name params body }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE RBRACE
    { make_single_clause_function name params (Literal LNil) }
  | FUN _name = IDENT LPAREN _params = separated_list(COMMA, IDENT) RPAREN error
    { failwith "Enhanced:Missing function body - expected '{' after parameter list|Suggestion:Add '{' and '}' to define the function body|Context:function definition" }
  | FUN _name = IDENT error
    { failwith "Enhanced:Missing parameter list or clause block - expected '(' or '{' after function name|Suggestion:Add '()' for parameters or '{}' for multiple clauses|Context:function definition" }
  | FUN error
    { failwith "Enhanced:Missing function name after 'fun' keyword|Suggestion:Provide a valid identifier name for the function|Context:function definition" }

function_clause:
  | LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE body = function_body RBRACE
    { { params; body } }
  | LPAREN params = separated_list(COMMA, IDENT) RPAREN LBRACE RBRACE
    { { params; body = Literal LNil } }

otp_component:
  | w = worker_def { w }
  | s = supervisor_def { s }

worker_def:
  | WORKER name = IDENT LBRACE
      functions = function_def*
      specs = spec_def*
    RBRACE
    { Worker { name; functions; specs } }

supervisor_def:
  | SUPERVISOR name = IDENT LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN children = separated_list(COMMA, IDENT)
    RBRACE
    { Supervisor { name; strategy; children } }

otp_strategy:
  | ONE_FOR_ONE { OneForOne }
  | ONE_FOR_ALL { OneForAll }
  | REST_FOR_ONE { RestForOne }

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
  | IDENT LBRACE tests = test_def* RBRACE
    { if $1 = "describe" then { name = ""; tests } else failwith "Expected 'describe' keyword" }
  | IDENT name = STRING LBRACE tests = test_def* RBRACE
    { if $1 = "describe" then { name; tests } else failwith "Expected 'describe' keyword" }
  | IDENT error
    { if $1 = "describe" then
        failwith "Enhanced:The describe block must be followed by a description string|Suggestion:Use 'describe \"description\" { ... }'|Context:test block definition"
      else failwith "Expected 'describe' keyword" }

test_def:
  | IDENT name = STRING LBRACE body = expr RBRACE
    { if $1 = "test" then { name; body } else failwith "Expected 'test' keyword" }
  | IDENT error
    { if $1 = "test" then
        failwith "Enhanced:The test block must be followed by a description string|Suggestion:Use 'test \"description\" { ... }'|Context:test definition"
      else failwith "Expected 'test' keyword" }

expr:
  | e = simple_expr { e }
  | name = IDENT EQ value = expr
    { Assign (name, value) }
  | func = expr LPAREN args = separated_list(COMMA, expr) RPAREN
    { App (func, args) }
  | IF cond = expr THEN then_expr = expr ELSE else_expr = expr
    { If (cond, then_expr, Some else_expr) }
  | IF cond = expr THEN then_expr = expr
    { If (cond, then_expr, None) }
  | IF _cond = expr THEN error
    { failwith "Enhanced:Missing expression after 'then' in if statement|Suggestion:Add an expression after 'then'|Context:if statement" }
  | IF _cond = expr error
    { failwith "Enhanced:Missing 'then' keyword in if statement|Suggestion:Add 'then' after the condition|Context:if statement" }
  | IF error
    { failwith "Enhanced:Missing condition after 'if' keyword|Suggestion:Add a boolean expression after 'if'|Context:if statement" }
  | CASE value = expr LBRACE cases = case_branch* RBRACE
    { Match (value, cases) }
  | FOR var = IDENT IN iterable = expr LBRACE body = expr RBRACE
    { For (var, iterable, body) }

simple_expr:
  | l = literal { Literal l }
  | name = IDENT { Var name }
  (* External function call: module.function(args) *)
  | module_name = IDENT DOT func_name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { ExternalCall (module_name, func_name, args) }
  (* Module reference without call (backward compatibility) *)
  | module_name = IDENT DOT func_name = IDENT { Var (module_name ^ "." ^ func_name) }
  | LPAREN e = expr RPAREN { e }
  | LPAREN elements = separated_list(COMMA, expr) RPAREN
    { match elements with
      | [] -> Tuple []
      | [e] -> e  (* Single element in parentheses is just grouping *)
      | es -> Tuple es }
  (* Tuple syntax with .{} *)
  | DOT_LBRACE elements = separated_list(COMMA, expr) RBRACE
    { Tuple elements }
  | LBRACKET elements = separated_list(COMMA, expr) RBRACKET
    { List elements }
  (* Block expressions *)
  | LBRACE statements = statement_list RBRACE
    { match statements with
      | [e] -> e
      | es -> Block es }
  | LBRACE RBRACE
    { Literal LNil }

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
  (* Tuple pattern syntax with .{} *)
  | DOT_LBRACE patterns = separated_list(COMMA, pattern) RBRACE
    { PTuple patterns }
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
  | IDENT name = STRING LBRACE body = expr RBRACE
    { if $1 = "test" then { name; body } else failwith "Expected 'test' keyword" }

function_body:
  | e = expr { e }
  | e = expr SEMICOLON rest = function_body
    { match rest with
      | Sequence exprs -> Sequence (e :: exprs)
      | single_expr -> Sequence [e; single_expr] }
  | statements = statement_list
    { match statements with
      | [e] -> e
      | es -> Sequence es }

statement_list:
  | s = statement { [s] }
  | s = statement rest = statement_list { s :: rest }

statement:
  | e = expr { e }