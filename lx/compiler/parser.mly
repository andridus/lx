%{
open Ast

(* Helper function to convert menhir position to our position type *)
let make_position pos =
  { line = pos.Lexing.pos_lnum; column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1; filename = None }
%}

(* Tokens *)
%token <string> IDENT UPPER_IDENT STRING ATOM
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NIL
%token MODULE_MACRO

(* Keywords *)
%token PUB FUN CASE IF ELSE FOR WHEN IN

(* OTP Keywords *)
%token WORKER SUPERVISOR STRATEGY CHILDREN
%token ONE_FOR_ONE ONE_FOR_ALL REST_FOR_ONE

(* Application Keywords *)
%token APPLICATION DESCRIPTION VSN APPLICATIONS REGISTERED ENV

(* Spec Keywords *)
%token SPEC REQUIRES ENSURES MATCHES

(* Testing Keywords *)
%token DESCRIBE TEST ASSERT

(* Operators and Punctuation *)
%token EQ ARROW PIPE WILDCARD
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DOT_LBRACE
%token COMMA SEMICOLON CONS COLON DOT
%token PLUS MINUS MULT DIV

%token EOF

(* Precedence and associativity *)
%right ARROW
%left PIPE
%left CONS
%left COMMA
%left PLUS MINUS
%left MULT DIV
%left DOT COLON (* Highest precedence for module calls *)

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
  | a = application_def { Application a }

function_def:
  (* Public functions - new syntax for multiple arities *)
  | PUB FUN name = IDENT LBRACE clauses = function_clause+ RBRACE
    { let pos = make_position $startpos in { name; clauses; visibility = Public; position = Some pos } }
  (* Public functions - backward compatibility: single clause with pattern support *)
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  (* Private functions (default) - new syntax for multiple arities *)
  | FUN name = IDENT LBRACE clauses = function_clause+ RBRACE
    { let pos = make_position $startpos in { name; clauses; visibility = Private; position = Some pos } }
  (* Private functions (default) - backward compatibility: single clause with pattern support *)
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN _name = IDENT LPAREN _params = separated_list(COMMA, pattern) RPAREN error
    { failwith "Enhanced:Missing function body - expected '{' after parameter list|Suggestion:Add '{' and '}' to define the function body|Context:function definition" }
  | FUN _name = IDENT error
    { failwith "Enhanced:Missing parameter list or clause block - expected '(' or '{' after function name|Suggestion:Use 'fun name() { body }' for single clause functions or 'fun name { (params) { body } }' for multiple clause functions|Context:function definition" }
  | FUN error
    { failwith "Enhanced:Missing function name after 'fun' keyword|Suggestion:Provide a valid identifier name for the function|Context:function definition" }

function_clause:
  | LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in { params; body; position = Some pos } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in { params; body = Literal LNil; position = Some pos } }
  | error
    { failwith "Enhanced:Invalid function clause syntax - expected parameter list in parentheses|Suggestion:For single clause functions use 'fun name() { body }', for multiple clause functions use 'fun name { (params) { body } }'|Context:function clause definition" }

otp_component:
  | w = worker_def { w }
  | s = supervisor_def { s }

worker_def:
  | WORKER name = IDENT LBRACE functions = function_def* specs = spec_def* RBRACE
    { let pos = make_position $startpos in
      Worker { name; functions; specs; position = Some pos } }

supervisor_def:
  | SUPERVISOR name = IDENT LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACKET children = separated_list(COMMA, IDENT) RBRACKET
    RBRACE
    { let pos = make_position $startpos in
      Supervisor { name = Some name; strategy; children = SimpleChildren children; position = Some pos } }

  | SUPERVISOR name = IDENT LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACE children_spec = children_specification RBRACE
    RBRACE
    { let pos = make_position $startpos in
      Supervisor { name = Some name; strategy; children = children_spec; position = Some pos } }

  | SUPERVISOR LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACKET children = separated_list(COMMA, IDENT) RBRACKET
    RBRACE
    { let pos = make_position $startpos in
      Supervisor { name = None; strategy; children = SimpleChildren children; position = Some pos } }

  | SUPERVISOR LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACE children_spec = children_specification RBRACE
    RBRACE
    { let pos = make_position $startpos in
      Supervisor { name = None; strategy; children = children_spec; position = Some pos } }

  (* Error cases *)
  | SUPERVISOR name = IDENT LBRACE
      STRATEGY strategy = otp_strategy
      CHILDREN _first_child = IDENT error
    { failwith ("Enhanced:Invalid supervisor children field - brackets are required around the children list|Suggestion:Change 'children " ^ _first_child ^ ", ...' to 'children [" ^ _first_child ^ ", ...]'|Context:Supervisor children must always be enclosed in brackets for consistency with list syntax|Example:supervisor " ^ name ^ " {\n  strategy " ^ (match strategy with OneForOne -> "one_for_one" | OneForAll -> "one_for_all" | RestForOne -> "rest_for_one") ^ "\n  children [" ^ _first_child ^ "]\n}") }
  | SUPERVISOR name = IDENT LBRACE
      STRATEGY _strategy = otp_strategy
      CHILDREN error
    { failwith ("Enhanced:Invalid supervisor children field - expected '[' or '{' after 'children' keyword|Suggestion:Use 'children [worker1, worker2]' or 'children { worker [worker1], supervisor [supervisor1] }'|Context:Supervisor children must be enclosed in brackets or braces|Example:supervisor " ^ name ^ " {\n  strategy one_for_one\n  children [worker1, worker2]\n}") }

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
    { let pos = make_position $startpos in Assign (name, value, Some pos) }
  | func = expr LPAREN args = separated_list(COMMA, expr) RPAREN
    { App (func, args) }
  (* Error case: detect incorrect colon syntax and provide helpful error *)
  | module_name = IDENT COLON func_name = IDENT LPAREN _args = separated_list(COMMA, expr) RPAREN %prec COLON
    { failwith ("Enhanced:Invalid module call syntax - use '.' instead of ':' for module calls|Suggestion:Change '" ^ module_name ^ ":" ^ func_name ^ "()' to '" ^ module_name ^ "." ^ func_name ^ "()'|Context:Lx uses dot notation for module calls, not colon notation") }
  | left = expr PLUS right = expr
    { BinOp (left, "+", right) }
  | left = expr MINUS right = expr
    { BinOp (left, "-", right) }
  | left = expr MULT right = expr
    { BinOp (left, "*", right) }
  | left = expr DIV right = expr
    { BinOp (left, "/", right) }
  | IF cond = expr LBRACE then_expr = expr RBRACE ELSE LBRACE else_expr = expr RBRACE
    { If (cond, then_expr, Some else_expr) }
  | IF cond = expr LBRACE then_expr = expr RBRACE
    { If (cond, then_expr, None) }
  | IF _cond = expr LBRACE error
    { failwith "Enhanced:Missing expression in if block|Suggestion:Add an expression inside the braces|Context:if statement" }
  | IF _cond = expr error
    { failwith "Enhanced:Missing opening brace in if statement|Suggestion:Add '{' after the condition|Context:if statement" }
  | IF error
    { failwith "Enhanced:Missing condition after 'if' keyword|Suggestion:Add a boolean expression after 'if'|Context:if statement" }
  | CASE value = expr LBRACE cases = case_branch* RBRACE
    { Match (value, cases) }
  | FOR var = IDENT IN iterable = expr LBRACE body = expr RBRACE
    { For (var, iterable, body) }

simple_expr:
  | l = literal { Literal l }
  | name = IDENT { Var name }
  | MODULE_MACRO { Var "?MODULE" }
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

application_def:
  | APPLICATION LBRACE fields = application_field* RBRACE
    { let description = ref "Generated with LX" in
      let vsn = ref "0.1.0" in
      let applications = ref None in
      let registered = ref None in
      let env = ref None in
      List.iter (function
        | `Description s -> description := s
        | `Vsn s -> vsn := s
        | `Applications apps -> applications := Some apps
        | `Registered regs -> registered := Some regs
        | `Env envs -> env := Some envs
      ) fields;
      { description = !description; vsn = !vsn; applications = !applications;
        registered = !registered; env = !env } }

application_field:
  | DESCRIPTION desc = STRING { `Description desc }
  | VSN version = STRING { `Vsn version }
  | APPLICATIONS LBRACKET apps = separated_list(COMMA, IDENT) RBRACKET { `Applications apps }
  | REGISTERED LBRACKET regs = separated_list(COMMA, IDENT) RBRACKET { `Registered regs }
  | ENV LBRACKET envs = separated_list(COMMA, env_pair) RBRACKET { `Env envs }

env_pair:
  | key = IDENT COLON value = expr { (key, value) }

children_specification:
  | WORKER LBRACKET workers = separated_list(COMMA, IDENT) RBRACKET
    { TypedChildren { workers; supervisors = [] } }
  | SUPERVISOR LBRACKET supervisors = separated_list(COMMA, IDENT) RBRACKET
    { TypedChildren { workers = []; supervisors } }
  | WORKER LBRACKET workers = separated_list(COMMA, IDENT) RBRACKET
    SUPERVISOR LBRACKET supervisors = separated_list(COMMA, IDENT) RBRACKET
    { TypedChildren { workers; supervisors } }
  | SUPERVISOR LBRACKET supervisors = separated_list(COMMA, IDENT) RBRACKET
    WORKER LBRACKET workers = separated_list(COMMA, IDENT) RBRACKET
    { TypedChildren { workers; supervisors } }