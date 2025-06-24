%{
open Ast

(* Helper function to convert menhir position to our position type *)
let make_position pos =
  { line = pos.Lexing.pos_lnum; column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1; filename = None }

(* Helper function to convert expressions to patterns for semantic pattern matching *)
let rec expr_to_pattern = function
  | Var name -> PVar name
  | Literal lit -> PLiteral lit
  | Tuple exprs -> PTuple (List.map expr_to_pattern exprs)
  | List exprs -> PList (List.map expr_to_pattern exprs)
  | MapCreate fields ->
    let convert_field = function
      | AtomKeyField (key, expr) -> AtomKeyPattern (key, expr_to_pattern expr)
      | GeneralKeyField (key_expr, expr) -> GeneralKeyPattern (key_expr, expr_to_pattern expr)
    in
    PMap (List.map convert_field fields)
  | RecordCreate (name, fields) ->
    let convert_field (field_name, expr) = (field_name, expr_to_pattern expr) in
    PRecord (name, List.map convert_field fields)
  | _ -> failwith "Enhanced:Invalid pattern in assignment|Suggestion:Use valid pattern syntax for destructuring|Context:pattern matching assignment"

(* Helper types and functions for tuple detection *)
type simple_tuple_element =
  | STEIdent of string
  | STEAtom of string
  | STELiteral of literal

let string_of_literal = function
  | LString s -> "\"" ^ s ^ "\""
  | LInt i -> string_of_int i
  | LFloat f -> string_of_float f
  | LBool true -> "true"
  | LBool false -> "false"
  | LAtom a -> ":" ^ a
  | LNil -> "nil"

let string_of_simple_tuple_element = function
  | STEIdent s -> s
  | STEAtom a -> ":" ^ a
  | STELiteral lit -> string_of_literal lit
%}

(* Tokens *)
%token <string> IDENT UPPER_IDENT STRING ATOM
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NIL
%token MODULE_MACRO

(* Keywords *)
%token PUB FUN CASE IF ELSE FOR WHEN IN AND OR NOT ANDALSO ORELSE RECEIVE AFTER MATCH_KEYWORD
%token RECORD

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
%token EQEQ NEQ LT GT LEQ GEQ
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token DOT_LBRACE PERCENT_LBRACE PERCENT_LT PERCENT
%token COMMA SEMICOLON CONS COLON DOT
%token CONCAT PLUS MINUS MULT DIV SEND
%token ARROW_DOUBLE MATCH_ASSIGN PATTERN_MATCH

%token EOF

(* Precedence and associativity *)
%right ARROW
%left PIPE
%left CONS
%left COMMA
%left ORELSE
%left ANDALSO
%left OR
%left AND
%right NOT
%right SEND (* Send operator - right associative, lower precedence *)
%left EQEQ NEQ LT GT LEQ GEQ
%left CONCAT (* String concatenation *)
%left PLUS MINUS
%left MULT DIV
%left DOT COLON (* Highest precedence for module calls *)
%left ARROW_DOUBLE (* Map arrow syntax *)
%right EQ (* Assignment has low precedence *)
%right PATTERN_MATCH (* Pattern matching assignment *)

%start <program> main

%on_error_reduce expr
%on_error_reduce simple_expr
%on_error_reduce function_def
%on_error_reduce map_pattern_field
%on_error_reduce map_field

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
  | r = record_def { RecordDef r }

function_def:
  (* Public functions - new syntax for multiple arities *)
  | PUB FUN name = IDENT LBRACE clauses = function_clause+ RBRACE
    { let pos = make_position $startpos in { name; clauses; visibility = Public; position = Some pos } }
  (* Public functions - backward compatibility: single clause with pattern support *)
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | PUB FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  (* Private functions (default) - new syntax for multiple arities *)
  | FUN name = IDENT LBRACE clauses = function_clause+ RBRACE
    { let pos = make_position $startpos in { name; clauses; visibility = Private; position = Some pos } }
  (* Private functions (default) - backward compatibility: single clause with pattern support *)
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | FUN _name = IDENT LPAREN _params = separated_list(COMMA, pattern) RPAREN error
    { failwith "Enhanced:Missing function body - expected '{' after parameter list|Suggestion:Add '{' and '}' to define the function body|Context:function definition" }
  | FUN _name = IDENT error
    { failwith "Enhanced:Missing parameter list or clause block - expected '(' or '{' after function name|Suggestion:Use 'fun name() { body }' for single clause functions or 'fun name { (params) { body } }' for multiple clause functions|Context:function definition" }
  | FUN error
    { failwith "Enhanced:Missing function name after 'fun' keyword|Suggestion:Provide a valid identifier name for the function|Context:function definition" }

function_clause:
  | LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in { params; body; position = Some pos; guard = Some guard } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE body = function_body RBRACE
    { let pos = make_position $startpos in { params; body; position = Some pos; guard = None } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr LBRACE RBRACE
    { let pos = make_position $startpos in { params; body = Literal LNil; position = Some pos; guard = Some guard } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN LBRACE RBRACE
    { let pos = make_position $startpos in { params; body = Literal LNil; position = Some pos; guard = None } }
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
    (* Universal pattern matching/binding operator - semantic analysis determines the type *)
  | left = expr EQ right = expr
    { let pos = make_position $startpos in
      match left with
      | Var name -> Assign (name, right, Some pos)  (* Simple variable binding *)
      | _ ->
        (* Convert expression to pattern for pattern matching *)
        let pattern = expr_to_pattern left in
        PatternMatch (pattern, right, Some pos)
    }
  (* Pattern matching operator using <- (always pattern matching, never binding) *)
  | left = expr PATTERN_MATCH right = expr
    { let pos = make_position $startpos in
      let pattern = expr_to_pattern left in
      PatternMatch (pattern, right, Some pos)
    }
  | func = expr LPAREN args = separated_list(COMMA, expr) RPAREN
    { App (func, args) }
  (* Error case: detect incorrect colon syntax and provide helpful error *)
  | module_name = IDENT COLON func_name = IDENT LPAREN _args = separated_list(COMMA, expr) RPAREN %prec COLON
    { failwith ("Enhanced:Invalid module call syntax - use '.' instead of ':' for module calls|Suggestion:Change '" ^ module_name ^ ":" ^ func_name ^ "()' to '" ^ module_name ^ "." ^ func_name ^ "()'|Context:Lx uses dot notation for module calls, not colon notation") }
  | left = expr CONCAT right = expr
    { BinOp (left, "++", right) }
  | left = expr PLUS right = expr
    { BinOp (left, "+", right) }
  | left = expr MINUS right = expr
    { BinOp (left, "-", right) }
  | left = expr MULT right = expr
    { BinOp (left, "*", right) }
  | left = expr DIV right = expr
    { BinOp (left, "/", right) }
  | left = expr EQEQ right = expr
    { BinOp (left, "==", right) }
  | left = expr NEQ right = expr
    { BinOp (left, "!=", right) }
  | left = expr LT right = expr
    { BinOp (left, "<", right) }
  | left = expr GT right = expr
    { BinOp (left, ">", right) }
  | left = expr LEQ right = expr
    { BinOp (left, "<=", right) }
  | left = expr GEQ right = expr
    { BinOp (left, ">=", right) }
  | left = expr AND right = expr
    { BinOp (left, "and", right) }
  | left = expr OR right = expr
    { BinOp (left, "or", right) }
  | left = expr ANDALSO right = expr
    { BinOp (left, "andalso", right) }
  | left = expr ORELSE right = expr
    { BinOp (left, "orelse", right) }
  | left = expr SEND right = expr
    { Send (left, right) }
  | NOT right = expr
    { UnaryOp ("not", right) }
  | IF cond = expr LBRACE then_statements = statement_list RBRACE ELSE LBRACE else_statements = statement_list RBRACE
    { let then_expr = match then_statements with
        | [e] -> e
        | es -> Block es
      in
      let else_expr = match else_statements with
        | [e] -> e
        | es -> Block es
      in
      If (cond, then_expr, Some else_expr) }
  | IF cond = expr LBRACE then_statements = statement_list RBRACE
    { let then_expr = match then_statements with
        | [e] -> e
        | es -> Block es
      in
      If (cond, then_expr, None) }
  | IF _cond = expr LBRACE error
    { failwith "Enhanced:Missing expression in if block|Suggestion:Add an expression inside the braces|Context:if statement" }
  | IF _cond = expr error
    { failwith "Enhanced:Missing opening brace in if statement|Suggestion:Add '{' after the condition|Context:if statement" }
  | IF error
    { failwith "Enhanced:Missing condition after 'if' keyword|Suggestion:Add a boolean expression after 'if'|Context:if statement" }
  | CASE value = expr LBRACE cases = nonempty_list(case_branch) RBRACE
    { Match (value, cases) }
  | FOR var = IDENT IN iterable = expr LBRACE body = expr RBRACE
    { For (var, iterable, body) }
  | RECEIVE LBRACE clauses = nonempty_list(receive_clause) RBRACE
    { Receive (clauses, None) }
  | RECEIVE LBRACE clauses = nonempty_list(receive_clause) RBRACE AFTER timeout = expr LBRACE timeout_body = expr RBRACE
    { Receive (clauses, Some (timeout, timeout_body)) }
  (* Record expressions *)
  | record_name = UPPER_IDENT LBRACE fields = separated_list(COMMA, record_field_init) RBRACE
    { RecordCreate (record_name, fields) }
  (* Map expressions *)
  | PERCENT_LBRACE fields = separated_list(COMMA, map_field) RBRACE
    { MapCreate fields }
  | LBRACE expr = expr PIPE updates = separated_list(COMMA, record_field_update) RBRACE
    { RecordUpdate (expr, updates) }

simple_expr:
  | l = literal { Literal l }
  | name = IDENT { Var name }
  | RECORD { Var "record" }
  | MODULE_MACRO { Var "?MODULE" }
  (* External function call: module.function(args) *)
  | module_name = IDENT DOT func_name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { ExternalCall (module_name, func_name, args) }
  (* Record field access *)
  | record_var = IDENT DOT field_name = IDENT
    { RecordAccess (Var record_var, field_name) }
  | RECORD DOT field_name = IDENT
    { RecordAccess (Var "record", field_name) }

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
  (* Smart error detection for potential tuple syntax misuse *)
  | LBRACE first_var = IDENT COMMA rest_vars = separated_nonempty_list(COMMA, IDENT) RBRACE
    { let all_vars = first_var :: rest_vars in
      let vars_str = String.concat ", " all_vars in
      let suggestion = ".{" ^ vars_str ^ "}" in
      failwith ("Enhanced:Invalid block syntax - looks like you're trying to create a tuple|Suggestion:Use '" ^ suggestion ^ "' for tuple syntax instead of '{" ^ vars_str ^ "}'|Context:In Lx, tuples are created with .{} syntax, not {} syntax. Curly braces {} are used for code blocks and function bodies") }
  (* Detect tuple-like patterns with atoms and literals *)
  | LBRACE first_atom = ATOM COMMA rest_elements = separated_nonempty_list(COMMA, simple_tuple_element) RBRACE
    { let first_str = ":" ^ first_atom in
      let rest_strs = List.map string_of_simple_tuple_element rest_elements in
      let all_elements = first_str :: rest_strs in
      let elements_str = String.concat ", " all_elements in
      let suggestion = ".{" ^ elements_str ^ "}" in
      failwith ("Enhanced:Invalid block syntax - looks like you're trying to create a tuple|Suggestion:Use '" ^ suggestion ^ "' for tuple syntax instead of '{" ^ elements_str ^ "}'|Context:In Lx, tuples are created with .{} syntax, not {} syntax. Curly braces {} are used for code blocks and function bodies") }
  (* Detect tuple-like patterns starting with literals *)
  | LBRACE first_lit = literal COMMA rest_elements = separated_nonempty_list(COMMA, simple_tuple_element) RBRACE
    { let first_str = string_of_literal first_lit in
      let rest_strs = List.map string_of_simple_tuple_element rest_elements in
      let all_elements = first_str :: rest_strs in
      let elements_str = String.concat ", " all_elements in
      let suggestion = ".{" ^ elements_str ^ "}" in
      failwith ("Enhanced:Invalid block syntax - looks like you're trying to create a tuple|Suggestion:Use '" ^ suggestion ^ "' for tuple syntax instead of '{" ^ elements_str ^ "}'|Context:In Lx, tuples are created with .{} syntax, not {} syntax. Curly braces {} are used for code blocks and function bodies") }

case_branch:
  | pattern = pattern WHEN guard = guard_expr ARROW body = expr
    { (pattern, Some guard, body) }
  | pattern = pattern ARROW body = expr
    { (pattern, None, body) }

receive_clause:
  | pattern = pattern WHEN guard = guard_expr ARROW body = expr
    { (pattern, Some guard, body) }
  | pattern = pattern ARROW body = expr
    { (pattern, None, body) }

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
  | LBRACKET head = pattern PIPE tail = pattern RBRACKET
    { PCons (head, tail) }
  | LBRACKET first = pattern COMMA rest = separated_nonempty_list(COMMA, pattern) PIPE tail = pattern RBRACKET
    { List.fold_right (fun elem acc -> PCons (elem, acc)) (first :: rest) tail }
  (* Record patterns *)
  | record_name = UPPER_IDENT LBRACE fields = separated_list(COMMA, record_pattern_field) RBRACE
    { PRecord (record_name, fields) }
  (* Map patterns *)
  | PERCENT_LBRACE fields = separated_list(COMMA, map_pattern_field) RBRACE
    { PMap fields }


literal:
  | s = STRING { LString s }
  | i = INT { LInt i }
  | f = FLOAT { LFloat f }
  | b = BOOL { LBool b }
  | a = ATOM { LAtom a }
  | NIL { LNil }

simple_tuple_element:
  | name = IDENT { STEIdent name }
  | atom = ATOM { STEAtom atom }
  | lit = literal { STELiteral lit }

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

record_field_init:
  | field_name = IDENT COLON value = expr
    { (field_name, value) }

record_field_update:
  | field_name = IDENT COLON value = expr
    { (field_name, value) }

record_pattern_field:
  | field_name = IDENT COLON pattern = pattern
    { (field_name, pattern) }
  | field_name = IDENT
    { (field_name, PVar field_name) }  (* Shorthand: {name} = {name: name} *)

(* Unified map field rules for both creation and pattern matching *)
map_field:
  | key = IDENT COLON value = expr
    { AtomKeyField (key, value) }  (* atom key shorthand *)
  | key = expr ARROW_DOUBLE value = expr
    { GeneralKeyField (key, value) }  (* general key syntax *)

map_pattern_field:
  | key = IDENT COLON pattern = pattern
    { AtomKeyPattern (key, pattern) }
  | key = expr ARROW_DOUBLE pattern = pattern
    { GeneralKeyPattern (key, pattern) }

(* Guard expressions *)
guard_expr:
  | guard_and_expr { $1 }

guard_and_expr:
  | guard_andalso_expr { $1 }
  | left = guard_and_expr AND right = guard_andalso_expr { GuardAnd (left, right) }

guard_andalso_expr:
  | guard_or_expr { $1 }
  | left = guard_andalso_expr ANDALSO right = guard_or_expr { GuardAndalso (left, right) }

guard_or_expr:
  | guard_orelse_expr { $1 }
  | left = guard_or_expr OR right = guard_orelse_expr { GuardOr (left, right) }

guard_orelse_expr:
  | guard_primary { $1 }
  | left = guard_orelse_expr ORELSE right = guard_primary { GuardOrelse (left, right) }

guard_primary:
  | NOT guard = guard_primary { GuardNot guard }
  | LPAREN guard = guard_expr RPAREN { guard }
  | left = guard_value op = guard_op right = guard_value { GuardBinOp (left, op, right) }
  | func = IDENT LPAREN args = separated_list(COMMA, guard_value) RPAREN { GuardCall (func, args) }
  | atom = guard_atom { GuardAtom atom }

guard_value:
  | atom = guard_atom { GuardAtomValue atom }
  | func = IDENT LPAREN args = separated_list(COMMA, guard_value) RPAREN { GuardCallValue (func, args) }

guard_atom:
  | name = IDENT { GuardVar name }
  | lit = literal { GuardLiteral lit }
  | MINUS lit = literal {
      match lit with
      | LInt i -> GuardLiteral (LInt (-i))
      | LFloat f -> GuardLiteral (LFloat (-.f))
      | _ -> failwith "Enhanced:Invalid negative literal in guard|Suggestion:Only numbers can be negative in guards|Context:guard expression"
  }

guard_op:
  | EQEQ { "==" }
  | NEQ { "!=" }
  | LT { "<" }
  | GT { ">" }
  | LEQ { "<=" }
  | GEQ { ">=" }
  | PLUS { "+" }
  | MINUS { "-" }
  | MULT { "*" }
  | DIV { "/" }

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

record_def:
  | RECORD name = UPPER_IDENT LBRACE fields = separated_list(COMMA, record_field) RBRACE
    { let pos = make_position $startpos in { record_name = name; fields = fields; position = Some pos } }

record_field:
  | field_name = IDENT CONS field_type = type_expr
    { { field_name; field_type; default_value = None } }
  | field_name = IDENT CONS field_type = type_expr EQ default = expr
    { { field_name; field_type; default_value = Some default } }

type_expr:
  | IDENT { TypeName $1 }
  | type_expr PIPE type_expr { TypeUnion ($1, $3) }
  | LBRACKET type_expr RBRACKET { TypeList $2 }
  | LPAREN types = separated_list(COMMA, type_expr) RPAREN { TypeTuple types }
