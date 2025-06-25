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
  | BinaryCreate elements ->
    let convert_element = function
      | SimpleBinaryElement expr -> SimpleBinaryPattern (expr_to_pattern expr)
      | SizedBinaryElement (expr, size, spec) -> SizedBinaryPattern (expr_to_pattern expr, size, spec)
      | TypedBinaryElement (expr, spec) -> TypedBinaryPattern (expr_to_pattern expr, spec)
    in
    PBinary (List.map convert_element elements)
  | _ -> failwith "Enhanced:Invalid pattern in assignment|Suggestion:Use valid pattern syntax for destructuring|Context:pattern matching assignment"

%}

(* Tokens *)
%token <string> IDENT UPPER_IDENT STRING ATOM
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token NIL
%token MODULE_MACRO

(* Keywords *)
%token DEF DEFP FN CASE IF ELSE DO END FOR WHEN IN AND OR NOT ANDALSO ORELSE RECEIVE AFTER MATCH_KEYWORD
  %token RECORD UNSAFE WITH RESCUE

(* Module System Keywords *)
%token DEPS PROJECT VERSION APPS GITHUB PATH HEX

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
%token PERCENT_LBRACE PERCENT_LT PERCENT
%token COMMA SEMICOLON CONS COLON DOT
%token CONCAT PLUS MINUS MULT DIV SEND
%token ARROW_DOUBLE MATCH_ASSIGN PATTERN_MATCH
%token LBINARY RBINARY

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
  | deps = deps_declaration? items = module_item* EOF { { deps; items } }

(* For lx.config files *)
config_file:
  | project_declaration EOF { $1 }

project_declaration:
  | PROJECT LBRACE project_fields RBRACE { $3 }

project_fields:
  | fields = project_field* {
      let name = ref None in
      let version = ref None in
      let deps = ref [] in
      let apps = ref [] in
      List.iter (function
        | ProjectName n -> name := Some n
        | ProjectVersion v -> version := Some v
        | ProjectDeps d -> deps := d
        | ProjectApps a -> apps := a
      ) fields;
      { name = !name; version = !version; deps = !deps; apps = !apps }
    }

project_field:
  | name = IDENT s = STRING { if name = "name" then ProjectName s else failwith ("Expected 'name', got '" ^ name ^ "'") }
  | VERSION s = STRING { ProjectVersion s }
  | DEPS LBRACKET deps = separated_list(COMMA, dependency_spec) RBRACKET { ProjectDeps deps }
  | APPS LBRACKET apps = separated_list(COMMA, STRING) RBRACKET { ProjectApps apps }

deps_declaration:
  | DEPS LBRACKET deps = separated_list(COMMA, dependency_spec) RBRACKET { deps }

dependency_spec:
  | atom = atom { Simple atom }
  | LBRACE atom = atom COMMA version = STRING RBRACE { Version (atom, version) }
  | LBRACE atom = atom COMMA COLON GITHUB COMMA repo = STRING RBRACE { GitHub (atom, repo) }
  | LBRACE atom = atom COMMA COLON PATH COMMA path = STRING RBRACE { Path (atom, path) }
  | LBRACE atom = atom COMMA COLON HEX COMMA version = STRING RBRACE { Hex (atom, version) }

atom:
  | COLON name = IDENT { name }
  | name = ATOM { name }

module_item:
  | f = function_def { Function f }
  | c = otp_component { OtpComponent c }
  | s = spec_def { Spec s }
  | t = test_block { Test t }
  | standalone_test = standalone_test_def { Test { name = "Standalone Tests"; tests = [standalone_test] } }
  | a = application_def { Application a }
  | r = record_def { RecordDef r }

function_def:
  (* New syntax: def for public functions *)
  | DEF name = IDENT DO clauses = function_clause+ END
    { let pos = make_position $startpos in { name; clauses; visibility = Public; position = Some pos } }
  | DEF name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO body = function_body END
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | DEF name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN DO body = function_body END
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | DEF name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO END
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }
  | DEF name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN DO END
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Public; position = Some pos } }

  (* New syntax: defp for private functions *)
  | DEFP name = IDENT DO clauses = function_clause+ END
    { let pos = make_position $startpos in { name; clauses; visibility = Private; position = Some pos } }
  | DEFP name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO body = function_body END
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | DEFP name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN DO body = function_body END
    { let pos = make_position $startpos in
      let clause = { params; body; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | DEFP name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO END
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = Some guard } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }
  | DEFP name = IDENT LPAREN params = separated_list(COMMA, pattern) RPAREN DO END
    { let pos = make_position $startpos in
      let clause = { params; body = Literal LNil; position = Some pos; guard = None } in
      { name; clauses = [clause]; visibility = Private; position = Some pos } }



  (* Error handling for new syntax *)
  | DEF _name = IDENT LPAREN _params = separated_list(COMMA, pattern) RPAREN error
    { failwith "Enhanced:Missing function body - expected 'do' after parameter list|Suggestion:Add 'do' and 'end' to define the function body|Context:function definition" }
  | DEF _name = IDENT error
    { failwith "Enhanced:Missing parameter list or clause block - expected '(' or 'do' after function name|Suggestion:Use 'def name() do body end' for single clause functions or 'def name do (params) do body end end' for multiple clause functions|Context:function definition" }
  | DEF error
    { failwith "Enhanced:Missing function name after 'def' keyword|Suggestion:Provide a valid identifier name for the function|Context:function definition" }
  | DEFP _name = IDENT LPAREN _params = separated_list(COMMA, pattern) RPAREN error
    { failwith "Enhanced:Missing function body - expected 'do' after parameter list|Suggestion:Add 'do' and 'end' to define the function body|Context:function definition" }
  | DEFP _name = IDENT error
    { failwith "Enhanced:Missing parameter list or clause block - expected '(' or 'do' after function name|Suggestion:Use 'defp name() do body end' for single clause functions or 'defp name do (params) do body end end' for multiple clause functions|Context:function definition" }
  | DEFP error
    { failwith "Enhanced:Missing function name after 'defp' keyword|Suggestion:Provide a valid identifier name for the function|Context:function definition" }



function_clause:
  | LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO body = function_body END
    { let pos = make_position $startpos in { params; body; position = Some pos; guard = Some guard } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN DO body = function_body END
    { let pos = make_position $startpos in { params; body; position = Some pos; guard = None } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN WHEN guard = guard_expr DO END
    { let pos = make_position $startpos in { params; body = Literal LNil; position = Some pos; guard = Some guard } }
  | LPAREN params = separated_list(COMMA, pattern) RPAREN DO END
    { let pos = make_position $startpos in { params; body = Literal LNil; position = Some pos; guard = None } }
  | error
    { failwith "Enhanced:Invalid function clause syntax - expected parameter list in parentheses|Suggestion:For single clause functions use 'def name() do body end' or 'defp name() do body end', for multiple clause functions use 'def name do (params) do body end end'|Context:function clause definition" }

otp_component:
  | w = worker_def { w }
  | s = supervisor_def { s }

worker_def:
  | WORKER name = IDENT DO functions = function_def* specs = spec_def* END
    { let pos = make_position $startpos in
      Worker { name; functions; specs; position = Some pos } }

supervisor_def:
  | SUPERVISOR name = IDENT DO
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACKET children = separated_list(COMMA, IDENT) RBRACKET
    END
    { let pos = make_position $startpos in
      Supervisor { name = Some name; strategy; children = SimpleChildren children; position = Some pos } }

  | SUPERVISOR name = IDENT DO
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACE children_spec = children_specification RBRACE
    END
    { let pos = make_position $startpos in
      Supervisor { name = Some name; strategy; children = children_spec; position = Some pos } }

  | SUPERVISOR DO
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACKET children = separated_list(COMMA, IDENT) RBRACKET
    END
    { let pos = make_position $startpos in
      Supervisor { name = None; strategy; children = SimpleChildren children; position = Some pos } }

  | SUPERVISOR DO
      STRATEGY strategy = otp_strategy
      CHILDREN LBRACE children_spec = children_specification RBRACE
    END
    { let pos = make_position $startpos in
      Supervisor { name = None; strategy; children = children_spec; position = Some pos } }

  (* Error cases *)
  | SUPERVISOR name = IDENT DO
      STRATEGY strategy = otp_strategy
      CHILDREN _first_child = IDENT error
    { failwith ("Enhanced:Invalid supervisor children field - brackets are required around the children list|Suggestion:Change 'children " ^ _first_child ^ ", ...' to 'children [" ^ _first_child ^ ", ...]'|Context:Supervisor children must always be enclosed in brackets for consistency with list syntax|Example:supervisor " ^ name ^ " do\n  strategy " ^ (match strategy with OneForOne -> "one_for_one" | OneForAll -> "one_for_all" | RestForOne -> "rest_for_one") ^ "\n  children [" ^ _first_child ^ "]\nend") }
  | SUPERVISOR name = IDENT DO
      STRATEGY _strategy = otp_strategy
      CHILDREN error
    { failwith ("Enhanced:Invalid supervisor children field - expected '[' or '{' after 'children' keyword|Suggestion:Use 'children [worker1, worker2]' or 'children { worker [worker1], supervisor [supervisor1] }'|Context:Supervisor children must be enclosed in brackets or braces|Example:supervisor " ^ name ^ " do\n  strategy one_for_one\n  children [worker1, worker2]\nend") }

otp_strategy:
  | ONE_FOR_ONE { OneForOne }
  | ONE_FOR_ALL { OneForAll }
  | REST_FOR_ONE { RestForOne }

spec_def:
  | SPEC name = IDENT DO END
    { { name; requires = []; ensures = [] } }
  | SPEC name = IDENT DO REQUIRES requires = separated_list(COMMA, spec_expr) END
    { { name; requires; ensures = [] } }
  | SPEC name = IDENT DO ENSURES ensures = separated_list(COMMA, spec_expr) END
    { { name; requires = []; ensures } }
  | SPEC name = IDENT DO
      REQUIRES requires = separated_list(COMMA, spec_expr)
      ENSURES ensures = separated_list(COMMA, spec_expr)
    END
    { { name; requires; ensures } }

spec_expr:
  | e = expr { e }

test_block:
  | IDENT DO tests = test_def* END
    { if $1 = "describe" then { name = ""; tests } else failwith "Expected 'describe' keyword" }
  | IDENT name = STRING DO tests = test_def* END
    { if $1 = "describe" then { name; tests } else failwith "Expected 'describe' keyword" }
  | IDENT error
    { if $1 = "describe" then
        failwith "Enhanced:The describe block must be followed by a description string|Suggestion:Use 'describe \"description\" do ... end'|Context:test block definition"
      else failwith "Expected 'describe' keyword" }

test_def:
  | IDENT name = STRING DO body = expr END
    { if $1 = "test" then { name; body } else failwith "Expected 'test' keyword" }
  | IDENT error
    { if $1 = "test" then
        failwith "Enhanced:The test block must be followed by a description string|Suggestion:Use 'test \"description\" do ... end'|Context:test definition"
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
        PatternMatch (pattern, right, Some pos, false)
    }
  | UNSAFE left = expr EQ right = expr
    { let pos = make_position $startpos in
      match left with
      | Var name -> Assign (name, right, Some pos)  (* Simple variable binding *)
      | _ ->
        (* Convert expression to pattern for pattern matching with unsafe flag *)
        let pattern = expr_to_pattern left in
        PatternMatch (pattern, right, Some pos, true)
    }
  (* Pattern matching operator using <- (always pattern matching, never binding) *)
  | left = expr PATTERN_MATCH right = expr
    { let pos = make_position $startpos in
      let pattern = expr_to_pattern left in
      PatternMatch (pattern, right, Some pos, false)
    }
  | UNSAFE left = expr PATTERN_MATCH right = expr
    { let pos = make_position $startpos in
      let pattern = expr_to_pattern left in
      PatternMatch (pattern, right, Some pos, true)
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
  | IF cond = expr DO then_statements = statement_list ELSE else_statements = statement_list END
    { let then_expr = match then_statements with
        | [e] -> e
        | es -> Block es
      in
      let else_expr = match else_statements with
        | [e] -> e
        | es -> Block es
      in
      If (cond, then_expr, Some (SimpleElse else_expr)) }
  | IF cond = expr DO then_statements = statement_list CASE case_clauses = nonempty_list(case_branch) END
    { let then_expr = match then_statements with
        | [e] -> e
        | es -> Block es
      in
      If (cond, then_expr, Some (ClauseElse case_clauses)) }


  | IF cond = expr DO then_statements = statement_list END
    { let then_expr = match then_statements with
        | [e] -> e
        | es -> Block es
      in
      If (cond, then_expr, None) }
  | IF _cond = expr DO error
    { failwith "Enhanced:Missing expression in if block|Suggestion:Add an expression after 'do'|Context:if statement" }
  | IF _cond = expr error
    { failwith "Enhanced:Missing 'do' keyword in if statement|Suggestion:Add 'do' after the condition|Context:if statement" }
  | IF error
    { failwith "Enhanced:Missing condition after 'if' keyword|Suggestion:Add a boolean expression after 'if'|Context:if statement" }
  | CASE value = expr DO cases = nonempty_list(case_branch) END
    { Match (value, cases) }
  | MATCH_KEYWORD steps = match_rescue_steps DO success_body = expr END
    { MatchRescue (steps, success_body) }
  | MATCH_KEYWORD pattern = pattern PATTERN_MATCH expr = expr RESCUE rescue_expr = expr END
    { MatchRescueStep (pattern, expr, rescue_expr) }
  | WITH steps = with_steps DO success_body = expr END
    { With (steps, success_body, None) }
  | WITH steps = with_steps DO success_body = expr ELSE else_statements = statement_list END
    { let else_expr = match else_statements with
        | [e] -> e
        | es -> Block es
      in
      With (steps, success_body, Some (SimpleElse else_expr)) }
  | WITH steps = with_steps DO success_body = expr CASE case_clauses = nonempty_list(case_branch) END
    { With (steps, success_body, Some (ClauseElse case_clauses)) }
  | FOR pattern = pattern EQ var = IDENT IN iterable = expr WHEN guard = guard_expr DO body = expr END
    { For (pattern, Some var, iterable, body, Some guard) }
  | FOR pattern = pattern EQ var = IDENT IN iterable = expr DO body = expr END
    { For (pattern, Some var, iterable, body, None) }
  | FOR pattern = pattern IN iterable = expr WHEN guard = guard_expr DO body = expr END
    { For (pattern, None, iterable, body, Some guard) }
  | FOR pattern = pattern IN iterable = expr DO body = expr END
    { For (pattern, None, iterable, body, None) }
  | RECEIVE DO clauses = nonempty_list(receive_clause) END
    { Receive (clauses, None) }
  | RECEIVE DO clauses = nonempty_list(receive_clause) END AFTER timeout = expr DO timeout_body = expr END
    { Receive (clauses, Some (timeout, timeout_body)) }
  (* Record expressions *)
  | record_name = UPPER_IDENT LBRACE fields = separated_list(COMMA, record_field_init) RBRACE
    { RecordCreate (record_name, fields) }
  (* Map expressions *)
  | PERCENT_LBRACE fields = separated_list(COMMA, map_field) RBRACE
    { MapCreate fields }
  (* Binary expressions *)
  | LBINARY elements = separated_list(COMMA, binary_element) RBINARY
    { BinaryCreate elements }
  | LBRACE expr = expr PIPE updates = separated_list(COMMA, record_field_update) RBRACE
    { RecordUpdate (expr, updates) }

simple_expr:
  | l = literal { Literal l }
  | name = IDENT { Var name }
  | RECORD { Var "record" }
  | MODULE_MACRO { Var "?MODULE" }
  (* External function call: module.function(args) *)
  | module_name = IDENT DOT func_name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    { let pos = make_position $startpos in ExternalCall (module_name, func_name, args, Some pos) }
  (* Record field access *)
  | record_var = IDENT DOT field_name = IDENT
    { RecordAccess (Var record_var, field_name) }
  | RECORD DOT field_name = IDENT
    { RecordAccess (Var "record", field_name) }
  (* Anonymous function expressions *)
  | FN LPAREN params = separated_list(COMMA, IDENT) RPAREN DO body = expr END
    { FunExpression (params, body) }
  | FN DO clauses = fun_expression_clause+ END
    { FunExpressionClauses clauses }
  | LPAREN e = expr RPAREN { e }
  | LPAREN elements = separated_list(COMMA, expr) RPAREN
    { match elements with
      | [] -> Tuple []
      | [e] -> e  (* Single element in parentheses is just grouping *)
      | es -> Tuple es }
  (* Tuple syntax with {} *)
  | LBRACE elements = separated_list(COMMA, expr) RBRACE
    { Tuple elements }
  | LBRACKET elements = separated_list(COMMA, expr) RBRACKET
    { List elements }
  (* Block expressions with do/end syntax *)
  | DO statements = statement_list END
    { match statements with
      | [e] -> e  (* Single expression block returns the expression directly *)
      | es -> Block es  (* Multiple expressions become a Block *)
    }
  | LBRACE elements = separated_list(COMMA, expr) RBRACE
    { Tuple elements }
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

fun_expression_clause:
  | LPAREN params = separated_list(COMMA, IDENT) RPAREN WHEN guard = guard_expr DO body = expr END
    { (params, Some guard, body) }
  | LPAREN params = separated_list(COMMA, IDENT) RPAREN DO body = expr END
    { (params, None, body) }





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
  | LBRACE patterns = separated_list(COMMA, pattern) RBRACE
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
  (* Binary patterns *)
  | LBINARY elements = separated_list(COMMA, binary_pattern_element) RBINARY
    { PBinary elements }


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
  | IDENT name = STRING DO body = expr END
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
  | APPLICATION DO fields = application_field* END
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
  | APPLICATION DO fields = application_field* END
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
  | record_var = IDENT DOT field_name = IDENT { GuardRecordAccess (record_var, field_name) }
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

(* Binary element rules for construction *)
binary_element:
  | expr = simple_expr
    { SimpleBinaryElement expr }
  | expr = simple_expr COLON size = simple_expr
    { SizedBinaryElement (expr, size, None) }
  | expr = simple_expr COLON size = simple_expr DIV spec = binary_spec
    { SizedBinaryElement (expr, size, Some spec) }
  | expr = simple_expr DIV spec = binary_spec
    { TypedBinaryElement (expr, spec) }

(* Binary pattern element rules for pattern matching *)
binary_pattern_element:
  | pattern = simple_pattern
    { SimpleBinaryPattern pattern }
  | pattern = simple_pattern COLON size = simple_expr
    { SizedBinaryPattern (pattern, size, None) }
  | pattern = simple_pattern COLON size = simple_expr DIV spec = binary_spec
    { SizedBinaryPattern (pattern, size, Some spec) }
  | pattern = simple_pattern DIV spec = binary_spec
    { TypedBinaryPattern (pattern, spec) }

with_steps:
  | step = with_step
    { [step] }
  | step = with_step COMMA steps = with_steps
    { step :: steps }

with_step:
  | pattern = pattern LEQ expr = expr
    { (pattern, expr) }
  | pattern = pattern PATTERN_MATCH expr = expr
    { (pattern, expr) }

match_rescue_steps:
  | step = match_rescue_step
    { [step] }
  | step = match_rescue_step COMMA steps = match_rescue_steps
    { step :: steps }

match_rescue_step:
  | pattern = pattern PATTERN_MATCH expr = expr RESCUE rescue_expr = expr
    { (pattern, expr, rescue_expr) }

(* Binary specifications *)
binary_spec:
  | IDENT { BinaryType $1 }
  | IDENT MINUS IDENT { BinaryTypeWithEndian ($1, $3) }
