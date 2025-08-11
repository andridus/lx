Nonterminals
    program
    top_level_declaration
    function_def
    function_head
    function_body
    expression_list
    record_def
    record_fields
    record_field
    type_def
    type_spec
    expression
    literal
    variable
    function_call
    record_create
    record_field_list
    record_field_assignment
    list_expr
    list_elements
    tuple_expr
    block_expr
    pattern
    pattern_list
    case_expr
    case_clauses
    case_clause
    with_expr
    with_clauses
    with_clause
    match_expr
    guard_expr.

Terminals
    atom
    integer
    float
    string
    var
    'def'
    'defp'
    'end'
    'do'
    'record'
    'type'
    'case'
    'if'
    'else'
    'when'
    'with'
    'match'
    'rescue'
    '::'
    '|'
    '['
    ']'
    '{'
    '}'
    '('
    ')'
    ','
    ';'
    ':'
    '='
    '+'
    '-'
    '*'
    '/'
    '->'
    '<-'
    'and'
    'or'.

Rootsymbol program.

%% Operator precedence
Left 100 '='.
Left 200 '|'.
Left 300 ','.
Left 400 '+'.
Left 400 '-'.
Left 500 '*'.
Left 500 '/'.

%% Expect conflicts
Expect 16.

program -> top_level_declaration : ['$1'].
program -> top_level_declaration program : ['$1' | '$2'].

top_level_declaration -> function_def : '$1'.
top_level_declaration -> record_def : '$1'.
top_level_declaration -> type_def : '$1'.

%% Function definitions
function_def -> 'def' function_head 'do' function_body 'end' :
    {function_def, '$2', '$4', public}.
function_def -> 'defp' function_head 'do' function_body 'end' :
    {function_def, '$2', '$4', private}.

function_head -> var '(' ')' : {function_head, '$1', []}.
function_head -> var '(' pattern_list ')' : {function_head, '$1', '$3'}.

function_body -> expression : '$1'.
function_body -> block_expr : '$1'.

expression_list -> expression : ['$1'].
expression_list -> expression ';' expression_list : ['$1' | '$3'].

%% Block expressions for multiple statements
block_expr -> 'do' expression_list 'end' : {block, '$2'}.

%% Record definitions
record_def -> 'record' var '{' record_fields '}' :
    {record_def, '$2', '$4'}.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '::' type_spec : {record_field, '$1', '$3'}.

%% Type definitions
type_def -> 'type' var '::' type_spec : {type_def, '$2', '$4'}.

type_spec -> var : {type_spec, '$1'}.
type_spec -> atom : {type_spec, '$1'}.
type_spec -> '[' ']' : {type_spec, list}.
type_spec -> '[' type_spec ']' : {type_spec, {list, '$2'}}.
type_spec -> '{' '}' : {type_spec, tuple}.
type_spec -> type_spec '|' type_spec : {type_spec, {union, '$1', '$3'}}.

%% Expressions
expression -> literal : '$1'.
expression -> variable : '$1'.
expression -> function_call : '$1'.
expression -> record_create : '$1'.
expression -> list_expr : '$1'.
expression -> tuple_expr : '$1'.
expression -> case_expr : '$1'.
expression -> with_expr : '$1'.
expression -> match_expr : '$1'.
expression -> expression '+' expression : {binary_op, '+', '$1', '$3'}.
expression -> expression '-' expression : {binary_op, '-', '$1', '$3'}.
expression -> expression '*' expression : {binary_op, '*', '$1', '$3'}.
expression -> expression '/' expression : {binary_op, '/', '$1', '$3'}.
expression -> '(' expression ')' : '$2'.

%% Literals
literal -> integer : {literal, integer, '$1'}.
literal -> float : {literal, float, '$1'}.
literal -> string : {literal, string, '$1'}.
literal -> atom : {literal, atom, '$1'}.

%% Variables
variable -> var : {variable, '$1'}.

%% Function calls
function_call -> var '(' ')' : {function_call, '$1', []}.
function_call -> var '(' expression_list ')' : {function_call, '$1', '$3'}.

%% Record operations
record_create -> var '{' '}' : {record_create, '$1', []}.
record_create -> var '{' record_field_list '}' : {record_create, '$1', '$3'}.

record_field_list -> record_field_assignment : ['$1'].
record_field_list -> record_field_assignment ',' record_field_list : ['$1' | '$3'].

record_field_assignment -> var ':' expression : {record_field_assignment, '$1', '$3'}.

%% Lists
list_expr -> '[' ']' : {list, []}.
list_expr -> '[' list_elements ']' : {list, '$2'}.

list_elements -> expression : ['$1'].
list_elements -> expression ',' list_elements : ['$1' | '$3'].
list_elements -> expression '|' expression : {cons, '$1', '$3'}.

%% Tuples
tuple_expr -> '{' '}' : {tuple, []}.
tuple_expr -> '{' expression_list '}' : {tuple, '$2'}.

%% Patterns
pattern -> literal : '$1'.
pattern -> variable : '$1'.
pattern -> '[' ']' : {list_pattern, []}.
pattern -> '[' pattern_list ']' : {list_pattern, '$2'}.
pattern -> '{' '}' : {tuple_pattern, []}.
pattern -> '{' pattern_list '}' : {tuple_pattern, '$2'}.

pattern_list -> pattern : ['$1'].
pattern_list -> pattern ',' pattern_list : ['$1' | '$3'].

%% Case expressions
case_expr -> 'case' expression 'do' case_clauses 'end' : {case_expr, '$2', '$4'}.

case_clauses -> case_clause : ['$1'].
case_clauses -> case_clause case_clauses : ['$1' | '$2'].

case_clause -> pattern '->' expression : {case_clause, '$1', '$3'}.
case_clause -> pattern 'when' guard_expr '->' expression : {case_clause, '$1', '$3', '$5'}.

%% With expressions
with_expr -> 'with' with_clauses 'do' expression 'end' : {with_expr, '$2', '$4'}.
with_expr -> 'with' with_clauses 'do' expression 'else' case_clauses 'end' : {with_expr, '$2', '$4', '$6'}.

with_clauses -> with_clause : ['$1'].
with_clauses -> with_clause ',' with_clauses : ['$1' | '$3'].

with_clause -> pattern '<-' expression : {with_clause, '$1', '$3'}.

%% Match expressions
match_expr -> 'match' pattern '<-' expression : {match_expr, '$2', '$4'}.
match_expr -> 'match' pattern '<-' expression 'rescue' var 'do' expression 'end' : {match_rescue_expr, '$2', '$4', '$6', '$8'}.

%% Guard expressions
guard_expr -> expression : '$1'.
guard_expr -> guard_expr 'and' guard_expr : {binary_op, 'and', '$1', '$3'}.
guard_expr -> guard_expr 'or' guard_expr : {binary_op, 'or', '$1', '$3'}.

Erlang code.

%% Helper functions for AST construction
build_ast_node(Type, Line, Data) ->
    {Type, Line, Data}.

build_function_def(Name, Args, Body, Visibility) ->
    {function_def, Name, Args, Body, Visibility}.

build_record_def(Name, Fields) ->
    {record_def, Name, Fields}.

build_type_def(Name, Spec) ->
    {type_def, Name, Spec}.