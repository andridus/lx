Nonterminals
program statement expression literal atom_expr ident_expr
macro_def macro_body macro_args macro_call block_expr
arrow_expr pattern_expr expression_list pattern_list
binding_expr ident_with_type macro_args_with_types
type_expr type_basic type_fun type_list type_tuple type_var
type_expr_list type_identifier.

Terminals
% Primitivos básicos
defmacro do end '->' integer float string atom boolean nil
ident underscore bind semicolon
'(' ')' '[' ']' '{' '}' comma colon
'::' type_identifier.

Rootsymbol program.

% Program consists of statements
program -> statement : ['$1'].
program -> statement semicolon program : ['$1' | '$3'].
program -> statement program : ['$1' | '$2'].

% Statements
statement -> macro_def : '$1'.
statement -> expression : '$1'.
statement -> binding_expr : '$1'.

% Macro definitions (the only primitive)
macro_def -> defmacro ident '(' ')' do macro_body end :
    {defmacro, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [
        extract_ident('$2'),
        {__block__, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, []},
        '$6'
    ]}.

macro_def -> defmacro ident '(' macro_args ')' do macro_body end :
    {defmacro, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [
        extract_ident('$2'),
        '$4',
        '$7'
    ]}.

% Macro definitions with type annotations
macro_def -> defmacro ident '(' macro_args_with_types ')' do macro_body end :
    {defmacro, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [
        extract_ident('$2'),
        '$4',
        '$7'
    ]}.

% Macro arguments
macro_args -> ident : ['$1'].
macro_args -> ident comma macro_args : ['$1' | '$3'].

% Macro arguments with type annotations
macro_args_with_types -> ident_with_type : ['$1'].
macro_args_with_types -> ident_with_type comma macro_args_with_types : ['$1' | '$3'].

% Identifier with type annotation
ident_with_type -> ident : {ident, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_ident('$1')}.
ident_with_type -> ident '::' type_expr :
    {typed_ident, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [
        extract_ident('$1'),
        '$3'
    ]}.

% Type expressions
type_expr -> type_basic : '$1'.
type_expr -> type_fun : '$1'.
type_expr -> type_list : '$1'.
type_expr -> type_tuple : '$1'.
type_expr -> type_var : '$1'.

% Basic types
type_basic -> type_identifier : {type_basic, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_type('$1')}.

% Function types
type_fun -> '{' 'fun' ',' '[' type_expr_list ']' ',' type_expr '}' :
    {type_fun, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, ['$5', '$8']}.

% List types
type_list -> '{' 'list' ',' type_expr '}' :
    {type_list, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, '$4'}.
type_list -> '[' type_expr_list ']' :
    {type_list, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, '$2'}.

% Tuple types
type_tuple -> '{' 'tuple' ',' '[' type_expr_list ']' '}' :
    {type_tuple, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, '$5'}.

% Type variable
type_var -> type_identifier :
    {type_var, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_type_var('$1')}.

% Type expression lists
type_expr_list -> type_expr : ['$1'].
type_expr_list -> type_expr comma type_expr_list : ['$1' | '$3'].

% Macro body
macro_body -> expression : ['$1'].
macro_body -> expression semicolon macro_body : ['$1' | '$3'].

% Expressions
expression -> literal : '$1'.
expression -> atom_expr : '$1'.
expression -> ident_expr : '$1'.
expression -> macro_call : '$1'.
expression -> block_expr : '$1'.
expression -> arrow_expr : '$1'.

% Literals (básicos do Erlang)
literal -> integer : {integer, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_integer('$1')}.
literal -> float : {float, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_float('$1')}.
literal -> string : {string, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_string('$1')}.
literal -> boolean : {boolean, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_boolean('$1')}.
literal -> nil : {nil, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, nil}.
literal -> underscore : {underscore, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, undefined}.

% Atom expressions
atom_expr -> atom : {atom, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_atom('$1')}.

% Identifier expressions
ident_expr -> ident : {ident, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_ident('$1')}.

% Macro calls
macro_call -> ident '(' ')' :
    {macro_call, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [extract_ident('$1'), []]}.
macro_call -> ident '(' expression_list ')' :
    {macro_call, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [extract_ident('$1'), '$3']}.

% Expression lists
expression_list -> expression : ['$1'].
expression_list -> expression comma expression_list : ['$1' | '$3'].

% Block expressions
block_expr -> do expression_list end :
    {__block__, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, '$2'}.

% Arrow expressions (for pattern matching)
arrow_expr -> pattern_expr '->' expression :
    {'->', #{line => get_line('$2'), column => get_column('$2'), type => undefined}, ['$1', '$3']}.

% Pattern expressions
pattern_expr -> ident : {ident, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, extract_ident('$1')}.
pattern_expr -> '(' pattern_list ')' :
    {tuple, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, '$2'}.

% Pattern lists
pattern_list -> ident : ['$1'].
pattern_list -> ident comma pattern_list : ['$1' | '$3'].

% Binding expressions (variable assignment)
binding_expr -> ident bind expression :
    {bind, #{line => get_line('$2'), column => get_column('$2'), type => undefined}, [extract_ident('$1'), '$3']}.

Erlang code.

% Funções auxiliares para extrair linha e coluna
get_line({_, Line, _, _}) -> Line;
get_line({_, Line, _}) -> Line;
get_line(_) -> 1.

get_column({_, _, Col, _}) -> Col;
get_column({_, _, Col}) -> Col;
get_column(_) -> 1.

% Funções auxiliares para extrair valores
extract_ident({ident, _, _, Value}) when is_list(Value) -> list_to_atom(Value);
extract_ident({ident, _, _, Value}) -> Value.
extract_integer({integer, _, _, Value}) -> Value.
extract_float({float, _, _, Value}) -> Value.
extract_string({string, _, _, Value}) -> Value.
extract_atom({atom, _, _, Value}) -> Value.
extract_boolean({boolean, _, _, Value}) -> Value.
extract_type({type_identifier, _, _, Value}) when is_list(Value) -> list_to_atom(Value);
extract_type({type_identifier, _, _, Value}) -> Value.
extract_type_var({type_identifier, _, _, Value}) when is_list(Value) -> list_to_atom(Value);
extract_type_var({type_identifier, _, _, Value}) -> Value.