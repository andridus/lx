Nonterminals
program statement expression literal atom_expr ident_expr
macro_def macro_body macro_args macro_call block_expr
binding_expr block_expressions tuple_expr list_expr typed_ident tuple_elements list_elements macro_call_args.

Terminals
% Primitivos básicos
defmacro infix do end '->' integer float string atom boolean nil
ident underscore bind semicolon type_identifier
'(' ')' '[' ']' '{' '}' ',' '::'
'+' '-' '*' '/' '++'.

Rootsymbol program.

% Precedência
Left 300 ','.
Left 200 '+'.
Left 200 '-'.
Left 300 '*'.
Left 300 '/'.
Left 200 '++'.

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
    {defmacro, {get_line('$1'), get_column('$1'), undefined}, [
        extract_ident('$2'),
        {'__block__', {get_line('$1'), get_column('$1'), undefined}, []},
        '$6'
    ]}.

macro_def -> defmacro ident '(' macro_args ')' do macro_body end :
    {defmacro, {get_line('$1'), get_column('$1'), undefined}, [
        extract_ident('$2'),
        '$4',
        '$7'
    ]}.

% Infix macro definitions
macro_def -> defmacro infix '+' '(' macro_args ')' do macro_body end :
    {defmacro_infix, {get_line('$1'), get_column('$1'), undefined}, [
        '+',
        '$4',
        '$7'
    ]}.

macro_def -> defmacro infix '-' '(' macro_args ')' do macro_body end :
    {defmacro_infix, {get_line('$1'), get_column('$1'), undefined}, [
        '-',
        '$4',
        '$7'
    ]}.

macro_def -> defmacro infix '*' '(' macro_args ')' do macro_body end :
    {defmacro_infix, {get_line('$1'), get_column('$1'), undefined}, [
        '*',
        '$4',
        '$7'
    ]}.

macro_def -> defmacro infix '/' '(' macro_args ')' do macro_body end :
    {defmacro_infix, {get_line('$1'), get_column('$1'), undefined}, [
        '/',
        '$4',
        '$7'
    ]}.

macro_def -> defmacro infix '++' '(' macro_args ')' do macro_body end :
    {defmacro_infix, {get_line('$1'), get_column('$1'), undefined}, [
        '++',
        '$4',
        '$7'
    ]}.

% Macro arguments
macro_args -> ident : ['$1'].
macro_args -> ident ',' macro_args : ['$1' | '$3'].
macro_args -> typed_ident : ['$1'].
macro_args -> typed_ident ',' macro_args : ['$1' | '$3'].

% Typed identifier
typed_ident -> ident '::' type_identifier :
    {typed_ident, {get_line('$1'), get_column('$1'), undefined}, [extract_ident('$1'), '$3']}.

% Macro body
macro_body -> expression : ['$1'].
macro_body -> expression semicolon macro_body : ['$1' | '$3'].

% Expressions
expression -> literal : '$1'.
expression -> atom_expr : '$1'.
expression -> ident_expr : '$1'.
expression -> macro_call : '$1'.
expression -> block_expr : '$1'.
expression -> tuple_expr : '$1'.
expression -> list_expr : '$1'.
expression -> expression '+' expression :
    {macro_call, {get_line('$2'), get_column('$2'), undefined}, ['+', ['$1', '$3']]}.
expression -> expression '-' expression :
    {macro_call, {get_line('$2'), get_column('$2'), undefined}, ['-', ['$1', '$3']]}.
expression -> expression '*' expression :
    {macro_call, {get_line('$2'), get_column('$2'), undefined}, ['*', ['$1', '$3']]}.
expression -> expression '/' expression :
    {macro_call, {get_line('$2'), get_column('$2'), undefined}, ['/', ['$1', '$3']]}.
expression -> expression '++' expression :
    {macro_call, {get_line('$2'), get_column('$2'), undefined}, ['++', ['$1', '$3']]}.

% Literals (básicos do Erlang)
literal -> integer : {integer, {get_line('$1'), get_column('$1'), undefined}, extract_integer('$1')}.
literal -> float : {float, {get_line('$1'), get_column('$1'), undefined}, extract_float('$1')}.
literal -> string : {string, {get_line('$1'), get_column('$1'), undefined}, extract_string('$1')}.
literal -> boolean : {boolean, {get_line('$1'), get_column('$1'), undefined}, extract_boolean('$1')}.
literal -> nil : {nil, {get_line('$1'), get_column('$1'), undefined}, nil}.
literal -> underscore : {underscore, {get_line('$1'), get_column('$1'), undefined}, undefined}.

% Meta record literal (for infix macros)
literal -> '{' integer ',' integer ',' atom ',' '[' list_elements ']' '}' :
    {meta, {get_line('$1'), get_column('$1'), undefined}, {extract_integer('$2'), extract_integer('$4'), extract_atom('$6'), '$8'}}.

% Atom expressions
atom_expr -> atom : {atom, {get_line('$1'), get_column('$1'), undefined}, extract_atom('$1')}.

% Identifier expressions
ident_expr -> ident : {ident, {get_line('$1'), get_column('$1'), undefined}, extract_ident('$1')}.

% Tuple expressions
tuple_expr -> '{' '}' :
    {tuple, {get_line('$1'), get_column('$1'), undefined}, []}.
tuple_expr -> '{' tuple_elements '}' :
    {tuple, {get_line('$1'), get_column('$1'), undefined}, '$2'}.

% Elementos da tupla (recursão à direita)
tuple_elements -> expression : ['$1'].
tuple_elements -> expression ',' tuple_elements : ['$1' | '$3'].

% List expressions
list_expr -> '[' ']' :
    {list, {get_line('$1'), get_column('$1'), undefined}, []}.
list_expr -> '[' list_elements ']' :
    {list, {get_line('$1'), get_column('$1'), undefined}, '$2'}.

% Elementos da lista (recursão à direita)
list_elements -> expression : ['$1'].
list_elements -> expression ',' list_elements : ['$1' | '$3'].

% Macro calls
macro_call -> ident '(' ')' :
    {macro_call, {get_line('$1'), get_column('$1'), undefined}, [extract_ident('$1'), []]}.
macro_call -> ident '(' macro_call_args ')' :
    {macro_call, {get_line('$1'), get_column('$1'), undefined}, [extract_ident('$1'), '$3']}.

% Argumentos de macro call (sequência de expressions separadas por vírgula)
macro_call_args -> expression : ['$1'].
macro_call_args -> expression ',' macro_call_args : ['$1' | '$3'].

% Block expressions
block_expr -> do block_expressions end :
    {'__block__', {get_line('$1'), get_column('$1'), undefined}, '$2'}.

block_expressions -> expression : ['$1'].
block_expressions -> expression semicolon block_expressions : ['$1' | '$3'].

% Binding expressions (variable assignment)
binding_expr -> ident bind expression :
    {bind, {get_line('$2'), get_column('$2'), undefined}, [extract_ident('$1'), '$3']}.

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