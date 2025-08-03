%% LX2 Common Definitions
%% This file contains common type definitions and macros for the LX2 compiler

-ifndef(LX2_HRL).
-define(LX2_HRL, true).

%% Basic types
-type line() :: integer().
-type column() :: integer().
-type position() :: {line(), column()}.

%% Token types
-type token_type() ::
    integer | float | string | atom | boolean | nil |
    identifier | newline |
    'def' | 'do' | 'end' | 'record' | 'module' |
    '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>=' |
    'and' | 'or' | '&&&' | '|||' | '^^^' | '<<<' | '>>>' |
    '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | ',' | '=' | '|' |
    'lbracket' | 'rbracket' | 'lbrace' | 'rbrace' | 'lparen' | 'rparen' |
    'comma' | 'semicolon' | 'equals' | 'pipe' | 'percent' | 'dollar' |
    'type_annotation' | 'dot'.

%% Token structure
-type token() :: {token_type(), line(), value()}.
-type value() :: term().

%% AST node types
-type ast_node() ::
    {function_def, lx_identifier(), [parameter()], block()} |
    {binary_op, operator(), ast_node(), ast_node()} |
    {variable_binding, lx_identifier(), ast_node()} |
    {variable_ref, lx_identifier()} |
    {literal, literal_type(), value()} |
    {list_literal, [ast_node()]} |
    {list_cons, ast_node(), ast_node()} |
    {tuple_literal, [ast_node()]} |
    {map_literal, [map_entry()]} |
    {map_entry, ast_node(), ast_node()} |
    {map_access, ast_node(), ast_node()} |
    {record_def, lx_identifier(), [record_field()]} |
    {record_field, lx_identifier(), type(), value()} |
    {record_literal, lx_identifier(), [field_assignment()]} |
    {field_assignment, lx_identifier(), ast_node()} |
    {record_access, ast_node(), lx_identifier()} |
    {record_update, ast_node(), [field_assignment()]} |
    {directive_call, directive_type(), ast_node()} |
    {function_call, lx_identifier(), [ast_node()]}.

%% Supporting types
-type lx_identifier() :: atom().
-type parameter() :: lx_identifier().
-type operator() :: atom().
-type literal_type() :: integer | float | string | atom | boolean | nil.
-type directive_type() :: atom().
-type map_entry() :: {ast_node(), ast_node()}.
-type record_field() :: {identifier(), type(), value()}.
-type field_assignment() :: {identifier(), ast_node()}.
-type block() :: [ast_node()].
-type type() :: term(). % Will be defined in lx2_types.erl

%% Error types
-type compilation_error() ::
    {lexical_error, line(), message()} |
    {syntax_error, line(), message()} |
    {type_error, line(), type(), type()} |
    {semantic_error, line(), message()} |
    {codegen_error, message()}.

-type message() :: string().

%% Common macros
-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-define(ERROR(Format, Args), io:format("[ERROR] " ++ Format ++ "~n", Args)).
-define(INFO(Format, Args), io:format("[INFO] " ++ Format ++ "~n", Args)).

-endif.