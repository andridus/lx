%% LX2 Common Definitions - Elixir-style AST with complete meta
-ifndef(LX2_HRL).
-define(LX2_HRL, true).

%% Basic types
-type line() :: integer().
-type column() :: integer().
-type position() :: {line(), column()}.

%% Metadata structure (Elixir-style with type)
-type metadata() :: #{line := line(), column := column(), type := term() | undefined}.

%% AST types - Everything is macro-based except primitives
-type ast_type() ::
    % Primitives (only these are built-in)
    defmacro | '__block__' |
    % Erlang literals
    integer | float | string | atom | boolean | nil | underscore |
    % Everything else is macro-defined
    def | record | module | do | 'end' | '->' | bind | '+' | '-' | '*' | '/' |
    '==' | '!=' | '<' | '>' | '<=' | '>=' | 'and' | 'or' | '&&&' | '|||' |
    '^^^' | '<<<' | '>>>' | '++' | 'in' | 'length' | 'not' | '|' | '[' | ']' |
    '{' | '}' | '(' | ')' | ':' | ';' | ',' | '.' | '%' | '$' |
    % Macro system
    macro_call | macro_def | macro_expand | ident | tuple.

%% AST node structure (Elixir-style)
-type ast_node() :: {ast_type(), metadata(), ast_arguments()}.

%% AST arguments can be various types
-type ast_arguments() :: term() | [ast_node()] | ast_node().

%% Token types (minimal - only what's needed for parsing)
-type token_type() ::
    % Primitives
    defmacro | do | 'end' | '->' |
    % Erlang literals
    integer | float | string | atom | boolean | nil | underscore |
    % Identifiers
    ident |
    % Basic syntax
    '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | ',' | bind | '->' |
    % Type system
    '::' | type_identifier |
    % Whitespace
    newline | whitespace |
    % Comments
    comment.

%% Token structure with column
-type token() :: {token_type(), line(), column(), value()}.
-type value() :: term().

%% Macro definition structure
-type macro_def() :: {macro_name(), macro_arity(), macro_body()}.
-type macro_name() :: atom().
-type macro_arity() :: integer().
-type macro_body() :: ast_node().

%% Macro environment
-type macro_env() :: #{macro_name() => [macro_def()]}.

%% Type system types
-type type() ::
    {type_basic, term()} |
    {type_fun, type(), type()} |
    {type_list, type()} |
    {type_tuple, [type()]} |
    {type_var, atom()}.

%% Error types
-type compilation_error() ::
    {lexical_error, line(), message()} |
    {syntax_error, line(), message()} |
    {macro_error, line(), message()} |
    {expansion_error, line(), message()} |
    {type_error, line(), type(), type()} |
    {semantic_error, line(), message()} |
    {codegen_error, message()}.

-type message() :: string().

%% Common macros
-define(DEBUG(Format, Args), io:format("[DEBUG] " ++ Format ++ "~n", Args)).
-define(ERROR(Format, Args), io:format("[ERROR] " ++ Format ++ "~n", Args)).
-define(INFO(Format, Args), io:format("[INFO] " ++ Format ++ "~n", Args)).

-endif.