%% lx Common Definitions
-ifndef(lx_HRL).
-define(lx_HRL, true).

%% Basic types
-type line() :: integer().
-type column() :: integer().
-type position() :: {line(), column()}.

%% Metadata structure
-type metadata() :: #{line := line(), column := column(), type := term() | undefined}.

%% Metadata record with opts (for infix macros)
-record(meta, {line :: integer(), column :: integer(), type :: term() | undefined, opts :: [term()]}).

%% AST node structure
-type ast_node() :: {ast_type(), metadata(), ast_arguments()}.

%% AST types - Everything is macro-based except primitives
-type ast_type() ::
    % Primitives (only these are built-in)
    defmacro | __block__ |
    % Erlang literals
    integer | float | string | atom | boolean | nil | underscore |
    % Macro system
    macro_call | macro_def | macro_expand | ident | tuple |
    % Type system
    type_basic | type_fun | type_list | type_tuple | type_var | typed_ident.

%% AST arguments can be various types
-type ast_arguments() :: term() | [ast_node()] | ast_node().

%% Token types (minimal - only what's needed for parsing)
-type token_type() ::
    % Primitives
    defmacro | do | 'end' | '->' |
    % Erlang literals
    integer | float | string | atom | boolean | nil | underscore |
    % Identifiers
    ident | type_identifier |
    % Basic syntax
    '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | ',' | bind | '->' | '::' |
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

%% Type system
-type type() ::
    integer | float | string | boolean | atom | nil | any |
    {'fun', [type()], type()} |
    {list, type()} |
    {tuple, [type()]} |
    {type_var, atom()}.

%% Type environment with macro definitions
-type type_env() :: #{
    variables => #{atom() => type()},
    macros => #{atom() => [macro_signature()]}
}.

-type macro_signature() :: {
    macro_name(),
    [typed_argument()],
    type()
}.

-type typed_argument() :: {
    argument_name(),
    type()
}.

-type argument_name() :: atom().

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