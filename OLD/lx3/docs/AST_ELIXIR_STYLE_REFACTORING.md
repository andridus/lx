# Refatoração AST Elixir-Style com Meta Completo e Sistema de Macros

## Visão Geral

Este documento detalha a refatoração completa do lx para adotar um modelo de AST no estilo Elixir, onde cada nó possui metadados completos (linha, coluna e tipo) e toda a sintaxe é definida via macros, exceto `defmacro` que é a única construção primitiva.

## Objetivos

1. **AST Elixir-Style**: Estrutura `{tipo, meta, args}` para todos os nós
2. **Meta Completo**: Cada nó contém linha, coluna e tipo no campo `meta`
3. **Sistema de Macros**: Tudo é definido via macros, exceto `defmacro`
4. **Tokens Simplificados**: `ident`, `bind`, `semicolon` e literais básicos do Erlang
5. **Posições Precisas**: Linha e coluna corretas para debugging e mensagens de erro

## Arquitetura da Nova AST

### Estrutura do Nó
```erlang
{Type, Meta, Arguments}
```

Onde:
- **Type**: Átomo identificando o tipo do nó
- **Meta**: Mapa com `#{line => integer(), column => integer(), type => term() | undefined}`
- **Arguments**: Lista de argumentos ou filhos do nó

### Exemplo de Meta Completo
```erlang
#{line => 3, column => 7, type => integer}
#{line => 5, column => 2, type => {list, integer}}
#{line => 1, column => 1, type => undefined}  % No parser
```

## 1. Atualização do Lexer

### Arquivo: `leex/lx_lexer.xrl`

```erlang
Definitions.

D = [0-9]
L = [a-zA-Z_]
WS = [\s\t]
NL = \n|\r\n|\r

Rules.

% Literais básicos do Erlang
{D}+ : {token, {integer, TokenLine, TokenCol, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, TokenCol, list_to_float(TokenChars)}}.
\"[^\"]*\" : {token, {string, TokenLine, TokenCol, strip_quotes(TokenChars)}}.
:({L}({L}|{D})*) : {token, {atom, TokenLine, TokenCol, strip_colon(TokenChars)}}.
true : {token, {boolean, TokenLine, TokenCol, true}}.
false : {token, {boolean, TokenLine, TokenCol, false}}.
nil : {token, {nil, TokenLine, TokenCol, nil}}.

% Keywords básicas (apenas primitivas)
defmacro : {token, {defmacro, TokenLine, TokenCol}}.
do : {token, {do, TokenLine, TokenCol}}.
end : {token, {'end', TokenLine, TokenCol}}.
'->' : {token, {'->', TokenLine, TokenCol}}.

% Identificadores e underscore
_ : {token, {underscore, TokenLine, TokenCol}}.
{L}({L}|{D})* : {token, {ident, TokenLine, TokenCol, TokenChars}}.

% Operadores e pontuação
= : {token, {bind, TokenLine, TokenCol}}.
; : {token, {semicolon, TokenLine, TokenCol}}.
\( : {token, {'(', TokenLine, TokenCol}}.
\) : {token, {')', TokenLine, TokenCol}}.
\[ : {token, {'[', TokenLine, TokenCol}}.
\] : {token, {']', TokenLine, TokenCol}}.
\{ : {token, {'{', TokenLine, TokenCol}}.
\} : {token, {'}', TokenLine, TokenCol}}.
, : {token, {comma, TokenLine, TokenCol}}.
: : {token, {colon, TokenLine, TokenCol}}.

% Whitespace e comentários
{WS}+ : skip_token.
{NL} : skip_token.
#.* : skip_token.

Erlang code.

strip_quotes([$" | Rest]) ->
    case lists:reverse(Rest) of
        [$" | Content] -> lists:reverse(Content);
        _ -> Rest
    end.

strip_colon([$: | Rest]) ->
    list_to_atom(Rest).
```

### Notas sobre Coluna
Se o leex não fornecer automaticamente a coluna, implemente um contador manual:

```erlang
% No início do arquivo, adicione:
-record(lexer_state, {line = 1, column = 1}).

% E atualize as regras para usar:
{token, {Type, Line, Col, Value}, #lexer_state{line = Line, column = Col + length(TokenChars)}}
```

## 2. Atualização do Parser

### Arquivo: `yecc/lx_parser.yrl`

```erlang
Nonterminals
program statement expression literal atom_expr ident_expr
macro_def macro_body macro_args macro_call block_expr
arrow_expr pattern_expr expression_list pattern_list
binding_expr.

Terminals
% Primitivos básicos
defmacro do end '->' integer float string atom boolean nil
ident underscore bind semicolon
'(' ')' '[' ']' '{' '}' comma colon.

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

% Macro arguments
macro_args -> ident : ['$1'].
macro_args -> ident comma macro_args : ['$1' | '$3'].

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
```

## 3. Atualização do Header de Tipos

### Arquivo: `include/lx.hrl`

```erlang
%% lx Common Definitions - Elixir-style AST with complete meta
-ifndef(lx_HRL).
-define(lx_HRL, true).

%% Basic types
-type line() :: integer().
-type column() :: integer().
-type position() :: {line(), column()}.

%% Metadata structure (Elixir-style with type)
-type metadata() :: #{line := line(), column := column(), type := term() | undefined}.

%% AST node structure (Elixir-style)
-type ast_node() :: {ast_type(), metadata(), ast_arguments()}.

%% AST types - Everything is macro-based except primitives
-type ast_type() ::
    % Primitives (only these are built-in)
    defmacro | __block__ |
    % Erlang literals
    integer | float | string | atom | boolean | nil | underscore |
    % Everything else is macro-defined
    def | record | module | do | end | '->' | bind | '+' | '-' | '*' | '/' |
    '==' | '!=' | '<' | '>' | '<=' | '>=' | 'and' | 'or' | '&&&' | '|||' |
    '^^^' | '<<<' | '>>>' | '++' | 'in' | 'length' | 'not' | '|' | '[' | ']' |
    '{' | '}' | '(' | ')' | ':' | ';' | ',' | '.' | '%' | '$' |
    % Macro system
    macro_call | macro_def | macro_expand | ident | tuple.

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
```

## 4. Sistema de Tipos com Macros e Hindley-Milner

### Especificação de Tipos em Macros

#### Sintaxe para Tipos em Argumentos
```lx
% Sintaxe para especificar tipos em argumentos de macro
defmacro +(left :: number, right :: number) do
  {binary_op, [], [+, left, right]}
end

defmacro not(expr :: boolean) do
  {unary_op, [], [not, expr]}
end

defmacro length(list :: list) do
  {list_length, [], [list]}
end

% Tipos compostos
defmacro map(fun :: {fun, [any], any}, list :: list) do
  {list_map, [], [fun, list]}
end

% Tipos com variáveis
defmacro filter(fun :: {fun, [T], boolean}, list :: {list, T}) do
  {list_filter, [], [fun, list]}
end
```

#### Estrutura AST para Tipos
```erlang
% Tipos básicos
{type_basic, #{line => L, column => C, type => undefined}, integer}
{type_basic, #{line => L, column => C, type => undefined}, float}
{type_basic, #{line => L, column => C, type => undefined}, string}
{type_basic, #{line => L, column => C, type => undefined}, boolean}
{type_basic, #{line => L, column => C, type => undefined}, atom}
{type_basic, #{line => L, column => C, type => undefined}, nil}
{type_basic, #{line => L, column => C, type => undefined}, any}

% Função: {fun, [arg_types], return_type}
{type_fun, #{line => L, column => C, type => undefined}, [
  {type_list, #{line => L, column => C, type => undefined}, [integer, string]},
  boolean
]}

% Lista: {list, element_type}
{type_list, #{line => L, column => C, type => undefined}, integer}

% Tupla: {tuple, [element_types]}
{type_tuple, #{line => L, column => C, type => undefined}, [integer, string, boolean]}

% Variável de tipo: {type_var, name}
{type_var, #{line => L, column => C, type => undefined}, 'T'}
```

### Atualização do Parser para Tipos

#### Novas Regras no Parser (`yecc/lx_parser.yrl`)
```erlang
% Macro definitions with type annotations
macro_def -> defmacro ident '(' macro_args_with_types ')' do macro_body end :
    {defmacro, #{line => get_line('$1'), column => get_column('$1'), type => undefined}, [
        extract_ident('$2'),
        '$4',
        '$7'
    ]}.

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
type_fun -> '{' 'fun' ',' '[' type_list ']' ',' type_expr '}' :
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
```

### Atualização do Lexer para Tipos

#### Novas Regras no Lexer (`leex/lx_lexer.xrl`)
```erlang
% Type annotation operator
:: : {token, {'::', TokenLine, TokenCol}}.

% Type identifiers (start with uppercase)
{A-Z}({L}|{D})* : {token, {type_identifier, TokenLine, TokenCol, TokenChars}}.

% Type keywords
integer : {token, {type_identifier, TokenLine, TokenCol, "integer"}}.
float : {token, {type_identifier, TokenLine, TokenCol, "float"}}.
string : {token, {type_identifier, TokenLine, TokenCol, "string"}}.
boolean : {token, {type_identifier, TokenLine, TokenCol, "boolean"}}.
atom : {token, {type_identifier, TokenLine, TokenCol, "atom"}}.
nil : {token, {type_identifier, TokenLine, TokenCol, "nil"}}.
any : {token, {type_identifier, TokenLine, TokenCol, "any"}}.
fun : {token, {type_identifier, TokenLine, TokenCol, "fun"}}.
list : {token, {type_identifier, TokenLine, TokenCol, "list"}}.
tuple : {token, {type_identifier, TokenLine, TokenCol, "tuple"}}.
```

### Sistema Hindley-Milner Atualizado

#### Arquivo: `src/lx_types.erl`
```erlang
-module(lx_types).

-export([infer_types/1, check_macro_types/2, unify_types/2]).

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

%% Infer types for AST with macro support
infer_types(AST) ->
    Env = new_type_env(),
    case infer_ast_types(AST, Env) of
        {ok, TypedAST, Specs} ->
            {ok, update_ast_types(TypedAST), Specs};
        {error, Errors} ->
            {error, Errors}
    end.

%% Check macro call types
check_macro_types({macro_call, Meta, [MacroName, Args]}, Env) ->
    case get_macro_signature(MacroName, length(Args), Env) of
        {ok, Signature} ->
            {ArgTypes, ArgAST} = infer_argument_types(Args, Env),
            case unify_macro_signature(Signature, ArgTypes) of
                {ok, ReturnType, Substitution} ->
                    {ok, update_node_type({macro_call, Meta, [MacroName, ArgAST]}, ReturnType)};
                {error, Error} ->
                    {error, {macro_type_error, MacroName, Error}}
            end;
        {error, Error} ->
            {error, {macro_not_found, MacroName, Error}}
    end.

%% Register macro signature when macro is defined
register_macro_signature(MacroName, Args, ReturnType, Env) ->
    #{macros := Macros} = Env,
    NewMacros = maps:update_with(MacroName,
        fun(Signatures) -> [{MacroName, Args, ReturnType} | Signatures] end,
        [{MacroName, Args, ReturnType}],
        Macros),
    Env#{macros := NewMacros}.

%% Get macro signature from environment
get_macro_signature(MacroName, Arity, #{macros := Macros}) ->
    case maps:find(MacroName, Macros) of
        {ok, Signatures} ->
            case find_signature_by_arity(Signatures, Arity) of
                {ok, Signature} -> {ok, Signature};
                error -> {error, {arity_mismatch, MacroName, Arity}}
            end;
        error ->
            {error, {macro_not_found, MacroName}}
    end.

%% Find signature by arity
find_signature_by_arity([{Name, Args, ReturnType} | _], Arity) when length(Args) =:= Arity ->
    {ok, {Name, Args, ReturnType}};
find_signature_by_arity([_ | Rest], Arity) ->
    find_signature_by_arity(Rest, Arity);
find_signature_by_arity([], _Arity) ->
    error.

%% Unify macro signature with argument types
unify_macro_signature({_Name, ExpectedArgs, ReturnType}, ActualArgTypes) ->
    case unify_argument_lists(ExpectedArgs, ActualArgTypes) of
        {ok, Substitution} ->
            {ok, apply_substitution(ReturnType, Substitution), Substitution};
        {error, Error} ->
            {error, Error}
    end.

%% Unify argument lists
unify_argument_lists([], []) ->
    {ok, #{}};
unify_argument_lists([{_Name, ExpectedType} | ExpectedRest], [ActualType | ActualRest]) ->
    case unify_types(ExpectedType, ActualType) of
        {ok, Substitution} ->
            case unify_argument_lists(ExpectedRest, ActualRest) of
                {ok, RestSubstitution} ->
                    {ok, maps:merge(Substitution, RestSubstitution)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_argument_lists(_, _) ->
    {error, argument_count_mismatch}.

%% Unify two types
unify_types(Type1, Type2) when Type1 =:= Type2 ->
    {ok, #{}};
unify_types({type_var, Name}, Type) ->
    {ok, #{Name => Type}};
unify_types(Type, {type_var, Name}) ->
    {ok, #{Name => Type}};
unify_types({type_fun, Args1, Return1}, {type_fun, Args2, Return2}) ->
    case unify_types(Args1, Args2) of
        {ok, Sub1} ->
            case unify_types(Return1, Return2) of
                {ok, Sub2} ->
                    {ok, maps:merge(Sub1, Sub2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_types({type_list, Element1}, {type_list, Element2}) ->
    unify_types(Element1, Element2);
unify_types({type_tuple, Elements1}, {type_tuple, Elements2}) ->
    unify_tuple_elements(Elements1, Elements2);
unify_types(_, _) ->
    {error, type_mismatch}.

%% Unify tuple elements
unify_tuple_elements([], []) ->
    {ok, #{}};
unify_tuple_elements([E1 | Rest1], [E2 | Rest2]) ->
    case unify_types(E1, E2) of
        {ok, Sub1} ->
            case unify_tuple_elements(Rest1, Rest2) of
                {ok, Sub2} ->
                    {ok, maps:merge(Sub1, Sub2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_tuple_elements(_, _) ->
    {error, tuple_size_mismatch}.

%% Apply substitution to type
apply_substitution(Type, Substitution) ->
    case Type of
        {type_var, Name} ->
            maps:get(Name, Substitution, Type);
        {type_fun, Args, Return} ->
            {type_fun, apply_substitution(Args, Substitution), apply_substitution(Return, Substitution)};
        {type_list, Element} ->
            {type_list, apply_substitution(Element, Substitution)};
        {type_tuple, Elements} ->
            {type_tuple, [apply_substitution(E, Substitution) || E <- Elements]};
        _ ->
            Type
    end.

%% Create new type environment (no built-in signatures)
new_type_env() ->
    #{
        variables => #{},
        macros => #{}
    }.
```
```

## 5. Exemplos de AST Gerada

### Exemplo 1: Macro com Tipos Específicos
```lx
% Definindo macro com tipos específicos
defmacro +(left :: number, right :: number) do
  {binary_op, [], [+, left, right]}
end

% Uso:
result = 5 + 3

% Processo de compilação:
% 1. Parser gera AST com definição da macro
% 2. Sistema registra assinatura: {+, [{left, number}, {right, number}], number}
% 3. Ao encontrar "5 + 3", verifica tipos e unifica
% 4. Result: number

% AST após parser:
% {bind, #{line => 1, column => 1, type => undefined}, [
%   {ident, #{line => 1, column => 1, type => undefined}, result},
%   {macro_call, #{line => 1, column => 5, type => undefined}, [
%     '+',
%     [
%       {integer, #{line => 1, column => 3, type => undefined}, 5},
%       {integer, #{line => 1, column => 7, type => undefined}, 3}
%     ]
%   ]
% ]}

% AST após inferência de tipos:
% {bind, #{line => 1, column => 1, type => number}, [
%   {ident, #{line => 1, column => 1, type => number}, result},
%   {macro_call, #{line => 1, column => 5, type => number}, [
%     '+',
%     [
%       {integer, #{line => 1, column => 3, type => number}, 5},
%       {integer, #{line => 1, column => 7, type => number}, 3}
%     ]
%   ]
% ]}
```

### Exemplo 2: Macro com Tipos Compostos
```lx
% Macro com tipos compostos
defmacro map(fun :: {fun, [T], U}, list :: {list, T}) do
  {list_map, [], [fun, list]}
end

% Uso:
doubled = map(fn(x) do x * 2 end, [1, 2, 3])

% Processo de compilação:
% 1. Parser gera AST com definição da macro
% 2. Sistema registra assinatura: {map, [{fun, {fun, [T], U}}, {list, {list, T}}], {list, U}}
% 3. Ao encontrar map(fn(x) do x * 2 end, [1, 2, 3]):
%    - fn(x) do x * 2 end -> {fun, [number], number}
%    - [1, 2, 3] -> {list, number}
%    - Unifica: T = number, U = number
% 4. Result: {list, number}
```

### Exemplo 3: Macro com Variáveis de Tipo
```lx
% Macro com variáveis de tipo
defmacro filter(fun :: {fun, [T], boolean}, list :: {list, T}) do
  {list_filter, [], [fun, list]}
end

% Uso:
evens = filter(fn(x) do x % 2 == 0 end, [1, 2, 3, 4, 5])

% Processo de compilação:
% 1. Parser gera AST com definição da macro
% 2. Sistema registra assinatura: {filter, [{fun, {fun, [T], boolean}}, {list, {list, T}}], {list, T}}
% 3. Ao encontrar filter(fn(x) do x % 2 == 0 end, [1, 2, 3, 4, 5]):
%    - fn(x) do x % 2 == 0 end -> {fun, [number], boolean}
%    - [1, 2, 3, 4, 5] -> {list, number}
%    - Unifica: T = number
% 4. Result: {list, number}
```

### Exemplo 4: Bind com Semicolon e Tipos
```lx
x = 42;
y = "hello"
```

**AST após parser:**
```erlang
[
    {bind, #{line => 1, column => 2, type => undefined}, [
        {ident, #{line => 1, column => 1, type => undefined}, x},
        {integer, #{line => 1, column => 5, type => undefined}, 42}
    ]},
    {bind, #{line => 2, column => 2, type => undefined}, [
        {ident, #{line => 2, column => 1, type => undefined}, y},
        {string, #{line => 2, column => 5, type => undefined}, "hello"}
    ]}
]
```

**AST após inferência de tipos:**
```erlang
[
    {bind, #{line => 1, column => 2, type => integer}, [
        {ident, #{line => 1, column => 1, type => integer}, x},
        {integer, #{line => 1, column => 5, type => integer}, 42}
    ]},
    {bind, #{line => 2, column => 2, type => binary}, [
        {ident, #{line => 2, column => 1, type => binary}, y},
        {string, #{line => 2, column => 5, type => binary}, "hello"}
    ]}
]
```

## 6. Pipeline de Compilação Atualizado

### Arquivo: `src/lx.erl`

```erlang
-module(lx).

-export([compile/1, compile/2]).

%% Main compilation pipeline
compile(Source) ->
    compile(Source, #{}).

compile(Source, Options) ->
    % Phase 1: Lexical analysis
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            % Phase 2: Syntactic analysis
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    % Phase 3: Macro expansion with type checking
                    case macro_expansion_with_types(AST) of
                        {ok, ExpandedAST} ->
                            % Phase 4: Type inference and meta update
                            case type_analysis(ExpandedAST) of
                                {ok, TypedAST, Specs} ->
                                    % Phase 5: Code generation
                                    code_generation(TypedAST, Specs, Options);
                                {error, TypeErrors} ->
                                    {error, TypeErrors}
                            end;
                        {error, MacroErrors} ->
                            {error, MacroErrors}
                    end;
                {error, SyntaxError} ->
                    {error, SyntaxError}
            end;
        {error, LexicalError} ->
            {error, LexicalError}
    end.

%% Macro expansion with type checking
macro_expansion_with_types(AST) ->
    % Start with empty macro environment - no built-in signatures
    MacroEnv = new_type_env(),
    case lx_macros:expand_macros_with_types(AST, MacroEnv) of
        {ok, ExpandedAST} ->
            {ok, ExpandedAST};
        {error, Error} ->
            {error, {macro_expansion_error, Error}}
    end.

%% Type analysis phase (inference + meta update)
type_analysis(AST) ->
    case lx_types:infer_types(AST) of
        {ok, TypedAST, Specs} ->
            {ok, TypedAST, Specs};
        {error, Error} ->
            {error, {type_analysis_error, Error}}
    end.

%% ... rest of compilation pipeline ...
```

## 7. Testes

### Arquivo: `test/lx_ast_meta_tests.erl`

```erlang
-module(lx_ast_meta_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test AST meta information
ast_meta_test() ->
    Source = "x = 42",
    {ok, AST} = lx:parse(Source),
    [{bind, Meta, _}] = AST,

    % Check meta has line, column, and type
    ?assert(maps:is_key(line, Meta)),
    ?assert(maps:is_key(column, Meta)),
    ?assert(maps:is_key(type, Meta)),

    % Check values
    ?assertEqual(1, maps:get(line, Meta)),
    ?assertEqual(2, maps:get(column, Meta)),
    ?assertEqual(undefined, maps:get(type, Meta)).

%% Test type inference updates meta
type_inference_meta_test() ->
    Source = "x = 42",
    {ok, AST} = lx:parse(Source),
    {ok, TypedAST, _} = lx_types:infer_types(AST),
    [{bind, Meta, _}] = TypedAST,

    % Check type was updated
    ?assertEqual(integer, maps:get(type, Meta)).

%% Test macro type checking
macro_type_check_test() ->
    Source = "defmacro +(left :: number, right :: number) do {binary_op, [], [+, left, right]} end",
    {ok, AST} = lx:parse(Source),
    [{defmacro, Meta, [Name, Args, Body]}] = AST,

    % Check macro definition has correct structure
    ?assertEqual('+', Name),
    ?assertEqual(2, length(Args)).

%% Test macro call with types
macro_call_type_test() ->
    Source = "result = 5 + 3",
    {ok, AST} = lx:parse(Source),
    {ok, TypedAST, _} = lx_types:infer_types(AST),
    [{bind, Meta, [_, {macro_call, CallMeta, ['+', Args]}]}] = TypedAST,

    % Check macro call has correct type
    ?assertEqual(number, maps:get(type, CallMeta)),
    ?assertEqual(2, length(Args)).

%% Test multiple statements with semicolon
semicolon_meta_test() ->
    Source = "x = 42; y = \"hello\"",
    {ok, AST} = lx:parse(Source),
    [Bind1, Bind2] = AST,

    % Check both binds have correct meta
    {bind, Meta1, _} = Bind1,
    {bind, Meta2, _} = Bind2,

    ?assertEqual(1, maps:get(line, Meta1)),
    ?assertEqual(2, maps:get(line, Meta2)).
```

## 8. Checklist de Implementação

### Fase 1: Lexer e Parser com Tipos (2-3 dias)
- [ ] Atualizar `leex/lx_lexer.xrl` com tokens de tipo (`::`, `type_identifier`)
- [ ] Implementar contador de coluna se necessário
- [ ] Atualizar `yecc/lx_parser.yrl` com regras para tipos
- [ ] Adicionar suporte a `ident_with_type` e `type_expr`
- [ ] Testar parsing de macros com anotações de tipo

### Fase 2: Sistema de Tipos com Macros (2-3 dias)
- [ ] Atualizar `include/lx.hrl` com tipos para macros
- [ ] Implementar `lx_types.erl` com Hindley-Milner para macros
- [ ] Adicionar `check_macro_types/2` e `unify_macro_signature/2`
- [ ] Implementar ambiente de tipos com assinaturas de macro
- [ ] Testar inferência de tipos para chamadas de macro

### Fase 3: Sistema de Macros com Tipos (2-3 dias)
- [ ] Implementar `lx_macros.erl` com verificação de tipos
- [ ] Implementar `register_macro_signature/4` para registro dinâmico
- [ ] Implementar `expand_macros_with_types/2` sem assinaturas built-in
- [ ] Testar expansão de macros com verificação de tipos
- [ ] Validar AST final com tipos corretos

### Fase 4: Integração e Testes (1-2 dias)
- [ ] Atualizar pipeline de compilação com verificação de tipos
- [ ] Criar testes abrangentes para macros com tipos
- [ ] Validar exemplos complexos com polimorfismo
- [ ] Documentar sistema de tipos em macros

## 9. Vantagens da Nova Implementação

### 1. **Type Safety com Macros**
- Verificação de tipos em tempo de compilação para macros
- Erros de tipo detectados antes da execução
- Macros com tipos garantem corretude

### 2. **Flexibilidade de Tipos**
- Tipos podem ser especificados ou inferidos
- Variáveis de tipo permitem polimorfismo
- Sistema extensível para novos tipos

### 3. **Integração com Hindley-Milner**
- Unificação automática de tipos para macros
- Inferência de tipos para expressões complexas
- Propagação correta de tipos através da AST

### 4. **Precisão de Posições**
- Linha e coluna corretas para debugging
- Mensagens de erro precisas com tipos
- Suporte a ferramentas de análise estática

### 5. **Extensibilidade Total**
- Tudo é macro (exceto `defmacro`)
- Linguagem auto-extensível com type safety
- DSLs fáceis de criar com tipos garantidos

### 6. **Compatibilidade**
- Modelo Elixir maduro e testado
- Compatível com ecossistema Erlang
- Ferramentas existentes funcionam

## 10. Conclusão

Esta refatoração transforma o lx em uma linguagem moderna e type-safe com:

- **AST Elixir-style** com meta completo (linha, coluna, tipo)
- **Sistema de tipos** integrado com Hindley-Milner para macros
- **Type safety** em tempo de compilação para todas as macros
- **Extensibilidade total** via macros com verificação de tipos
- **Polimorfismo** através de variáveis de tipo
- **Registro dinâmico** de assinaturas de macro (sem built-ins)
- **Compatibilidade** com ecossistema Erlang

O resultado é uma base sólida para desenvolvimento futuro, com ferramentas de análise avançadas, type safety garantido e capacidade de extensão ilimitada com verificação de tipos. **Todas as assinaturas de macro são definidas dinamicamente em tempo de compilação**, garantindo máxima flexibilidade e consistência.

---

**Próximos Passos:**
1. Implementar Fase 1 (Lexer e Parser com Tipos)
2. Implementar Fase 2 (Sistema de Tipos com Macros)
3. Implementar Fase 3 (Sistema de Macros com Tipos)
4. Executar Fase 4 (Integração e Testes)

Com esta implementação, o lx se tornará uma das linguagens funcionais mais poderosas e type-safe disponíveis, combinando a flexibilidade das macros com a segurança dos tipos, onde **tudo é definido via macros, incluindo as próprias assinaturas de tipo**.