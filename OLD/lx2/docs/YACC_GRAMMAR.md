# Gramática Yacc para LX2

## Visão Geral

O LX2 utiliza o `yecc` (Yacc para Erlang) para definir a gramática da linguagem LX. Esta abordagem oferece maior robustez e manutenibilidade comparada ao parser manual implementado no LX1.

## Estrutura da Gramática

### Arquivo `parser.yrl`

```erlang
Nonterminals
    program
    function_def
    function_head
    function_body
    expression
    literal
    variable
    binary_op
    list_expr
    tuple_expr
    map_expr
    record_def
    record_literal
    record_access
    directive_call
    block
    pattern
    guard
    .

Terminals
    'def' 'do' 'end' 'record' 'module' 'application'
    'when' 'case' 'of' 'receive' 'after' 'spawn' 'link' 'monitor'
    'fun' '->' '=>' '::' '|' '++' 'in'
    '(' ')' '[' ']' '{' '}' '%' '#' '.' ':' ';' ',' '='
    '+' '-' '*' '/' '==' '!=' '<' '<=' '>' '>='
    'and' 'or' 'not' '&&&' '|||' '^^^' '<<<' '>>>'
    'true' 'false' 'nil'
    integer float string atom identifier
    '$print' '$type'
    .

Rootsymbol program.

%% Gramática principal
program -> function_defs : '$1'.
program -> record_defs function_defs : {program, '$1', '$2'}.

function_defs -> function_def : ['$1'].
function_defs -> function_def function_defs : ['$1' | '$2'].

%% Definições de função
function_def -> 'def' identifier '(' ')' 'do' block 'end' :
    {function_def, {identifier, '$2'}, [], '$6'}.

function_def -> 'def' identifier '(' parameters ')' 'do' block 'end' :
    {function_def, {identifier, '$2'}, '$4', '$7'}.

function_def -> 'def' identifier 'do' function_heads 'end' :
    {multi_head_function, {identifier, '$2'}, '$4'}.

%% Parâmetros e patterns
parameters -> parameter : ['$1'].
parameters -> parameter ',' parameters : ['$1' | '$3'].

parameter -> identifier : {parameter, '$1'}.
parameter -> pattern : {parameter, '$1'}.

function_heads -> function_head : ['$1'].
function_heads -> function_head function_heads : ['$1' | '$2'].

function_head -> pattern '->' expression : {function_head, '$1', '$3'}.
function_head -> pattern '->' expression ';' : {function_head, '$1', '$3'}.

%% Patterns
pattern -> literal : {pattern, '$1'}.
pattern -> identifier : {pattern, '$1'}.
pattern -> list_pattern : {pattern, '$1'}.
pattern -> tuple_pattern : {pattern, '$1'}.
pattern -> map_pattern : {pattern, '$1'}.
pattern -> record_pattern : {pattern, '$1'}.

list_pattern -> '[' ']' : {empty_list}.
list_pattern -> '[' pattern_list ']' : {list_pattern, '$2'}.
list_pattern -> '[' pattern '|' pattern ']' : {cons_pattern, '$2', '$4'}.

pattern_list -> pattern : ['$1'].
pattern_list -> pattern ',' pattern_list : ['$1' | '$3'].

tuple_pattern -> '{' '}' : {empty_tuple}.
tuple_pattern -> '{' pattern_list '}' : {tuple_pattern, '$2'}.

map_pattern -> '%{' '}' : {empty_map}.
map_pattern -> '%{' map_pattern_entries '}' : {map_pattern, '$2'}.

map_pattern_entries -> map_pattern_entry : ['$1'].
map_pattern_entries -> map_pattern_entry ',' map_pattern_entries : ['$1' | '$3'].

map_pattern_entry -> pattern ':' pattern : {map_entry, '$1', '$3'}.

%% Expressões
expression -> literal : {expression, '$1'}.
expression -> variable : {expression, '$1'}.
expression -> binary_expression : {expression, '$1'}.
expression -> list_expr : {expression, '$1'}.
expression -> tuple_expr : {expression, '$1'}.
expression -> map_expr : {expression, '$1'}.
expression -> record_access : {expression, '$1'}.
expression -> function_call : {expression, '$1'}.
expression -> directive_call : {expression, '$1'}.
expression -> '(' expression ')' : {parenthesized, '$2'}.

%% Literais
literal -> integer : {integer, '$1'}.
literal -> float : {float, '$1'}.
literal -> string : {string, '$1'}.
literal -> atom : {atom, '$1'}.
literal -> 'true' : {boolean, true}.
literal -> 'false' : {boolean, false}.
literal -> 'nil' : {nil}.

%% Variáveis
variable -> identifier : {variable, '$1'}.

%% Operadores binários
binary_expression -> expression binary_op expression : {binary_op, '$2', '$1', '$3'}.

binary_op -> '+' : {operator, '+'}.
binary_op -> '-' : {operator, '-'}.
binary_op -> '*' : {operator, '*'}.
binary_op -> '/' : {operator, '/'}.
binary_op -> '==' : {operator, '=='}.
binary_op -> '!=' : {operator, '!='}.
binary_op -> '<' : {operator, '<'}.
binary_op -> '<=' : {operator, '<='}.
binary_op -> '>' : {operator, '>'}.
binary_op -> '>=' : {operator, '>='}.
binary_op -> 'and' : {operator, 'and'}.
binary_op -> 'or' : {operator, 'or'}.
binary_op -> '&&&' : {operator, '&&&'}.
binary_op -> '|||' : {operator, '|||'}.
binary_op -> '^^^' : {operator, '^^^'}.
binary_op -> '<<<' : {operator, '<<<'}.
binary_op -> '>>>' : {operator, '>>>'}.
binary_op -> '++' : {operator, '++'}.
binary_op -> 'in' : {operator, 'in'}.

%% Listas
list_expr -> '[' ']' : {empty_list}.
list_expr -> '[' expression_list ']' : {list, '$2'}.
list_expr -> '[' expression '|' expression ']' : {cons, '$2', '$4'}.

expression_list -> expression : ['$1'].
expression_list -> expression ',' expression_list : ['$1' | '$3'].

%% Tuplas
tuple_expr -> '{' '}' : {empty_tuple}.
tuple_expr -> '{' expression_list '}' : {tuple, '$2'}.

%% Maps
map_expr -> '%{' '}' : {empty_map}.
map_expr -> '%{' map_entries '}' : {map, '$2'}.

map_entries -> map_entry : ['$1'].
map_entries -> map_entry ',' map_entries : ['$1' | '$3'].

map_entry -> expression ':' expression : {map_entry, '$1', '$3'}.

%% Records
record_defs -> record_def : ['$1'].
record_defs -> record_def record_defs : ['$1' | '$2'].

record_def -> 'record' identifier '{' record_fields '}' :
    {record_def, {identifier, '$2'}, '$4'}.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> identifier '::' type : {record_field, '$1', '$3', undefined}.
record_field -> identifier '=' expression '::' type : {record_field, '$1', '$5', '$3'}.
record_field -> identifier '=' expression : {record_field, '$1', undefined, '$3'}.

type -> 'integer' : {type, integer}.
type -> 'float' : {type, float}.
type -> 'string' : {type, string}.
type -> 'boolean' : {type, boolean}.
type -> 'atom' : {type, atom}.
type -> 'nil' : {type, nil}.
type -> 'any' : {type, any}.

record_literal -> identifier '{' record_field_assignments '}' :
    {record_literal, '$1', '$3'}.

record_field_assignments -> record_field_assignment : ['$1'].
record_field_assignments -> record_field_assignment ',' record_field_assignments : ['$1' | '$3'].

record_field_assignment -> identifier ':' expression : {field_assignment, '$1', '$3'}.

record_access -> expression '.' identifier : {record_access, '$1', '$3'}.

%% Chamadas de função
function_call -> identifier '(' ')' : {function_call, '$1', []}.
function_call -> identifier '(' arguments ')' : {function_call, '$1', '$3'}.

arguments -> expression : ['$1'].
arguments -> expression ',' arguments : ['$1' | '$3'].

%% Diretivas
directive_call -> '$print' '(' expression ')' : {directive_call, print, '$3'}.
directive_call -> '$type' '(' expression ')' : {directive_call, type, '$3'}.

%% Blocos
block -> expression : {block, ['$1']}.
block -> expression ';' block : {block, ['$1' | '$3']}.

%% Guards
guard -> 'when' guard_expression : {guard, '$2'}.

guard_expression -> expression : {guard_expr, '$1'}.
guard_expression -> expression 'and' guard_expression : {guard_and, '$1', '$3'}.
guard_expression -> expression 'or' guard_expression : {guard_or, '$1', '$3'}.

Erlang code.

%% Funções auxiliares para construção de AST
build_ast(Node) ->
    case Node of
        {function_def, Name, Params, Body} ->
            {function_def, Name, Params, Body};
        {multi_head_function, Name, Heads} ->
            {multi_head_function, Name, Heads};
        {expression, Expr} ->
            Expr;
        {binary_op, Op, Left, Right} ->
            {binary_op, Op, Left, Right};
        {list, Elements} ->
            {list, Elements};
        {tuple, Elements} ->
            {tuple, Elements};
        {map, Entries} ->
            {map, Entries};
        {record_def, Name, Fields} ->
            {record_def, Name, Fields};
        {record_literal, Name, Fields} ->
            {record_literal, Name, Fields};
        {record_access, Record, Field} ->
            {record_access, Record, Field};
        {function_call, Name, Args} ->
            {function_call, Name, Args};
        {directive_call, Type, Expr} ->
            {directive_call, Type, Expr};
        {block, Expressions} ->
            {block, Expressions};
        {guard, Expr} ->
            {guard, Expr};
        _ ->
            Node
    end.
```

## Precedência de Operadores

### Definição de Precedência

```erlang
Left 100 'or'.
Left 200 'and'.
Left 300 '==' '!=' '<' '<=' '>' '>='.
Left 400 '++'.
Left 500 '+' '-'.
Left 600 '*' '/'.
Left 700 '&&&' '|||' '^^^'.
Left 800 '<<<' '>>>'.
```

### Regras de Associatividade

- **Operadores aritméticos**: Associativos à esquerda
- **Operadores de comparação**: Associativos à esquerda
- **Operadores lógicos**: Associativos à esquerda
- **Operadores bitwise**: Associativos à esquerda
- **Operador de concatenação**: Associativo à direita

## Tratamento de Erros

### Estrutura de Erros

```erlang
-type parse_error() :: {
    parse_error,
    Line :: integer(),
    Column :: integer(),
    Message :: string(),
    Context :: string()
}.
```

### Funções de Tratamento de Erro

```erlang
%% Tratamento de erros de sintaxe
handle_parse_error(Token, Line, Column, Message) ->
    {parse_error, Line, Column, Message, format_context(Token)}.

%% Formatação de contexto
format_context(Token) ->
    case Token of
        {identifier, Name} -> "identifier '" ++ atom_to_list(Name) ++ "'";
        {integer, Value} -> "integer " ++ integer_to_list(Value);
        {string, Value} -> "string \"" ++ Value ++ "\"";
        {atom, Value} -> "atom '" ++ atom_to_list(Value) ++ "'";
        _ -> "token " ++ atom_to_list(Token)
    end.

%% Recuperação de erros
recover_from_error(Tokens, Error) ->
    % Pular tokens até encontrar um ponto de sincronização
    SynchronizationTokens = ['def', 'record', 'end', ';'],
    skip_until_sync(Tokens, SynchronizationTokens, Error).

skip_until_sync([Token | Rest], SyncTokens, Error) ->
    case lists:member(element(1, Token), SyncTokens) of
        true -> {ok, [Token | Rest]};
        false -> skip_until_sync(Rest, SyncTokens, Error)
    end;
skip_until_sync([], _SyncTokens, Error) ->
    {error, Error}.
```

## Integração com Lexer

### Interface com Leex

```erlang
%% Tokenização
tokenize(Source) ->
    case leex:string(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, leex, {illegal, Char}}, _EndLine} ->
            {error, {lexical_error, Line, "illegal character: " ++ [Char]}};
        {error, {Line, leex, {user, Error}}, _EndLine} ->
            {error, {lexical_error, Line, Error}}
    end.

%% Conversão de tokens para formato Yacc
convert_tokens(Tokens) ->
    lists:map(fun convert_token/1, Tokens).

convert_token({Token, Line, Column}) ->
    {Token, Line, Column};
convert_token({Token, Line, Column, Value}) ->
    {Token, Line, Column, Value}.
```

## Otimizações

### 1. Análise de Lookahead

```erlang
%% Análise de lookahead para resolução de ambiguidades
lookahead_analysis(Tokens) ->
    analyze_lookahead(Tokens, []).

analyze_lookahead([Token | Rest], Acc) ->
    case needs_lookahead(Token) of
        true ->
            case Rest of
                [NextToken | _] ->
                    ResolvedToken = resolve_ambiguity(Token, NextToken),
                    analyze_lookahead(Rest, [ResolvedToken | Acc]);
                [] ->
                    analyze_lookahead(Rest, [Token | Acc])
            end;
        false ->
            analyze_lookahead(Rest, [Token | Acc])
    end;
analyze_lookahead([], Acc) ->
    lists:reverse(Acc).

%% Detecção de tokens que precisam de lookahead
needs_lookahead({':', _Line, _Column}) -> true;
needs_lookahead({identifier, _Line, _Column}) -> true;
needs_lookahead(_) -> false.

%% Resolução de ambiguidades
resolve_ambiguity({':', Line, Column}, {identifier, _, _}) ->
    {atom, Line, Column};
resolve_ambiguity({':', Line, Column}, _) ->
    {colon, Line, Column}.
```

### 2. Cache de Parsing

```erlang
%% Cache de parsing para expressões comuns
-type parse_cache() :: #{expression() => ast_node()}.

parse_with_cache(Source, Cache) ->
    case maps:get(Source, Cache, not_found) of
        not_found ->
            case parse(Source) of
                {ok, AST} ->
                    {ok, AST, Cache#{Source => AST}};
                Error ->
                    {Error, Cache}
            end;
        CachedAST ->
            {ok, CachedAST, Cache}
    end.
```

## Testes da Gramática

### Testes Unitários

```erlang
%% Testes para literais
literal_test() ->
    ?assertMatch({ok, {integer, 42}}, parse("42")),
    ?assertMatch({ok, {float, 3.14}}, parse("3.14")),
    ?assertMatch({ok, {string, "hello"}}, parse("\"hello\"")),
    ?assertMatch({ok, {atom, ok}}, parse(":ok")),
    ?assertMatch({ok, {boolean, true}}, parse("true")),
    ?assertMatch({ok, {nil}}, parse("nil")).

%% Testes para expressões binárias
binary_expression_test() ->
    ?assertMatch({ok, {binary_op, '+', {integer, 1}, {integer, 2}}},
                 parse("1 + 2")),
    ?assertMatch({ok, {binary_op, '*', {binary_op, '+', {integer, 1}, {integer, 2}}, {integer, 3}}},
                 parse("(1 + 2) * 3")).

%% Testes para estruturas de dados
data_structures_test() ->
    ?assertMatch({ok, {list, [{integer, 1}, {integer, 2}, {integer, 3}]}},
                 parse("[1, 2, 3]")),
    ?assertMatch({ok, {tuple, [{integer, 1}, {string, "hello"}]}},
                 parse("{1, \"hello\"}")),
    ?assertMatch({ok, {map, [{map_entry, {atom, key}, {string, "value"}}]}},
                 parse("%{key: \"value\"}")).

%% Testes para funções
function_test() ->
    Source = "def add(a, b) do a + b end",
    ?assertMatch({ok, {function_def, {identifier, add}, [{parameter, {identifier, a}}, {parameter, {identifier, b}}], _}},
                 parse(Source)).

%% Testes para records
record_test() ->
    Source = "record Person { name :: string, age :: integer }",
    ?assertMatch({ok, {record_def, {identifier, 'Person'}, _}},
                 parse(Source)).
```

### Testes de Integração

```erlang
%% Testes de integração completos
integration_test() ->
    Source = "
        record Person { name :: string, age :: integer }

        def create_person(name, age) do
            Person{name: name, age: age}
        end

        def get_age(person) do
            person.age
        end
    ",
    ?assertMatch({ok, _}, parse(Source)).

%% Testes de erro
error_test() ->
    ?assertMatch({error, _}, parse("def func(")),  % Parêntese não fechado
    ?assertMatch({error, _}, parse("def func do")), % Bloco não fechado
    ?assertMatch({error, _}, parse("1 +"));         % Expressão incompleta
```

## Performance

### Métricas de Performance

```erlang
%% Benchmark de parsing
benchmark_parsing() ->
    Source = generate_test_source(1000), % 1000 linhas de código
    {Time, _} = timer:tc(fun() -> parse(Source) end),
    io:format("Parsing time: ~p ms~n", [Time div 1000]).

%% Análise de complexidade
analyze_complexity(Source) ->
    Tokens = tokenize(Source),
    AST = parse(Source),
    {token_count(Tokens), ast_depth(AST), ast_size(AST)}.
```

### Otimizações de Performance

1. **Lazy Evaluation**: Parsing lazy para expressões grandes
2. **Memoization**: Cache de resultados de parsing
3. **Streaming**: Processamento em stream para arquivos grandes
4. **Parallel Parsing**: Parsing paralelo para módulos independentes

## Conclusão

A gramática Yacc para LX2 oferece:

1. **Robustez**: Tratamento robusto de erros e recuperação
2. **Manutenibilidade**: Gramática declarativa e bem estruturada
3. **Performance**: Otimizações específicas para parsing eficiente
4. **Extensibilidade**: Fácil adição de novas construções
5. **Integração**: Integração nativa com o ecossistema Erlang
6. **Testabilidade**: Testes abrangentes e automatizados

Esta gramática forma a base sólida para um compilador LX2 robusto e eficiente.