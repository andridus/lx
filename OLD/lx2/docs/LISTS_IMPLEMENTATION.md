# Implementação de Listas no LX2

## Visão Geral

Este documento descreve a implementação completa de **Listas** no compilador LX2, seguindo as melhores práticas de Erlang/OTP e mantendo compatibilidade com a sintaxe LX1. A implementação inclui list literals, list cons, operadores de lista (definidos como macros) e sistema de tipos completo.

## Status do Projeto

**Fase**: Task 5 - Lists
**Status**: Planejamento

## Especificação da Sintaxe

### List Literals
```lx
def empty_list() do
    []
end

def number_list() do
    [1, 2, 3, 4, 5]
end

def mixed_list() do
    [1, "hello", :ok, true]
end

def nested_list() do
    [[1, 2], [3, 4], [5, 6]]
end
```

### List Cons (Constructor)
```lx
def list_cons() do
    [1 | [2, 3, 4]]
end

def nested_cons() do
    [1 | [2 | [3 | []]]]
end

def mixed_cons() do
    [head | tail] = [1, 2, 3, 4]
    {head, tail}
end
```

### Operadores de Lista (Macros)
```lx
% Definição dos operadores como macros
defmacro ++(left, right) do
    {++, _, [left, right]}
end

defmacro in(element, list) do
    {in, _, [element, list]}
end

defmacro length(list) do
    {length, _, [list]}
end

% Uso dos operadores
def list_operations() do
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]

    % Concatenation
    combined = list1 ++ list2

    % Length
    size = length(list1)

    % Membership
    has_three = 3 in list1

    {combined, size, has_three}
end
```

## Arquitetura da Implementação

### 1. Extensão do Lexer (leex/lx2_lexer.xrl)

```erlang
% Adicionar ao leex/lx2_lexer.xrl
Definitions.
D = [0-9]
L = [a-zA-Z_]
WS = [\s\t]
NL = \n|\r\n|\r

Rules.
% ... existing rules ...

% List tokens
\[ : {token, {lbracket, TokenLine}}.
\] : {token, {rbracket, TokenLine}}.
\| : {token, {pipe, TokenLine}}.
, : {token, {comma, TokenLine}}.

% ... rest of existing rules ...
```

### 2. Extensão do Parser (yecc/lx2_parser.yrl)

```erlang
Nonterminals
program function_def block statement expression literal variable_binding
list_literal list_cons list_expression expression_list
macro_def macro_call.

Terminals
def do end integer float string atom boolean nil identifier underscore
'(' ')' equals semicolon lbracket rbracket pipe comma
defmacro.

Rootsymbol program.

% ... existing rules ...

% List expressions
expression -> list_expression : '$1'.
expression -> macro_call : '$1'.

list_expression -> list_literal : '$1'.
list_expression -> list_cons : '$1'.

% List literals
list_literal -> lbracket rbracket : {list_literal, []}.
list_literal -> lbracket expression_list rbracket : {list_literal, '$2'}.

% List cons
list_cons -> lbracket expression pipe expression rbracket :
    {list_cons, '$2', '$4'}.

% Expression lists
expression_list -> expression : ['$1'].
expression_list -> expression comma expression_list : ['$1' | '$3'].

% Macro definitions
function_def -> defmacro identifier '(' identifier ',' identifier ')' do block end :
    {macro_def, extract_identifier('$2'), [extract_identifier('$4'), extract_identifier('$6')], '$9'}.

% Macro calls
macro_call -> identifier '(' expression ',' expression ')' :
    {macro_call, extract_identifier('$1'), ['$3', '$5']}.

% ... rest of existing rules ...
```

### 3. Sistema de Tipos (src/lx2_types.erl)

```erlang
% Adicionar ao src/lx2_types.erl

%% List type constructor
new_type_list(ElementType) ->
    {type_list, ElementType}.

%% Infer list literal type
infer_list_literal({list_literal, Elements}, Env) ->
    case Elements of
        [] ->
            % Empty list gets a polymorphic type
            {new_type_list(new_type_var(any)), new_substitution()};
        [First | Rest] ->
            % Infer type of first element
            {FirstType, Sub1} = infer_expression(First, Env),

            % Infer types of remaining elements
            {RestTypes, Sub2} = infer_expressions(Rest, apply_substitution(Env, Sub1)),

            % Unify all element types
            AllTypes = [FirstType | RestTypes],
            {UnifiedType, Sub3} = unify_list_types(AllTypes),

            % Compose substitutions
            FinalSub = compose_substitutions(Sub3, compose_substitutions(Sub2, Sub1)),

            {new_type_list(UnifiedType), FinalSub}
    end.

%% Infer list cons type
infer_list_cons({list_cons, Head, Tail}, Env) ->
    % Infer head type
    {HeadType, Sub1} = infer_expression(Head, Env),

    % Infer tail type
    {TailType, Sub2} = infer_expression(Tail, apply_substitution(Env, Sub1)),

    % Unify tail with list of head type
    ExpectedTailType = new_type_list(HeadType),
    case unify(TailType, ExpectedTailType) of
        {Sub3, _} ->
            FinalSub = compose_substitutions(Sub3, compose_substitutions(Sub2, Sub1)),
            {new_type_list(HeadType), FinalSub};
        {error, Error} ->
            {error, {type_mismatch, TailType, ExpectedTailType}}
    end.

%% Infer macro call type
infer_macro_call({macro_call, MacroName, Args}, Env) ->
    % Get macro definition
    case get_macro_definition(MacroName, Env) of
        {ok, MacroDef} ->
            % Infer types of arguments
            {ArgTypes, Sub1} = infer_expressions(Args, Env),

            % Check if argument types match macro signature
            case check_macro_signature(MacroDef, ArgTypes) of
                {ok, ReturnType, Sub2} ->
                    FinalSub = compose_substitutions(Sub2, Sub1),
                    {ReturnType, FinalSub};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Helper function to unify list element types
unify_list_types([Type]) ->
    {Type, new_substitution()};
unify_list_types([Type1, Type2 | Rest]) ->
    case unify(Type1, Type2) of
        {Sub, UnifiedType} ->
            case unify_list_types([UnifiedType | Rest]) of
                {FinalType, FinalSub} ->
                    {FinalType, compose_substitutions(FinalSub, Sub)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.
```

### 4. Gerador de Código (src/lx2_codegen.erl)

```erlang
% Adicionar ao src/lx2_codegen.erl

%% Generate list literal code
generate_list_literal({list_literal, Elements}) ->
    case Elements of
        [] ->
            "[]";
        _ ->
            ElementCodes = [generate_expression(Element) || Element <- Elements],
            "[" ++ string:join(ElementCodes, ", ") ++ "]"
    end.

%% Generate list cons code
generate_list_cons({list_cons, Head, Tail}) ->
    HeadCode = generate_expression(Head),
    TailCode = generate_expression(Tail),
    "[" ++ HeadCode ++ " | " ++ TailCode ++ "]".

%% Generate macro call code
generate_macro_call({macro_call, MacroName, Args}) ->
    ArgCodes = [generate_expression(Arg) || Arg <- Args],
    case MacroName of
        '++' ->
            [Left, Right] = ArgCodes,
            Left ++ " ++ " ++ Right;
        'in' ->
            [Element, List] = ArgCodes,
            "lists:member(" ++ Element ++ ", " ++ List ++ ")";
        'length' ->
            [List] = ArgCodes,
            "length(" ++ List ++ ")";
        _ ->
            % Generic macro call
            MacroNameStr = atom_to_list(MacroName),
            MacroNameStr ++ "(" ++ string:join(ArgCodes, ", ") ++ ")"
    end.
```

### 5. Sistema de Macros (src/lx2_macros.erl)

```erlang
% Criar src/lx2_macros.erl

-module(lx2_macros).

-export([expand_macro/3, get_macro_definition/2, register_macro/3]).

%% Macro definitions for list operations
-define(LIST_MACROS, #{
    '++' => #{
        arity => 2,
        body => fun(Left, Right) -> {concat, Left, Right} end,
        return_type => {type_list, {type_var, element}},
        description => "List concatenation"
    },
    'in' => #{
        arity => 2,
        body => fun(Element, List) -> {membership, Element, List} end,
        return_type => {type_const, boolean},
        description => "List membership test"
    },
    'length' => #{
        arity => 1,
        body => fun(List) -> {length, List} end,
        return_type => {type_const, integer},
        description => "List length"
    }
}).

%% Expand macro call
expand_macro(MacroName, Args, Env) ->
    case maps:find(MacroName, ?LIST_MACROS) of
        {ok, MacroDef} ->
            case apply_macro(MacroDef, Args, Env) of
                {ok, Expanded} -> {ok, Expanded};
                {error, Error} -> {error, Error}
            end;
        error ->
            {error, {undefined_macro, MacroName}}
    end.

%% Get macro definition
get_macro_definition(MacroName, _Env) ->
    case maps:find(MacroName, ?LIST_MACROS) of
        {ok, MacroDef} -> {ok, MacroDef};
        error -> {error, {undefined_macro, MacroName}}
    end.

%% Register new macro
register_macro(Name, Definition, Env) ->
    % This would be used for user-defined macros
    {ok, Env#{macros => maps:put(Name, Definition, maps:get(macros, Env, #{}))}}.

%% Apply macro definition
apply_macro(#{body := Body, arity := Arity}, Args, _Env) ->
    case length(Args) of
        Arity ->
            try
                Result = apply(Body, Args),
                {ok, Result}
            catch
                _:Error -> {error, {macro_application_error, Error}}
            end;
        _ ->
            {error, {arity_mismatch, Arity, length(Args)}}
    end.
```

## Exemplos de Uso

### Exemplos Básicos

```lx
% examples/task_05/basic_lists.lx
def empty_list() do
    []
end

def number_list() do
    [1, 2, 3, 4, 5]
end

def string_list() do
    ["hello", "world", "lx"]
end

def mixed_list() do
    [1, "hello", :ok, true, 3.14]
end

def nested_list() do
    [[1, 2], [3, 4], [5, 6]]
end
```

### Exemplos de List Cons

```lx
% examples/task_05/list_cons.lx
def simple_cons() do
    [1 | [2, 3, 4]]
end

def nested_cons() do
    [1 | [2 | [3 | []]]]
end

def pattern_matching() do
    [head | tail] = [1, 2, 3, 4]
    {head, tail}
end

def multiple_heads() do
    [first, second | rest] = [1, 2, 3, 4, 5]
    {first, second, rest}
end
```

### Exemplos de Operadores (Macros)

```lx
% examples/task_05/list_operations.lx
% Definição dos operadores como macros
defmacro ++(left, right) do
    {++, _, [left, right]}
end

defmacro in(element, list) do
    {in, _, [element, list]}
end

defmacro length(list) do
    {length, _, [list]}
end

% Uso dos operadores
def concatenation() do
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]
    list1 ++ list2
end

def length_operation() do
    numbers = [1, 2, 3, 4, 5]
    length(numbers)
end

def membership_test() do
    numbers = [1, 2, 3, 4, 5]
    3 in numbers
end

def complex_operations() do
    list1 = [1, 2, 3]
    list2 = [4, 5, 6]
    combined = list1 ++ list2
    size = length(combined)
    has_five = 5 in combined
    {combined, size, has_five}
end
```

### Exemplos de Tipos Mistos

```lx
% examples/task_05/mixed_types.lx
def nested_lists() do
    matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    first_row = [1, 2, 3]
    first_row in matrix
end

def list_of_tuples() do
    points = [{1, 2}, {3, 4}, {5, 6}]
    length(points)
end

def list_of_maps() do
    users = [%{name: "Alice", age: 30}, %{name: "Bob", age: 25}]
    length(users)
end

def complex_nested() do
    data = [
        {name: "Alice", scores: [85, 90, 92]},
        {name: "Bob", scores: [78, 85, 88]},
        {name: "Charlie", scores: [92, 95, 89]}
    ]
    length(data)
end
```

## Testes

### Testes Unitários

```erlang
% test/lx2_lists_tests.erl
-module(lx2_lists_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test list literals
list_literal_empty_test() ->
    Source = "def test() do [] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([], ModuleName:test()).

list_literal_numbers_test() ->
    Source = "def test() do [1, 2, 3] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

list_literal_mixed_test() ->
    Source = "def test() do [1, \"hello\", :ok] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, <<"hello">>, ok], ModuleName:test()).

%% Test list cons
list_cons_simple_test() ->
    Source = "def test() do [1 | [2, 3]] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

list_cons_nested_test() ->
    Source = "def test() do [1 | [2 | [3 | []]]] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3], ModuleName:test()).

%% Test macro definitions
macro_definition_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ [3, 4]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3, 4], ModuleName:test()).

%% Test list operations with macros
list_concatenation_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ [3, 4]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1, 2, 3, 4], ModuleName:test()).

list_length_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        def test() do
            length([1, 2, 3, 4, 5])
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(5, ModuleName:test()).

list_membership_test() ->
    Source = "
        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            3 in [1, 2, 3, 4, 5]
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(true, ModuleName:test()).

%% Test type errors (these should fail compilation)
list_type_error_concatenation_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        def test() do
            [1, 2] ++ 3
        end
    ",
    case lx2:compile(Source) of
        {error, {semantic_error, _}} ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

%% Test complex operations with macros
complex_list_operations_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        defmacro length(list) do
            {length, _, [list]}
        end

        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            list1 = [1, 2, 3]
            list2 = [4, 5, 6]
            combined = list1 ++ list2
            size = length(combined)
            has_five = 5 in combined
            {combined, size, has_five}
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({[1,2,3,4,5,6], 6, true}, ModuleName:test()).
```

### Testes de Integração

```erlang
% test/lx2_lists_integration_tests.erl
-module(lx2_lists_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Test complex list operations with macros
complex_list_operations_test() ->
    Source = "
        defmacro ++(left, right) do
            {++, _, [left, right]}
        end

        defmacro length(list) do
            {length, _, [list]}
        end

        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            list1 = [1, 2, 3]
            list2 = [4, 5, 6]
            combined = list1 ++ list2
            size = length(combined)
            has_five = 5 in combined
            {combined, size, has_five}
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({[1,2,3,4,5,6], 6, true}, ModuleName:test()).

%% Test nested lists with macros
nested_lists_test() ->
    Source = "
        defmacro length(list) do
            {length, _, [list]}
        end

        defmacro in(element, list) do
            {in, _, [element, list]}
        end

        def test() do
            matrix = [[1, 2], [3, 4], [5, 6]]
            first_row = [1, 2]
            has_first = first_row in matrix
            total_elements = length(matrix) * length(first_row)
            {has_first, total_elements}
        end
    ",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({true, 6}, ModuleName:test()).
```

## Plano de Implementação

### Fase 1: Lexer e Parser (Dia 1-2)

1. **Extender o Lexer**
   - Adicionar tokens para `[`, `]`, `|`, `,`
   - Adicionar token para `defmacro`

2. **Extender o Parser**
   - Implementar gramática para list literals
   - Implementar gramática para list cons
   - Implementar gramática para macro definitions
   - Implementar gramática para macro calls

### Fase 2: Sistema de Macros (Dia 3)

1. **Implementar Sistema de Macros**
   - Registro de macros
   - Expansão de macros
   - Validação de aridade

### Fase 3: Sistema de Tipos (Dia 4-5)

1. **Implementar Type Inference**
   - Inferência para list literals
   - Inferência para list cons
   - Inferência para macro calls

2. **Implementar Type Checking**
   - Verificação de compatibilidade de tipos
   - Unificação de tipos de elementos de lista

### Fase 4: Code Generation (Dia 6)

1. **Gerador de Código**
   - Geração de código para list literals
   - Geração de código para list cons
   - Geração de código para macro calls

### Fase 5: Testes e Validação (Dia 7)

1. **Testes Unitários**
   - Testes para cada funcionalidade
   - Testes de casos de erro

2. **Testes de Integração**
   - Testes de cenários complexos
   - Validação de compatibilidade

## Critérios de Aceitação

### Funcionalidade
- [ ] List literals funcionam corretamente
- [ ] List cons funciona corretamente
- [ ] Macro definitions funcionam
- [ ] Macro calls funcionam
- [ ] Operadores `++`, `in`, `length` funcionam como macros
- [ ] Pattern matching em listas funciona

### Tipos
- [ ] Type inference para listas vazias
- [ ] Type inference para listas homogêneas
- [ ] Type inference para listas heterogêneas
- [ ] Type checking para macro calls
- [ ] Error messages claros para type errors

### Performance
- [ ] Compilação em tempo aceitável
- [ ] Código gerado eficiente
- [ ] Sem memory leaks

### Compatibilidade
- [ ] Compatível com sintaxe LX1
- [ ] Compatível com Erlang/OTP
- [ ] Integração com sistema de tipos existente

## Riscos e Mitigações

### Riscos Técnicos
| Risco | Probabilidade | Impacto | Mitigação |
|-------|---------------|---------|-----------|
| Complexidade do Parser | Média | Alto | Implementação incremental |
| Macro System Complex | Alta | Médio | Testes extensivos |
| Performance Issues | Baixa | Médio | Profiling e otimização |

### Riscos de Compatibilidade
| Risco | Probabilidade | Impacto | Mitigação |
|-------|---------------|---------|-----------|
| Incompatibilidade LX1 | Baixa | Alto | Testes de regressão |
| Breaking Changes | Média | Médio | Versionamento cuidadoso |

## Conclusão

A implementação de listas no LX2 representa um marco importante no desenvolvimento do compilador, adicionando suporte a uma das estruturas de dados mais fundamentais em programação funcional. A abordagem baseada em macros garante flexibilidade e extensibilidade.

**Próximos Passos:**
1. Implementar Fase 1 (Lexer e Parser)
2. Implementar Fase 2 (Sistema de Macros)
3. Implementar Fase 3 (Sistema de Tipos)
4. Implementar Fase 4 (Code Generation)
5. Executar testes e validação
6. Documentar e integrar com o sistema existente

A implementação seguirá as melhores práticas de Erlang/OTP e manterá compatibilidade total com a sintaxe LX1, garantindo uma transição suave para os usuários existentes.