# Sistema de Tipos Hindley-Milner no LX2

## Visão Geral

O LX2 implementa um sistema de tipos Hindley-Milner completo, baseado na experiência bem-sucedida do LX1, mas adaptado para as capacidades nativas do Erlang e integração com o Dialyzer.

## Fundamentos Teóricos

### O que é Hindley-Milner?

O sistema de tipos Hindley-Milner é um sistema de tipos polimórfico que permite:
- **Inferência automática de tipos** sem anotações explícitas
- **Polimorfismo paramétrico** (generics)
- **Unificação de tipos** para resolver restrições
- **Generalização** de tipos para reutilização

### Componentes Principais

#### 1. Type Variables (Variáveis de Tipo)
```erlang
% Representação de variáveis de tipo
-type type_var() :: {type_var, atom()}.
-type type_const() :: {type_const, atom()}.
-type type_fun() :: {type_fun, type(), type()}.
-type type_list() :: {type_list, type()}.
-type type_tuple() :: {type_tuple, [type()]}.
-type type_map() :: {type_map, type(), type()}.
-type type_record() :: {type_record, atom(), [{atom(), type()}]}.
```

#### 2. Type Environment (Ambiente de Tipos)
```erlang
% Mapeamento de variáveis para tipos
-type type_env() :: #{atom() => type()}.
```

#### 3. Type Substitution (Substituição de Tipos)
```erlang
% Substituição de variáveis de tipo por tipos concretos
-type substitution() :: #{type_var() => type()}.
```

## Implementação no LX2

### Estrutura de Dados

#### Módulo `lx2_types.erl`

```erlang
-module(lx2_types).

%% Tipos básicos
-export([new_type_var/1, new_type_const/1, new_type_fun/2]).
-export([new_type_list/1, new_type_tuple/1, new_type_map/2]).
-export([new_type_record/2]).

%% Unificação
-export([unify/2, unify_types/2]).

%% Inferência
-export([infer_type/2, generalize/2, instantiate/2]).

%% Ambiente
-export([new_env/0, extend_env/3, lookup_env/2]).

%% Substituição
-export([new_substitution/0, compose_substitutions/2, apply_substitution/2]).
```

### Algoritmo de Inferência

#### 1. Inferência para Literais

```erlang
infer_literal(integer, _Env) ->
    {new_type_const(integer), new_substitution()};

infer_literal(float, _Env) ->
    {new_type_const(float), new_substitution()};

infer_literal(string, _Env) ->
    {new_type_const(binary), new_substitution()};

infer_literal(atom, _Env) ->
    {new_type_const(atom), new_substitution()};

infer_literal(boolean, _Env) ->
    {new_type_const(boolean), new_substitution()};

infer_literal(nil, _Env) ->
    {new_type_const(nil), new_substitution()}.
```

#### 2. Inferência para Variáveis

```erlang
infer_variable(Var, Env) ->
    case lookup_env(Var, Env) of
        {ok, Type} ->
            {instantiate(Type), new_substitution()};
        not_found ->
            NewVar = new_type_var(Var),
            {NewVar, new_substitution()}
    end.
```

#### 3. Inferência para Aplicações de Função

```erlang
infer_application(Fun, Args, Env) ->
    % Inferir tipo da função
    {FunType, S1} = infer_expression(Fun, Env),

    % Inferir tipos dos argumentos
    {ArgTypes, S2} = infer_expressions(Args, apply_substitution(Env, S1)),

    % Unificar com tipo de função esperado
    ResultType = new_type_var(result),
    ExpectedFunType = new_type_fun(ArgTypes, ResultType),

    % Unificar tipos
    {S3, _} = unify(FunType, ExpectedFunType),

    % Compor substituições
    FinalSub = compose_substitutions(S3, compose_substitutions(S2, S1)),

    {apply_substitution(ResultType, FinalSub), FinalSub}.
```

### Unificação de Tipos

#### Algoritmo de Unificação

```erlang
unify(T1, T2) when T1 =:= T2 ->
    {new_substitution(), T1};

unify({type_var, V}, T) ->
    case occurs_check(V, T) of
        true -> {error, occurs_check_failed};
        false -> {#{V => T}, T}
    end;

unify(T, {type_var, V}) ->
    unify({type_var, V}, T);

unify({type_fun, Arg1, Ret1}, {type_fun, Arg2, Ret2}) ->
    {S1, UnifiedArg} = unify(Arg1, Arg2),
    {S2, UnifiedRet} = unify(
        apply_substitution(Ret1, S1),
        apply_substitution(Ret2, S1)
    ),
    FinalSub = compose_substitutions(S2, S1),
    {FinalSub, {type_fun, UnifiedArg, UnifiedRet}};

unify({type_list, Elem1}, {type_list, Elem2}) ->
    {S, UnifiedElem} = unify(Elem1, Elem2),
    {S, {type_list, UnifiedElem}};

unify({type_tuple, Types1}, {type_tuple, Types2}) ->
    case length(Types1) =:= length(Types2) of
        true -> unify_tuple_types(Types1, Types2, new_substitution());
        false -> {error, tuple_arity_mismatch}
    end.
```

### Integração com Dialyzer

#### Geração de Specs

```erlang
generate_spec(FunName, Arity, Type) ->
    Spec = type_to_spec(Type),
    io:format("-spec ~s/~p :: ~s.~n", [FunName, Arity, Spec]).

type_to_spec({type_const, integer}) -> "integer()";
type_to_spec({type_const, float}) -> "float()";
type_to_spec({type_const, binary}) -> "binary()";
type_to_spec({type_const, atom}) -> "atom()";
type_to_spec({type_const, boolean}) -> "boolean()";
type_to_spec({type_const, nil}) -> "nil()";

type_to_spec({type_list, ElemType}) ->
    "[" ++ type_to_spec(ElemType) ++ "]";

type_to_spec({type_tuple, Types}) ->
    "{" ++ string:join([type_to_spec(T) || T <- Types], ", ") ++ "}";

type_to_spec({type_map, KeyType, ValueType}) ->
    "#{" ++ type_to_spec(KeyType) ++ " => " ++ type_to_spec(ValueType) ++ "}";

type_to_spec({type_fun, ArgType, RetType}) ->
    "(" ++ type_to_spec(ArgType) ++ ") -> " ++ type_to_spec(RetType).
```

## Exemplos de Uso

### 1. Inferência Básica

```lx
def add(a, b) do
    a + b
end
```

**Inferência**:
- `a`: `{type_var, 'a'}`
- `b`: `{type_var, 'b'}`
- `+`: `{type_fun, {type_tuple, [{type_var, 'a'}, {type_var, 'b'}]}, {type_var, 'result'}}`
- **Unificação**: `{type_var, 'a'}` = `{type_var, 'b'}` = `{type_const, integer}`
- **Resultado**: `(integer(), integer()) -> integer()`

### 2. Polimorfismo

```lx
def identity(x) do
    x
end
```

**Inferência**:
- `x`: `{type_var, 'a'}`
- **Resultado**: `(A) -> A` (polimórfico)

### 3. Listas

```lx
def head(list) do
    [first | rest] = list
    first
end
```

**Inferência**:
- `list`: `{type_list, {type_var, 'elem'}}`
- `first`: `{type_var, 'elem'}`
- **Resultado**: `([A]) -> A`

### 4. Records

```lx
record Person { name :: string, age :: integer }

def get_age(person) do
    person.age
end
```

**Inferência**:
- `person`: `{type_record, 'Person', [{'name', {type_const, binary}}, {'age', {type_const, integer}}]}`
- **Resultado**: `(#person{}) -> integer()`

## Tratamento de Erros

### Tipos de Erros

#### 1. Type Mismatch
```erlang
type_error(Type1, Type2, Position) ->
    {error, {type_mismatch, Type1, Type2, Position}}.
```

#### 2. Undefined Variable
```erlang
undefined_variable(Var, Position) ->
    {error, {undefined_variable, Var, Position}}.
```

#### 3. Occurs Check
```erlang
occurs_check_failed(Var, Type, Position) ->
    {error, {occurs_check_failed, Var, Type, Position}}.
```

### Mensagens de Erro

```erlang
format_type_error({type_mismatch, T1, T2, Pos}) ->
    io:format("Type error at ~p: expected ~s, got ~s~n",
              [Pos, type_to_string(T1), type_to_string(T2)]);

format_type_error({undefined_variable, Var, Pos}) ->
    io:format("Undefined variable ~s at ~p~n", [Var, Pos]).
```

## Otimizações

### 1. Type Caching

```erlang
-type type_cache() :: #{expression() => type()}.

infer_with_cache(Expr, Env, Cache) ->
    case maps:get(Expr, Cache, not_found) of
        not_found ->
            {Type, Sub} = infer_expression(Expr, Env),
            {Type, Sub, Cache#{Expr => Type}};
        CachedType ->
            {CachedType, new_substitution(), Cache}
    end.
```

### 2. Lazy Type Variables

```erlang
new_lazy_type_var() ->
    {type_var, make_ref()}.
```

### 3. Type Normalization

```erlang
normalize_type(Type) ->
    case Type of
        {type_fun, {type_fun, A1, R1}, R2} ->
            normalize_type({type_fun, A1, {type_fun, R1, R2}});
        _ -> Type
    end.
```

## Integração com o Compilador

### Pipeline de Tipos

```erlang
type_check_pipeline(AST, Env) ->
    % Fase 1: Inferência de tipos
    {TypedAST, Sub} = infer_ast(AST, Env),

    % Fase 2: Verificação de erros
    case check_type_errors(TypedAST) of
        [] ->
            % Fase 3: Generalização
            GeneralizedAST = generalize_ast(TypedAST, Sub),

            % Fase 4: Geração de specs
            Specs = generate_specs(GeneralizedAST),

            {ok, GeneralizedAST, Specs};
        Errors ->
            {error, Errors}
    end.
```

### Integração com Parser

```erlang
parse_and_type_check(Source) ->
    case lx2_parser:parse(Source) of
        {ok, AST} ->
            case lx2_types:type_check_pipeline(AST, new_env()) of
                {ok, TypedAST, Specs} ->
                    {ok, TypedAST, Specs};
                {error, TypeErrors} ->
                    {error, TypeErrors}
            end;
        {error, ParseErrors} ->
            {error, ParseErrors}
    end.
```

## Testes

### Testes Unitários

```erlang
unify_test() ->
    T1 = new_type_var('a'),
    T2 = new_type_const(integer),
    {Sub, Unified} = unify(T1, T2),
    ?assertEqual(T2, Unified),
    ?assertEqual(#{'a' => T2}, Sub).

inference_test() ->
    Env = new_env(),
    {Type, _Sub} = infer_literal(integer, Env),
    ?assertEqual(new_type_const(integer), Type).
```

### Testes de Integração

```erlang
integration_test() ->
    Source = "def add(a, b) do a + b end",
    {ok, TypedAST, Specs} = parse_and_type_check(Source),
    ?assertMatch([{add, 2, "(integer(), integer()) -> integer()"}], Specs).
```

## Conclusão

O sistema de tipos Hindley-Milner no LX2 oferece:

1. **Inferência automática** de tipos sem anotações explícitas
2. **Polimorfismo** para reutilização de código
3. **Integração nativa** com o Dialyzer do Erlang
4. **Mensagens de erro** claras e informativas
5. **Performance otimizada** com caching e normalização
6. **Compatibilidade** com o sistema LX1 existente

Este sistema forma a base para um compilador robusto e type-safe, garantindo que programas LX sejam corretos em tempo de compilação.