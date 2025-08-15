# Arquitetura do LX2

## Visão Geral

O LX2 é uma reescrita completa do compilador LX usando Erlang/OTP e ferramentas nativas como Yacc (yecc) e Lex (leex). Esta arquitetura aproveita as capacidades nativas do Erlang para criar um compilador robusto, escalável e manutenível.

## Arquitetura de Alto Nível

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Source Code   │    │   Lexer (leex)  │    │  Parser (yecc)  │
│     (.lx)       │───▶│   Tokens        │───▶│     AST         │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                       │
                                                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  Erlang Code    │    │   Codegen       │    │  Type System    │
│     (.erl)      │◀───│   (AST → Erlang)│◀───│  (Hindley-Milner)│
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Componentes Principais

### 1. Lexer (leex)

#### Responsabilidades
- Tokenização do código fonte
- Reconhecimento de literais, identificadores e operadores
- Tratamento de comentários e whitespace
- Geração de tokens com posição e valor

#### Estrutura
```erlang
-module(lx2_lexer).

%% Tokens básicos
-export([tokenize/1]).

%% Tipos de tokens
-type token() :: {
    token_type(),
    line() :: integer(),
    column() :: integer(),
    value() :: term()
}.

-type token_type() ::
    integer | float | string | atom | identifier |
    'def' | 'do' | 'end' | 'record' | 'module' |
    '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' |
    '(' | ')' | '[' | ']' | '{' | '}' | ':' | ';' | ','.
```

#### Exemplo de Uso
```erlang
%% Tokenização de código LX
Source = "def add(a, b) do a + b end",
{ok, Tokens, _} = lx2_lexer:tokenize(Source).
```

### 2. Parser (yecc)

#### Responsabilidades
- Análise sintática baseada em gramática Yacc
- Construção da AST (Abstract Syntax Tree)
- Validação de estrutura sintática
- Tratamento de erros de parsing

#### Estrutura da AST
```erlang
-type ast_node() ::
    {function_def, identifier(), [parameter()], block()} |
    {multi_head_function, identifier(), [function_head()]} |
    {binary_op, operator(), ast_node(), ast_node()} |
    {list, [ast_node()]} |
    {tuple, [ast_node()]} |
    {map, [map_entry()]} |
    {record_def, identifier(), [record_field()]} |
    {record_literal, identifier(), [field_assignment()]} |
    {record_access, ast_node(), identifier()} |
    {function_call, identifier(), [ast_node()]} |
    {directive_call, directive_type(), ast_node()} |
    {literal, literal_type(), value()}.
```

#### Exemplo de Uso
```erlang
%% Parsing de código LX
{ok, AST} = lx2_parser:parse(Tokens).
```

### 3. Sistema de Tipos (Hindley-Milner)

#### Responsabilidades
- Inferência automática de tipos
- Verificação de tipos em tempo de compilação
- Unificação de tipos
- Geração de specs Erlang

#### Estrutura
```erlang
-module(lx2_types).

%% Tipos básicos
-type type() ::
    {type_var, atom()} |
    {type_const, atom()} |
    {type_fun, type(), type()} |
    {type_list, type()} |
    {type_tuple, [type()]} |
    {type_map, type(), type()} |
    {type_record, atom(), [{atom(), type()}]}.

%% Ambiente de tipos
-type type_env() :: #{atom() => type()}.

%% Substituição de tipos
-type substitution() :: #{type_var() => type()}.
```

#### Exemplo de Uso
```erlang
%% Inferência de tipos
Env = lx2_types:new_env(),
{TypedAST, Specs} = lx2_types:infer_types(AST, Env).
```

### 4. Gerador de Código

#### Responsabilidades
- Transformação da AST tipada em código Erlang
- Compilação direta para bytecode BEAM
- Geração opcional de arquivos `.erl` para debugging
- Geração de specs baseadas no sistema de tipos
- Otimizações de código
- Formatação do código gerado

#### Estrutura
```erlang
-module(lx2_codegen).

%% Geração de código
-export([generate/1, generate_module/2, compile_direct/1, generate_erl/1]).

%% Tipos de saída
-type generated_code() :: {
    module_name() :: atom(),
    specs() :: [spec()],
    functions() :: [function()],
    records() :: [record_def()]
}.

-type compilation_result() :: {
    module_name() :: atom(),
    beam_code() :: binary(),
    debug_info() :: map()
}.
```

#### Exemplo de Uso
```erlang
%% Compilação direta para BEAM (recomendado)
{ModuleName, BeamCode, DebugInfo} = lx2_codegen:compile_direct(TypedAST).

%% Geração de arquivo .erl (para debugging)
{ok, ErlCode} = lx2_codegen:generate_erl(TypedAST).

%% Geração tradicional (para compatibilidade)
{ModuleName, Specs, Functions, Records} = lx2_codegen:generate(TypedAST).
```

## Pipeline de Compilação

### 1. Fase de Análise Léxica

```erlang
lexical_analysis(Source) ->
    case lx2_lexer:tokenize(Source) of
        {ok, Tokens, _EndLine} ->
            {ok, Tokens};
        {error, {Line, leex, {illegal, Char}}, _EndLine} ->
            {error, {lexical_error, Line, "illegal character: " ++ [Char]}};
        {error, {Line, leex, {user, Error}}, _EndLine} ->
            {error, {lexical_error, Line, Error}}
    end.
```

### 2. Fase de Análise Sintática

```erlang
syntactic_analysis(Tokens) ->
    case lx2_parser:parse(Tokens) of
        {ok, AST} ->
            {ok, AST};
        {error, {Line, yecc, {syntax_error, Message}}} ->
            {error, {syntax_error, Line, Message}};
        {error, {Line, yecc, {user, Error}}} ->
            {error, {syntax_error, Line, Error}}
    end.
```

### 3. Fase de Análise Semântica

```erlang
semantic_analysis(AST) ->
    Env = lx2_types:new_env(),
    case lx2_types:type_check_pipeline(AST, Env) of
        {ok, TypedAST, Specs} ->
            {ok, TypedAST, Specs};
        {error, TypeErrors} ->
            {error, {type_errors, TypeErrors}}
    end.
```

### 4. Fase de Geração de Código

```erlang
code_generation(TypedAST, Specs, Options) ->
    case maps:get(mode, Options, direct) of
        direct ->
            % Compilação direta para BEAM (padrão)
            case lx2_codegen:compile_direct(TypedAST) of
                {ModuleName, BeamCode, DebugInfo} ->
                    {ok, ModuleName, BeamCode, DebugInfo};
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end;
        erl ->
            % Geração de arquivo .erl
            case lx2_codegen:generate_erl(TypedAST) of
                {ok, ErlCode} ->
                    {ok, ErlCode};
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end;
        both ->
            % Ambos: BEAM + arquivo .erl para debugging
            case lx2_codegen:compile_direct(TypedAST) of
                {ModuleName, BeamCode, DebugInfo} ->
                    case lx2_codegen:generate_erl(TypedAST) of
                        {ok, ErlCode} ->
                            {ok, ModuleName, BeamCode, ErlCode, DebugInfo};
                        {error, ErlError} ->
                            {error, {erl_generation_error, ErlError}}
                    end;
                {error, CodegenError} ->
                    {error, {codegen_error, CodegenError}}
            end
    end.
```

### 5. Pipeline Completo

```erlang
compile(Source, Options) ->
    % Fase 1: Análise léxica
    case lexical_analysis(Source) of
        {ok, Tokens} ->
            % Fase 2: Análise sintática
            case syntactic_analysis(Tokens) of
                {ok, AST} ->
                    % Fase 3: Análise semântica
                    case semantic_analysis(AST) of
                        {ok, TypedAST, Specs} ->
                            % Fase 4: Geração de código
                            case code_generation(TypedAST, Specs, Options) of
                                {ok, ModuleName, BeamCode, DebugInfo} ->
                                    % Compilação direta para BEAM
                                    {ok, ModuleName, BeamCode, DebugInfo};
                                {ok, ErlCode} ->
                                    % Geração de arquivo .erl
                                    {ok, ErlCode};
                                {ok, ModuleName, BeamCode, ErlCode, DebugInfo} ->
                                    % Ambos: BEAM + .erl
                                    {ok, ModuleName, BeamCode, ErlCode, DebugInfo};
                                {error, CodegenError} ->
                                    {error, CodegenError}
                            end;
                        {error, TypeErrors} ->
                            {error, TypeErrors}
                    end;
                {error, SyntaxError} ->
                    {error, SyntaxError}
            end;
        {error, LexicalError} ->
            {error, LexicalError}
    end.

%% Conveniência: compilação direta para BEAM (padrão)
compile(Source) ->
    compile(Source, #{mode => direct}).
```

## Estrutura de Módulos

### Organização dos Módulos

```
lx2/
├── src/
│   ├── lx2.erl              # Módulo principal
│   ├── lx2_lexer.erl        # Analisador léxico
│   ├── lx2_parser.erl       # Analisador sintático (gerado)
│   ├── lx2_ast.erl          # Estruturas AST
│   ├── lx2_types.erl        # Sistema de tipos
│   ├── lx2_codegen.erl      # Gerador de código
│   ├── lx2_utils.erl        # Utilitários
│   ├── lx2_errors.erl       # Tratamento de erros
│   └── lx2_optimizer.erl    # Otimizações
├── include/
│   └── lx2.hrl              # Definições comuns
├── leex/
│   └── lx2_lexer.xrl        # Definição do lexer
├── yecc/
│   └── lx2_parser.yrl       # Definição da gramática
└── test/
    ├── lx2_lexer_tests.erl  # Testes do lexer
    ├── lx2_parser_tests.erl # Testes do parser
    ├── lx2_types_tests.erl  # Testes do sistema de tipos
    └── lx2_codegen_tests.erl # Testes do gerador
```

### Dependências entre Módulos

```erlang
%% Dependências principais
lx2.erl
├── lx2_lexer.erl
├── lx2_parser.erl
├── lx2_types.erl
├── lx2_codegen.erl
├── lx2_errors.erl
└── lx2_utils.erl

lx2_parser.erl
├── lx2_ast.erl
└── lx2_errors.erl

lx2_types.erl
├── lx2_ast.erl
└── lx2_errors.erl

lx2_codegen.erl
├── lx2_ast.erl
├── lx2_types.erl
└── lx2_optimizer.erl
```

## Tratamento de Erros

### Hierarquia de Erros

```erlang
-type compilation_error() ::
    {lexical_error, line(), message()} |
    {syntax_error, line(), message()} |
    {type_error, line(), type(), type()} |
    {semantic_error, line(), message()} |
    {codegen_error, message()}.

-type error_context() :: {
    file() :: string(),
    line() :: integer(),
    column() :: integer(),
    context() :: string()
}.
```

### Sistema de Relatórios de Erro

```erlang
report_error({lexical_error, Line, Message}, Source) ->
    Context = extract_context(Source, Line),
    io:format("Lexical error at line ~p:~n~s~n~s~n", [Line, Context, Message]);

report_error({syntax_error, Line, Message}, Source) ->
    Context = extract_context(Source, Line),
    io:format("Syntax error at line ~p:~n~s~n~s~n", [Line, Context, Message]);

report_error({type_error, Line, Expected, Got}, Source) ->
    Context = extract_context(Source, Line),
    io:format("Type error at line ~p:~n~s~nExpected: ~s~nGot: ~s~n",
              [Line, Context, type_to_string(Expected), type_to_string(Got)]).
```

## Otimizações

### 1. Otimizações de Parsing

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

### 2. Otimizações de Tipo

```erlang
%% Cache de inferência de tipos
-type type_cache() :: #{ast_node() => type()}.

infer_with_cache(AST, Env, Cache) ->
    case maps:get(AST, Cache, not_found) of
        not_found ->
            {Type, Sub} = infer_type(AST, Env),
            {Type, Sub, Cache#{AST => Type}};
        CachedType ->
            {CachedType, new_substitution(), Cache}
    end.
```

### 3. Otimizações de Código

```erlang
%% Otimizações de código gerado
optimize_code(AST) ->
    AST1 = constant_folding(AST),
    AST2 = dead_code_elimination(AST1),
    AST3 = function_inlining(AST2),
    AST3.

constant_folding({binary_op, '+', {integer, A}, {integer, B}}) ->
    {integer, A + B};
constant_folding(Node) ->
    Node.

%% Compilação direta para BEAM com otimizações
compile_to_beam(AST, ModuleName) ->
    % Gerar código Erlang interno
    ErlCode = ast_to_erlang_code(AST),

    % Compilar diretamente para BEAM
    case compile:forms(ErlCode, [return, binary, debug_info]) of
        {ok, ModuleName, BeamCode} ->
            {ok, ModuleName, BeamCode};
        {ok, ModuleName, BeamCode, Warnings} ->
            {ok, ModuleName, BeamCode, Warnings};
        {error, Errors, Warnings} ->
            {error, Errors, Warnings}
    end.

%% Geração de arquivo .erl para debugging
generate_erl_file(AST, ModuleName, Filename) ->
    ErlCode = ast_to_erlang_code(AST),
    FormattedCode = format_erlang_code(ErlCode),
    file:write_file(Filename, FormattedCode).
```

## Concorrência e Escalabilidade

### 1. Parsing Paralelo

```erlang
%% Parsing paralelo para múltiplos arquivos
parse_files(Files) ->
    Tasks = [{File, fun() -> parse_file(File) end} || File <- Files],
    Results = parallel_map(Tasks),
    process_results(Results).

parallel_map(Tasks) ->
    Parent = self(),
    Pids = [spawn(fun() ->
        Result = Task(),
        Parent ! {self(), Result}
    end) || {_, Task} <- Tasks],
    [receive {Pid, Result} -> Result end || Pid <- Pids].
```

### 2. Análise de Tipos Paralela

```erlang
%% Análise de tipos paralela para funções independentes
parallel_type_check(AST) ->
    Functions = extract_functions(AST),
    IndependentFunctions = find_independent_functions(Functions),
    Tasks = [{Fun, fun() -> type_check_function(Fun) end}
             || Fun <- IndependentFunctions],
    Results = parallel_map(Tasks),
    merge_type_results(Results).
```

## Integração com Ferramentas Erlang

### 1. Dialyzer

```erlang
%% Integração com Dialyzer para análise estática
dialyzer_integration(GeneratedCode) ->
    % Gerar specs compatíveis com Dialyzer
    Specs = generate_dialyzer_specs(GeneratedCode),

    % Executar análise Dialyzer
    case dialyzer:run([{files, [GeneratedCode]}, {defines, []}]) of
        {ok, []} ->
            {ok, "No Dialyzer warnings"};
        {ok, Warnings} ->
            {warning, format_dialyzer_warnings(Warnings)};
        {error, Error} ->
            {error, Error}
    end.
```

### 2. EUnit e Common Test

```erlang
%% Testes automatizados
-module(lx2_tests).

-include_lib("eunit/include/eunit.hrl").

%% Testes unitários
basic_compilation_test() ->
    Source = "def add(a, b) do a + b end",
    {ok, GeneratedCode} = lx2:compile(Source),
    ?assert(is_binary(GeneratedCode)).

%% Testes de propriedades
prop_type_inference() ->
    ?FORALL(Source, valid_lx_source(),
        begin
            {ok, _} = lx2:compile(Source),
            true
        end).
```

### 3. Observer e Debugging

```erlang
%% Integração com Observer para debugging
debug_compilation(Source) ->
    % Habilitar tracing
    dbg:tracer(),
    dbg:p(all, [call, return]),
    dbg:tpl(lx2, []),

    % Executar compilação
    Result = lx2:compile(Source),

    % Desabilitar tracing
    dbg:stop(),

    Result.
```

## Compilação Direta para BEAM

### Vantagens da Abordagem Híbrida

#### 1. **Compilação Direta (Padrão)**
```erlang
%% Compilação direta para BEAM - mais eficiente
{ok, ModuleName, BeamCode, DebugInfo} = lx2:compile(Source).

%% Carregar módulo diretamente na VM
code:load_binary(ModuleName, "", BeamCode).
```

**Vantagens:**
- **Performance**: Sem I/O de arquivos intermediários
- **Segurança**: Não expõe código fonte gerado
- **Eficiência**: Compilação em memória
- **Integração**: Carregamento direto na VM

#### 2. **Geração de .erl (Debugging)**
```erlang
%% Geração de arquivo .erl para debugging
{ok, ErlCode} = lx2:compile(Source, #{mode => erl}).

%% Salvar arquivo .erl
file:write_file("debug_module.erl", ErlCode).
```

**Vantagens:**
- **Debugging**: Inspeção do código gerado
- **Análise**: Verificação manual do output
- **Compatibilidade**: Compatível com ferramentas externas
- **Documentação**: Código fonte para referência

#### 3. **Modo Híbrido (Ambos)**
```erlang
%% Compilação + geração de .erl
{ok, ModuleName, BeamCode, ErlCode, DebugInfo} =
    lx2:compile(Source, #{mode => both}).

%% Carregar BEAM e salvar .erl
code:load_binary(ModuleName, "", BeamCode),
file:write_file(ModuleName ++ ".erl", ErlCode).
```

**Vantagens:**
- **Flexibilidade**: Melhor dos dois mundos
- **Desenvolvimento**: Debugging durante desenvolvimento
- **Produção**: Performance otimizada
- **Transparência**: Código fonte disponível quando necessário

### Implementação Técnica

#### Compilação Direta
```erlang
compile_direct(AST) ->
    % Gerar formas Erlang internas
    Forms = ast_to_forms(AST),

    % Compilar diretamente para BEAM
    case compile:forms(Forms, [return, binary, debug_info]) of
        {ok, ModuleName, BeamCode} ->
            {ModuleName, BeamCode, #{}};
        {ok, ModuleName, BeamCode, Warnings} ->
            {ModuleName, BeamCode, #{warnings => Warnings}};
        {error, Errors, Warnings} ->
            {error, {compilation_errors, Errors, Warnings}}
    end.
```

#### Geração de .erl
```erlang
generate_erl(AST) ->
    % Gerar código Erlang formatado
    ErlCode = ast_to_erlang_source(AST),
    {ok, ErlCode}.
```

### Casos de Uso

#### Desenvolvimento
```erlang
%% Durante desenvolvimento: modo híbrido
{ok, ModuleName, BeamCode, ErlCode, DebugInfo} =
    lx2:compile(Source, #{mode => both, debug => true}).

%% Carregar e executar
code:load_binary(ModuleName, "", BeamCode),
ModuleName:main().
```

#### Produção
```erlang
%% Em produção: apenas BEAM
{ok, ModuleName, BeamCode, DebugInfo} =
    lx2:compile(Source, #{mode => direct, optimize => true}).

%% Carregar sem expor código fonte
code:load_binary(ModuleName, "", BeamCode).
```

#### Debugging
```erlang
%% Para debugging: apenas .erl
{ok, ErlCode} = lx2:compile(Source, #{mode => erl, format => pretty}).

%% Salvar e analisar
file:write_file("debug.erl", ErlCode),
io:format("Generated code:~n~s~n", [ErlCode]).
```

## Conclusão

A arquitetura do LX2 oferece:

1. **Modularidade**: Componentes bem definidos e independentes
2. **Escalabilidade**: Suporte a parsing e análise paralelos
3. **Robustez**: Tratamento abrangente de erros
4. **Performance**: Otimizações em todas as fases
5. **Integração**: Compatibilidade total com ecossistema Erlang
6. **Manutenibilidade**: Código limpo e bem documentado
7. **Testabilidade**: Testes abrangentes e automatizados
8. **Flexibilidade**: Compilação direta para BEAM + geração opcional de .erl

Esta arquitetura forma a base para um compilador LX2 robusto, eficiente e pronto para produção, oferecendo o melhor dos dois mundos: performance máxima com transparência quando necessário.