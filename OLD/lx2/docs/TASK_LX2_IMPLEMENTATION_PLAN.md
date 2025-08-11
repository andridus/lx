# Task LX2: Implementação Completa em Erlang

## Visão Geral

Este documento define o plano completo para implementar todas as funcionalidades do LX1 no LX2 usando Erlang/OTP, Yacc (yecc) e Lex (leex). O objetivo é reimplementar com sucesso todas as 9 tasks do LX1, aproveitando as capacidades nativas do Erlang para criar um compilador mais robusto e eficiente.

## Status do Projeto

**Status Atual**: Planejamento e Estruturação
**Objetivo**: Implementar todas as funcionalidades do LX1 no LX2
**Tecnologia**: Erlang/OTP + Yacc (yecc) + Lex (leex)
**Prazo Estimado**: 12-16 semanas

## Arquitetura de Alto Nível

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Source Code   │    │   Lexer (leex)  │    │  Parser (yecc)  │
│     (.lx)       │───▶│   Tokens        │───▶│     AST         │
└─────────────────┘    └─────────────────┘    └─────────────────┘
                                                       │
                                                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│  BEAM Code      │    │   Codegen       │    │  Type System    │
│   (Direct)      │◀───│   (AST → BEAM)  │◀───│  (Hindley-Milner)│
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Estrutura do Projeto

```
lx2/
├── src/
│   ├── lx2.erl                 # Módulo principal
│   ├── lx2_lexer.erl           # Analisador léxico (gerado)
│   ├── lx2_parser.erl          # Analisador sintático (gerado)
│   ├── lx2_ast.erl             # Estruturas AST
│   ├── lx2_types.erl           # Sistema de tipos Hindley-Milner
│   ├── lx2_codegen.erl         # Gerador de código
│   ├── lx2_errors.erl          # Tratamento de erros
│   ├── lx2_utils.erl           # Utilitários
│   └── lx2_optimizer.erl       # Otimizações
├── leex/
│   └── lx2_lexer.xrl           # Definição do lexer
├── yecc/
│   └── lx2_parser.yrl          # Definição da gramática
├── include/
│   └── lx2.hrl                 # Definições comuns
├── test/
│   ├── lx2_lexer_tests.erl     # Testes do lexer
│   ├── lx2_parser_tests.erl    # Testes do parser
│   ├── lx2_types_tests.erl     # Testes do sistema de tipos
│   ├── lx2_codegen_tests.erl   # Testes do gerador
│   └── integration_tests.erl   # Testes de integração
├── examples/
│   ├── task_01/                # Exemplos Task 1
│   ├── task_02/                # Exemplos Task 2
│   ├── task_03/                # Exemplos Task 3
│   ├── task_04/                # Exemplos Task 4
│   ├── task_05/                # Exemplos Task 5
│   ├── task_06/                # Exemplos Task 6
│   ├── task_07/                # Exemplos Task 7
│   ├── task_08/                # Exemplos Task 8
│   └── task_09/                # Exemplos Task 9
├── rebar.config                # Configuração Rebar3
├── Makefile                    # Makefile para build
└── README.md                   # Documentação principal
```

## Fases de Implementação

### Fase 1: Fundação (Semanas 1-2)

#### 1.1 Configuração do Ambiente
- [ ] Setup do projeto Erlang/OTP
- [ ] Configuração do Rebar3
- [ ] Estrutura de diretórios
- [ ] Makefile e scripts de build
- [ ] Configuração de testes (EUnit + Common Test)
- [ ] Configuração do Dialyzer

#### 1.2 Lexer Básico (leex)
- [ ] Definição de tokens básicos em `leex/lx2_lexer.xrl`
- [ ] Suporte a literais: integers, floats, strings, atoms, booleans, nil
- [ ] Reconhecimento de identificadores
- [ ] Tratamento de comentários (`#`)
- [ ] Tratamento de whitespace e newlines
- [ ] Tokens para operadores e pontuação básica

#### 1.3 Parser Básico (yecc)
- [ ] Gramática básica em `yecc/lx2_parser.yrl`
- [ ] Gramática para literais
- [ ] Gramática para funções simples: `def name() do ... end`
- [ ] Estrutura AST básica em `src/lx2_ast.erl`

#### 1.4 Sistema de Tipos Básico
- [ ] Estrutura de tipos básicos em `src/lx2_types.erl`
- [ ] Type variables e type constants
- [ ] Type environment básico
- [ ] Inferência para literais

#### 1.5 Gerador de Código Básico
- [ ] Geração de código Erlang básico
- [ ] Compilação direta para BEAM
- [ ] Geração de specs básicas

### Fase 2: Task 1 - Functions with Literals (Semana 3)

#### 2.1 Extensão do Lexer
```erlang
% leex/lx2_lexer.xrl
Definitions.
D = [0-9]
L = [a-zA-Z_]
WS = [\s\t]
NL = \n|\r\n|\r

Rules.
{D}+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+ : {token, {float, TokenLine, list_to_float(TokenChars)}}.
\"[^\"]*\" : {token, {string, TokenLine, strip_quotes(TokenChars)}}.
:({L}({L}|{D})*) : {token, {atom, TokenLine, strip_colon(TokenChars)}}.
true : {token, {boolean, TokenLine, true}}.
false : {token, {boolean, TokenLine, false}}.
nil : {token, {nil, TokenLine, nil}}.
def : {token, {def, TokenLine}}.
do : {token, {do, TokenLine}}.
end : {token, {end, TokenLine}}.
{L}({L}|{D})* : {token, {identifier, TokenLine, TokenChars}}.
{WS}+ : skip_token.
{NL} : {token, {newline, TokenLine}}.
#.* : skip_token.
```

#### 2.2 Extensão do Parser
```erlang
% yecc/lx2_parser.yrl
Nonterminals
program function_def block expression literal.

Terminals
def do end integer float string atom boolean nil identifier newline.

Rootsymbol program.

program -> function_def : ['$1'].
program -> function_def program : ['$1' | '$2'].

function_def -> def identifier '(' ')' do block end :
    {function_def, '$2', [], '$6'}.

block -> expression : ['$1'].
block -> expression newline block : ['$1' | '$3'].

expression -> literal : '$1'.

literal -> integer : {literal, integer, '$1'}.
literal -> float : {literal, float, '$1'}.
literal -> string : {literal, string, '$1'}.
literal -> atom : {literal, atom, '$1'}.
literal -> boolean : {literal, boolean, '$1'}.
literal -> nil : {literal, nil, '$1'}.
```

#### 2.3 Sistema de Tipos para Task 1
```erlang
% src/lx2_types.erl
infer_literal({literal, Type, Value}, _Env) ->
    case Type of
        integer -> {new_type_const(integer), new_substitution()};
        float -> {new_type_const(float), new_substitution()};
        string -> {new_type_const(binary), new_substitution()};
        atom -> {new_type_const(atom), new_substitution()};
        boolean -> {new_type_const(boolean), new_substitution()};
        nil -> {new_type_const(nil), new_substitution()}
    end.
```

#### 2.4 Gerador de Código para Task 1
```erlang
% src/lx2_codegen.erl
generate_literal({literal, Type, Value}) ->
    case Type of
        integer -> integer_to_list(Value);
        float -> float_to_list(Value);
        string -> "<<" ++ Value ++ "/utf8>>";
        atom -> atom_to_list(Value);
        boolean -> atom_to_list(Value);
        nil -> "nil"
    end.

generate_function({function_def, Name, Params, Body}) ->
    ErlName = atom_to_list(Name),
    ErlBody = generate_block(Body),
    ErlName ++ "() ->\n    " ++ ErlBody ++ ".\n".
```

### Fase 3: Task 2 - Variables and Local Bindings (Semana 4)

#### 3.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
= : {token, {equals, TokenLine}}.
; : {token, {semicolon, TokenLine}}.
```

#### 3.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
equals semicolon.

Nonterminals
variable_binding variable_ref.

block -> expression : ['$1'].
block -> expression newline block : ['$1' | '$3'].
block -> variable_binding newline block : ['$1' | '$3'].
block -> variable_binding semicolon block : ['$1' | '$3'].

variable_binding -> identifier equals expression :
    {variable_binding, '$1', '$3'}.

expression -> identifier : {variable_ref, '$1'}.
```

#### 3.3 Sistema de Tipos para Task 2
```erlang
% src/lx2_types.erl
infer_variable_binding({variable_binding, Var, Expr}, Env) ->
    {ExprType, Sub1} = infer_expression(Expr, Env),
    NewEnv = extend_env(Var, ExprType, Env),
    {ExprType, Sub1, NewEnv}.

infer_variable_ref({variable_ref, Var}, Env) ->
    case lookup_env(Var, Env) of
        {ok, Type} -> {Type, new_substitution()};
        not_found -> {error, {undefined_variable, Var}}
    end.
```

#### 3.4 Gerador de Código para Task 2
```erlang
% src/lx2_codegen.erl
generate_variable_binding({variable_binding, Var, Expr}, VarCounter) ->
    ErlVar = capitalize_var(Var, VarCounter),
    ErlExpr = generate_expression(Expr),
    ErlVar ++ " = " ++ ErlExpr.

generate_variable_ref({variable_ref, Var}, VarMap) ->
    case maps:get(Var, VarMap, not_found) of
        not_found -> {error, {undefined_variable, Var}};
        ErlVar -> ErlVar
    end.
```

### Fase 4: Task 3 - Binary Operators (Semana 5)

#### 4.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
\+ : {token, {plus, TokenLine}}.
- : {token, {minus, TokenLine}}.
\* : {token, {asterisk, TokenLine}}.
\/ : {token, {slash, TokenLine}}.
== : {token, {eq, TokenLine}}.
!= : {token, {ne, TokenLine}}.
< : {token, {lt, TokenLine}}.
<= : {token, {le, TokenLine}}.
> : {token, {gt, TokenLine}}.
>= : {token, {ge, TokenLine}}.
and : {token, {and, TokenLine}}.
or : {token, {or, TokenLine}}.
&&& : {token, {band, TokenLine}}.
\|\|\| : {token, {bor, TokenLine}}.
\^\^\^ : {token, {bxor, TokenLine}}.
<<< : {token, {bsl, TokenLine}}.
>>> : {token, {bsr, TokenLine}}.
\( : {token, {lparen, TokenLine}}.
\) : {token, {rparen, TokenLine}}.
, : {token, {comma, TokenLine}}.
```

#### 4.2 Extensão do Parser (Pratt Parser)
```erlang
% Implementar parser de precedência em src/lx2_parser.erl
parse_expression(Tokens) ->
    parse_expression_with_precedence(Tokens, 0).

parse_expression_with_precedence([Token | Rest], MinPrec) ->
    {Left, Rest1} = parse_prefix(Token, Rest),
    parse_infix(Left, Rest1, MinPrec).

parse_infix(Left, [Token | Rest], MinPrec) ->
    case get_operator_precedence(Token) of
        Prec when Prec >= MinPrec ->
            {Right, Rest1} = parse_expression_with_precedence(Rest, Prec + 1),
            NewLeft = {binary_op, Token, Left, Right},
            parse_infix(NewLeft, Rest1, MinPrec);
        _ ->
            {Left, [Token | Rest]}
    end;
parse_infix(Left, [], _MinPrec) ->
    {Left, []}.
```

#### 4.3 Sistema de Kernel
```erlang
% src/lx2_kernel.erl
-define(OPERATORS, #{
    '+' => #{precedence => 1, associativity => left, erlang_op => '+'},
    '-' => #{precedence => 1, associativity => left, erlang_op => '-'},
    '*' => #{precedence => 2, associativity => left, erlang_op => '*'},
    '/' => #{precedence => 2, associativity => left, erlang_op => '/'},
    '==' => #{precedence => 3, associativity => left, erlang_op => '=='},
    '!=' => #{precedence => 3, associativity => left, erlang_op => '/='},
    '<' => #{precedence => 3, associativity => left, erlang_op => '<'},
    '<=' => #{precedence => 3, associativity => left, erlang_op => '<='},
    '>' => #{precedence => 3, associativity => left, erlang_op => '>'},
    '>=' => #{precedence => 3, associativity => left, erlang_op => '>='},
    'and' => #{precedence => 4, associativity => left, erlang_op => 'andalso'},
    'or' => #{precedence => 4, associativity => left, erlang_op => 'orelse'},
    '&&&' => #{precedence => 5, associativity => left, erlang_op => 'band'},
    '|||' => #{precedence => 5, associativity => left, erlang_op => 'bor'},
    '^^^' => #{precedence => 5, associativity => left, erlang_op => 'bxor'},
    '<<<' => #{precedence => 6, associativity => left, erlang_op => 'bsl'},
    '>>>' => #{precedence => 6, associativity => left, erlang_op => 'bsr'}
}).
```

#### 4.4 Sistema de Tipos para Task 3
```erlang
% src/lx2_types.erl
infer_binary_op({binary_op, Op, Left, Right}, Env) ->
    {LeftType, Sub1} = infer_expression(Left, Env),
    {RightType, Sub2} = infer_expression(Right, apply_substitution(Env, Sub1)),

    % Verificar tipos compatíveis
    case check_operator_types(Op, LeftType, RightType) of
        {ok, ResultType} ->
            FinalSub = compose_substitutions(Sub2, Sub1),
            {ResultType, FinalSub};
        {error, Error} ->
            {error, Error}
    end.

check_operator_types('+', {type_const, integer}, {type_const, integer}) ->
    {ok, {type_const, integer}};
check_operator_types('+', {type_const, float}, {type_const, float}) ->
    {ok, {type_const, float}};
check_operator_types('+', {type_const, integer}, {type_const, float}) ->
    {error, {type_mismatch, integer, float}};
% ... outros operadores
```

### Fase 5: Task 4 - Directives (Semana 6)

#### 5.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
\$ : {token, {dollar, TokenLine}}.
```

#### 5.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
dollar.

Nonterminals
directive_call.

expression -> directive_call : '$1'.

directive_call -> dollar identifier '(' expression ')' :
    {directive_call, '$2', '$4'}.
```

#### 5.3 Sistema de Diretivas
```erlang
% src/lx2_directives.erl
-define(DIRECTIVES, #{
    '$print' => #{arity => 1, handler => fun print_directive/2},
    '$type' => #{arity => 1, handler => fun type_directive/2}
}).

print_directive(AST, _Env) ->
    io:format("AST for ~s:~n~p~n", [ast_to_string(AST), AST]),
    nil.

type_directive(AST, Env) ->
    {Type, _Sub} = infer_expression(AST, Env),
    io:format("Type: ~s~n", [type_to_string(Type)]),
    nil.
```

#### 5.4 Processamento Transparente
```erlang
% src/lx2_analyzer.erl
process_directives(AST) ->
    case AST of
        {directive_call, Directive, Arg} ->
            case process_directive(Directive, Arg) of
                nil -> nil; % Remove diretiva
                _ -> AST
            end;
        _ -> AST
    end.

filter_nil_nodes([nil | Rest]) -> filter_nil_nodes(Rest);
filter_nil_nodes([Node | Rest]) -> [Node | filter_nil_nodes(Rest)];
filter_nil_nodes([]) -> [].
```

### Fase 6: Task 5 - Lists (Semana 7)

#### 6.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
\[ : {token, {lbracket, TokenLine}}.
\] : {token, {rbracket, TokenLine}}.
\| : {token, {pipe, TokenLine}}.
```

#### 6.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
lbracket rbracket pipe.

Nonterminals
list_literal list_cons list_expression.

expression -> list_expression : '$1'.

list_expression -> list_literal : '$1'.
list_expression -> list_cons : '$1'.

list_literal -> lbracket rbracket : {list_literal, []}.
list_literal -> lbracket expression_list rbracket : {list_literal, '$2'}.

list_cons -> lbracket expression pipe expression rbracket :
    {list_cons, '$2', '$4'}.

expression_list -> expression : ['$1'].
expression_list -> expression comma expression_list : ['$1' | '$3'].
```

#### 6.3 Sistema de Kernel para Listas
```erlang
% Adicionar ao src/lx2_kernel.erl
-define(LIST_OPERATORS, #{
    '++' => #{precedence => 1, associativity => right, erlang_op => '++'},
    'length' => #{precedence => 0, associativity => left, erlang_op => 'length'},
    'in' => #{precedence => 3, associativity => left, erlang_op => 'lists:member'}
}).
```

#### 6.4 Sistema de Tipos para Listas
```erlang
% src/lx2_types.erl
infer_list_literal({list_literal, Elements}, Env) ->
    case Elements of
        [] -> {new_type_list(new_type_var(any)), new_substitution()};
        [First | Rest] ->
            {FirstType, Sub1} = infer_expression(First, Env),
            {RestTypes, Sub2} = infer_expressions(Rest, apply_substitution(Env, Sub1)),
            {UnifiedType, Sub3} = unify_list_types([FirstType | RestTypes]),
            FinalSub = compose_substitutions(Sub3, compose_substitutions(Sub2, Sub1)),
            {new_type_list(UnifiedType), FinalSub}
    end.

infer_list_cons({list_cons, Head, Tail}, Env) ->
    {HeadType, Sub1} = infer_expression(Head, Env),
    {TailType, Sub2} = infer_expression(Tail, apply_substitution(Env, Sub1)),
    case unify(TailType, new_type_list(HeadType)) of
        {Sub3, _} ->
            FinalSub = compose_substitutions(Sub3, compose_substitutions(Sub2, Sub1)),
            {new_type_list(HeadType), FinalSub};
        {error, Error} ->
            {error, Error}
    end.
```

### Fase 7: Task 6 - Tuples (Semana 8)

#### 7.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
\{ : {token, {lbrace, TokenLine}}.
\} : {token, {rbrace, TokenLine}}.
```

#### 7.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
lbrace rbrace.

Nonterminals
tuple_literal.

expression -> tuple_literal : '$1'.

tuple_literal -> lbrace rbrace : {tuple_literal, []}.
tuple_literal -> lbrace expression_list rbrace : {tuple_literal, '$2'}.
```

#### 7.3 Sistema de Kernel para Tuplas
```erlang
% Adicionar ao src/lx2_kernel.erl
-define(TUPLE_FUNCTIONS, #{
    'tuple_size' => #{arity => 1, erlang_fun => 'tuple_size'},
    'element' => #{arity => 2, erlang_fun => 'element'},
    'setelement' => #{arity => 3, erlang_fun => 'setelement'}
}).
```

#### 7.4 Sistema de Tipos para Tuplas
```erlang
% src/lx2_types.erl
infer_tuple_literal({tuple_literal, Elements}, Env) ->
    case Elements of
        [] -> {new_type_tuple([]), new_substitution()};
        _ ->
            {ElementTypes, Sub} = infer_expressions(Elements, Env),
            {new_type_tuple(ElementTypes), Sub}
    end.
```

### Fase 8: Task 7 - Maps (Semana 9)

#### 8.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
% : {token, {percent, TokenLine}}.
```

#### 8.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
percent.

Nonterminals
map_literal map_entry map_access.

expression -> map_literal : '$1'.
expression -> map_access : '$1'.

map_literal -> percent lbrace rbrace : {map_literal, []}.
map_literal -> percent lbrace map_entries rbrace : {map_literal, '$3'}.

map_entries -> map_entry : ['$1'].
map_entries -> map_entry comma map_entries : ['$1' | '$3'].

map_entry -> expression colon expression : {map_entry, '$1', '$3'}.

map_access -> expression lbracket expression rbracket :
    {map_access, '$1', '$3'}.
```

#### 8.3 Sistema de Kernel para Maps
```erlang
% Adicionar ao src/lx2_kernel.erl
-define(MAP_FUNCTIONS, #{
    'map_size' => #{arity => 1, erlang_fun => 'map_size'},
    'map_get' => #{arity => 2, erlang_fun => 'map_get'},
    'map_put' => #{arity => 3, erlang_fun => 'maps:put'},
    'map_remove' => #{arity => 2, erlang_fun => 'maps:remove'}
}).
```

### Fase 9: Task 8 - Records (Semana 10)

#### 9.1 Extensão do Lexer
```erlang
% Adicionar ao leex/lx2_lexer.xrl
record : {token, {record, TokenLine}}.
:: : {token, {type_annotation, TokenLine}}.
```

#### 9.2 Extensão do Parser
```erlang
% Adicionar ao yecc/lx2_parser.yrl
Terminals
record type_annotation.

Nonterminals
record_def record_literal record_access record_update.

program -> record_def program : ['$1' | '$2'].

record_def -> record identifier lbrace record_fields rbrace :
    {record_def, '$2', '$4'}.

record_fields -> record_field : ['$1'].
record_fields -> record_field comma record_fields : ['$1' | '$3'].

record_field -> identifier type_annotation type :
    {record_field, '$1', '$3', none}.
record_field -> identifier equals expression type_annotation type :
    {record_field, '$1', '$5', '$3'}.

expression -> record_literal : '$1'.
expression -> record_access : '$1'.
expression -> record_update : '$1'.

record_literal -> identifier lbrace field_assignments rbrace :
    {record_literal, '$1', '$3'}.

field_assignments -> field_assignment : ['$1'].
field_assignments -> field_assignment comma field_assignments : ['$1' | '$3'].

field_assignment -> identifier colon expression :
    {field_assignment, '$1', '$3'}.

record_access -> expression dot identifier :
    {record_access, '$1', '$3'}.

record_update -> percent lbrace expression pipe field_assignments rbrace :
    {record_update, '$3', '$5'}.
```

#### 9.3 Sistema de Tipos para Records
```erlang
% src/lx2_types.erl
infer_record_def({record_def, Name, Fields}, Env) ->
    FieldTypes = [infer_record_field(Field) || Field <- Fields],
    RecordType = new_type_record(Name, FieldTypes),
    {RecordType, new_substitution()}.

infer_record_literal({record_literal, Name, Fields}, Env) ->
    case lookup_record_type(Name, Env) of
        {ok, RecordType} ->
            {FieldTypes, Sub} = infer_field_assignments(Fields, Env),
            case unify_record_fields(RecordType, FieldTypes) of
                {ok, UnifiedType} -> {UnifiedType, Sub};
                {error, Error} -> {error, Error}
            end;
        not_found ->
            {error, {undefined_record, Name}}
    end.
```

### Fase 10: Task 9 - Functions with Arguments (Semana 11)

#### 10.1 Extensão do Parser
```erlang
% Modificar yecc/lx2_parser.yrl
function_def -> def identifier '(' parameters ')' do block end :
    {function_def, '$2', '$4', '$7'}.

parameters -> : [].
parameters -> parameter_list : '$1'.

parameter_list -> identifier : ['$1'].
parameter_list -> identifier comma parameter_list : ['$1' | '$3'].
```

#### 10.2 Sistema de Tipos para Funções
```erlang
% src/lx2_types.erl
infer_function_def({function_def, Name, Params, Body}, Env) ->
    % Criar ambiente local com parâmetros
    ParamTypes = [new_type_var(Param) || Param <- Params],
    LocalEnv = extend_env_list(Params, ParamTypes, Env),

    % Inferir tipo do corpo
    {BodyType, Sub} = infer_block(Body, LocalEnv),

    % Criar tipo da função
    FunType = new_type_fun(ParamTypes, BodyType),

    % Generalizar tipo da função
    GeneralizedType = generalize(FunType, Sub),

    {GeneralizedType, Sub}.
```

#### 10.3 Gerador de Código para Funções
```erlang
% src/lx2_codegen.erl
generate_function_def({function_def, Name, Params, Body}) ->
    ErlName = atom_to_list(Name),
    ErlParams = string:join([atom_to_list(Param) || Param <- Params], ", "),
    ErlBody = generate_block(Body),
    ErlName ++ "(" ++ ErlParams ++ ") ->\n    " ++ ErlBody ++ ".\n".
```

## Sistema de Testes

### Estrutura de Testes

```erlang
% test/lx2_integration_tests.erl
-module(lx2_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%% Testes Task 1
task1_literals_test() ->
    Source = "def answer() do 42 end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:answer()).

%% Testes Task 2
task2_variables_test() ->
    Source = "def test() do x = 42; x end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:test()).

%% Testes Task 3
task3_operators_test() ->
    Source = "def add() do 10 + 5 * 2 end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(20, ModuleName:add()).

%% Testes Task 4
task4_directives_test() ->
    Source = "def test() do x = 42; $print(x); x end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(42, ModuleName:test()).

%% Testes Task 5
task5_lists_test() ->
    Source = "def test() do [1, 2, 3] ++ [4, 5] end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual([1,2,3,4,5], ModuleName:test()).

%% Testes Task 6
task6_tuples_test() ->
    Source = "def test() do {1, 2, 3} end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual({1,2,3}, ModuleName:test()).

%% Testes Task 7
task7_maps_test() ->
    Source = "def test() do %{a: 1, b: 2} end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(#{a => 1, b => 2}, ModuleName:test()).

%% Testes Task 8
task8_records_test() ->
    Source = "record Person { name :: string, age :: integer }\ndef test() do Person{name: \"João\", age: 30} end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertMatch(#person{name = <<"João">>, age = 30}, ModuleName:test()).

%% Testes Task 9
task9_functions_test() ->
    Source = "def add(a, b) do a + b end",
    {ok, ModuleName, BeamCode} = lx2:compile(Source),
    code:load_binary(ModuleName, "", BeamCode),
    ?assertEqual(7, ModuleName:add(3, 4)).
```

## Exemplos de Uso

### Exemplos Task 1
```lx
% examples/task_01/simple.lx
def answer() do
    42
end

def greeting() do
    "Hello, World!"
end

def status() do
    :ok
end
```

### Exemplos Task 2
```lx
% examples/task_02/variables.lx
def simple_binding() do
    x = 42
    x
end

def multiple_bindings() do
    a = 10
    b = 20
    a + b
end
```

### Exemplos Task 3
```lx
% examples/task_03/operators.lx
def arithmetic() do
    a = 10
    b = 5
    a + b * 2
end

def comparison() do
    x = 10
    y = 5
    x > y and x < 20
end
```

### Exemplos Task 4
```lx
% examples/task_04/directives.lx
def debug_example() do
    x = 42
    $print(x)
    $type(x)
    x
end
```

### Exemplos Task 5
```lx
% examples/task_05/lists.lx
def list_operations() do
    numbers = [1, 2, 3, 4, 5]
    extended = [0 | numbers]
    combined = [1, 2] ++ [3, 4]
    length(combined)
end
```

### Exemplos Task 6
```lx
% examples/task_06/tuples.lx
def tuple_operations() do
    point = {10, 20}
    size = tuple_size(point)
    x = element(1, point)
    {size, x}
end
```

### Exemplos Task 7
```lx
% examples/task_07/maps.lx
def map_operations() do
    map = %{a: 1, b: 2, c: 3}
    value = map[a]
    updated = %{map | a: 10}
    {value, updated}
end
```

### Exemplos Task 8
```lx
% examples/task_08/records.lx
record Person { name :: string, age :: integer }

def record_operations() do
    person = Person{name: "João", age: 30}
    name = person.name
    updated = %Person{person | age: 31}
    {name, updated}
end
```

### Exemplos Task 9
```lx
% examples/task_09/functions.lx
def add(a, b) do
    a + b
end

def factorial(n) do
    case n do
        0 -> 1
        n -> n * factorial(n - 1)
    end
end
```

## Comandos de Uso

### Compilação
```bash
# Compilar projeto
make

# Executar testes
make test

# Compilar arquivo LX específico
./bin/lx2 compile examples/task_01/simple.lx

# Executar arquivo LX
./bin/lx2 run examples/task_01/simple.lx

# Compilar com modo debugging
./bin/lx2 compile --mode=both examples/task_01/simple.lx
```

### Desenvolvimento
```bash
# Regenerar lexer e parser
make generate

# Executar Dialyzer
make dialyzer

# Executar testes específicos
make test TEST=lx2_integration_tests

# Debug com Observer
make debug
```

## Cronograma Detalhado

| Semana | Fase | Entregáveis | Critérios de Aceitação |
|--------|------|-------------|------------------------|
| 1-2 | Fundação | Lexer, Parser básico, AST | Tokens reconhecidos, AST gerada |
| 3 | Task 1 | Literais funcionais | Funções com literais compilam e executam |
| 4 | Task 2 | Variáveis e bindings | Variáveis funcionam, escopo isolado |
| 5 | Task 3 | Operadores binários | Precedência correta, tipos verificados |
| 6 | Task 4 | Diretivas | Transparência, efeitos colaterais |
| 7 | Task 5 | Listas | List literals, cons, operadores |
| 8 | Task 6 | Tuplas | Tuple literals, funções nativas |
| 9 | Task 7 | Maps | Map literals, acesso, atualização |
| 10 | Task 8 | Records | Record definitions, literals, access |
| 11 | Task 9 | Funções com argumentos | Parâmetros, type inference |
| 12 | Integração | Testes completos, documentação | 100% compatibilidade com LX1 |

## Métricas de Sucesso

### Funcionalidade
- [ ] 100% compatibilidade com sintaxe LX1
- [ ] Todas as 9 tasks implementadas e funcionais
- [ ] Sistema de tipos Hindley-Milner completo
- [ ] Compilação direta para BEAM

### Qualidade
- [ ] Cobertura de testes > 95%
- [ ] 0 warnings críticos do Dialyzer
- [ ] Performance de compilação < 1 segundo
- [ ] Documentação completa

### Performance
- [ ] Código gerado com performance similar ao Erlang nativo
- [ ] Compilação em tempo aceitável
- [ ] Uso eficiente de memória

## Riscos e Mitigações

### Riscos Técnicos
| Risco | Probabilidade | Impacto | Mitigação |
|-------|---------------|---------|-----------|
| Complexidade do Yacc | Média | Alto | Treinamento e documentação |
| Performance do Parser | Baixa | Médio | Profiling e otimizações |
| Integração Dialyzer | Média | Médio | Testes extensivos |
| Compatibilidade LX1 | Baixa | Alto | Testes de regressão |

### Riscos de Recursos
| Risco | Probabilidade | Impacto | Mitigação |
|-------|---------------|---------|-----------|
| Curva de aprendizado | Alta | Médio | Treinamento e mentoria |
| Disponibilidade da equipe | Média | Alto | Planejamento de contingência |

## Conclusão

Este plano fornece uma visão completa e estruturada para implementar todas as funcionalidades do LX1 no LX2 usando Erlang. A abordagem incremental permite validação contínua e ajustes baseados em feedback, garantindo que o produto final atenda às expectativas e requisitos do projeto.

O uso do Erlang e Yacc oferece uma base sólida para um compilador robusto e manutenível, enquanto a estrutura de fases permite desenvolvimento iterativo e validação contínua. A compilação direta para BEAM e integração com o ecossistema Erlang são diferenciais importantes do LX2 em relação ao LX1.

**Próximos Passos:**
1. Configurar ambiente de desenvolvimento
2. Implementar Fase 1 (Fundação)
3. Executar testes de validação
4. Continuar com fases subsequentes
5. Validação final e documentação