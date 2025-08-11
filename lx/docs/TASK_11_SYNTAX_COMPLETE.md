# Task 11: Funcionalidades de Sintaxe LX Pendentes - PENDING ⏳

## Status: PENDENTE DE IMPLEMENTAÇÃO

A Task 11 implementará todas as funcionalidades de sintaxe LX que ainda não foram implementadas no lx1, incluindo controle de fluxo, concorrência, binários, tipos customizados e sistema de módulos.

## O que será implementado

### 1. **Controle de Fluxo** 🔄
- **If expressions**: `if condition do ... else ... end`
- **Case expressions**: `case expr do pattern -> expr; pattern -> expr end`
- **With expressions**: `with pattern <- expr do ... else ... end`
- **Match expressions**: `match pattern <- expr rescue error do ... end`

### 2. **Concorrência e Processos** 🔄
- **Spawn**: `spawn(fn -> ... end)`
- **Send operator**: `pid ! message`
- **Receive**: `receive do pattern -> expr; pattern -> expr end`
- **Supervisors**: `supervisor name do ... end`
- **Workers**: `worker name do ... end`

### 3. **Binários e Bitstrings** 🔄
- **Sintaxe básica**: `<<1, 2, 3>>`, `<<"hello">>`
- **Pattern matching**: `<<version:8, data:32/binary>>`
- **Qualificações**: `<<value:16/integer-big>>`
- **Construção**: `<<header/binary, payload/binary>>`

### 4. **Tipos Customizados** 🔄
- **Union types**: `type status :: :ok | :error | :pending`
- **Generic types**: `type result(T) :: {:some, T} | :none`
- **Opaque types**: `type opaque user_id :: integer`
- **Nominal types**: `type nominal email :: string`
- **Recursive types**: `type list(T) :: [] | {T, list(T)}`

### 5. **Sistema de Módulos e Dependências** 🔄
- **Dependencies**: `deps [:cowboy, :outro_modulo]`
- **Application config**: `application.lx`
- **Imports**: `import Module`
- **Type validation**: Validação via beams

### 6. **Funcionalidades Avançadas** 🔄
- **String interpolation**: `"Hello, #{name}!"`
- **Anonymous functions**: `fn(x) -> x * 2 end`
- **List comprehensions**: `[x * 2 || x <- list]`
- **Directives**: `@doc`, `@spec`
- **Test framework**: `describe`, `test`, `assert`

## Estrutura de Arquivos

```
lx1/
├── ast/                        # AST estendida
│   ├── node.v                 # + if, case, with, spawn, receive, binary
│   └── builders.v             # + builders para novos nós
├── lexer/                     # Lexer estendido
│   ├── tokens.v               # + novos tokens
│   └── lexer.v                # + reconhecimento de novos tokens
├── parser/                    # Parser estendido
│   └── parser.v               # + parse de novas construções
├── analysis/                  # Análise estendida
│   ├── analyzer.v             # + análise de novas construções
│   ├── hm_inferencer.v        # + inferência para novos tipos
│   └── type_checker.v         # + verificação de novos tipos
├── generator/                 # Gerador estendido
│   └── erlang_generator.v     # + geração para novas construções
├── examples/task_11/          # NEW: Examples for Task 11
│   ├── control_flow.lx
│   ├── concurrency.lx
│   ├── binaries.lx
│   ├── custom_types.lx
│   ├── modules.lx
│   └── advanced_features.lx
└── tests/
    └── syntax_complete_test.v # NEW: Tests for Task 11
```

## Exemplos Funcionais

### Exemplo 1: Controle de Fluxo
**Entrada LX:**
```lx
def categorize_age(age) do
    if age < 18 do
        "minor"
    else
        "adult"
    end
end

def process_result(result) do
    case result do
        {:ok, data} -> "Success: #{data}"
        {:error, reason} -> "Error: #{reason}"
        _ -> "Unknown result"
    end
end

def get_user_info(id) do
    with {:ok, user} <- fetch_user(id),
         {:ok, profile} <- fetch_profile(user.profile_id) do
        {:ok, %{user: user, profile: profile}}
    else
        {:error, reason} -> {:error, reason}
    end
end
```

### Exemplo 2: Concorrência
**Entrada LX:**
```lx
def start_server() do
    pid = spawn(fn -> server_loop() end)
    pid
end

def server_loop() do
    receive do
        {:message, data} ->
            process_message(data)
            server_loop()
        {:stop} ->
            :ok
    end
end

supervisor main_supervisor do
    strategy :one_for_one
    children [
        worker: [server_worker]
    ]
end

worker server_worker do
    def start_link(_) do
        {:ok, spawn_link(fn -> server_loop() end)}
    end
end
```

### Exemplo 3: Binários
**Entrada LX:**
```lx
def encode_packet(version, data) do
    data_size = byte_size(data)
    <<version:8, data_size:32/big, data/binary>>
end

def decode_packet(packet) do
    <<version:8, data_size:32/big, data:data_size/binary>> = packet
    {:ok, %{version: version, data: data}}
end

def parse_binary_header(binary) do
    <<
        signature:2/binary,
        version:8/integer,
        flags:16/integer-little,
        rest/binary
    >> = binary

    %{signature: signature, version: version, flags: flags, rest: rest}
end
```

### Exemplo 4: Tipos Customizados
**Entrada LX:**
```lx
type status :: :ok | :error | :pending
type result(T) :: {:some, T} | :none
type opaque user_id :: integer
type nominal email :: string
type list(T) :: [] | {T, list(T)}

def process_status(status :: status) do
    case status do
        :ok -> "Success"
        :error -> "Failed"
        :pending -> "In progress"
    end
end

def safe_divide(a, b) :: result(float) do
    if b == 0 do
        :none
    else
        {:some, a / b}
    end
end
```

### Exemplo 5: Sistema de Módulos
**Entrada LX:**
```lx
# application.lx
application {
    description: "My Application",
    vsn: "1.0.0",
    applications: [:kernel, :stdlib],
    registered: [:main_server],
    env: %{
        debug: true,
        port: 8080
    },
    deps: [:cowboy, :jsx]
}

# main.lx
deps [:cowboy, :jsx]

import :cowboy
import :jsx

def start_server() do
    cowboy.start_clear(:http, [
        {port, 8080}
    ], %{
        env: %{dispatch: dispatch()}
    })
end
```

## Implementação Técnica

### 1. **Controle de Fluxo**
```v
// lx1/ast/node.v
pub enum NodeKind {
    // ... existing ...
    if_expr
    case_expr
    with_expr
    match_expr
}

// lx1/parser/parser.v
fn (mut p Parser) parse_if_expression() !ast.Node {
    // Parse if expressions
}

fn (mut p Parser) parse_case_expression() !ast.Node {
    // Parse case expressions
}
```

### 2. **Concorrência**
```v
// lx1/ast/node.v
pub enum NodeKind {
    // ... existing ...
    spawn_expr
    send_expr
    receive_expr
    supervisor_def
    worker_def
}

// lx1/parser/parser.v
fn (mut p Parser) parse_spawn_expression() !ast.Node {
    // Parse spawn expressions
}

fn (mut p Parser) parse_receive_expression() !ast.Node {
    // Parse receive expressions
}
```

### 3. **Binários**
```v
// lx1/ast/node.v
pub enum NodeKind {
    // ... existing ...
    binary_literal
    binary_pattern
}

pub struct BinarySegment {
    value    ast.Node
    size     ?ast.Node
    options  []string
    position ast.Position
}

// lx1/parser/parser.v
fn (mut p Parser) parse_binary_literal() !ast.Node {
    // Parse binary literals
}

fn (mut p Parser) parse_binary_pattern() !ast.Node {
    // Parse binary patterns
}
```

### 4. **Tipos Customizados**
```v
// lx1/ast/node.v
pub enum NodeKind {
    // ... existing ...
    type_def
    type_alias
}

pub struct TypeDef {
    name       string
    parameters []string
    variants   []TypeVariant
    position   ast.Position
}

// lx1/parser/parser.v
fn (mut p Parser) parse_type_definition() !ast.Node {
    // Parse type definitions
}
```

## Testes Completos

### 1. **Testes de Controle de Fluxo**
```v
// lx1/tests/syntax_complete_test.v
fn test_if_expressions() {
    lx_code := '
def test_if(x) do
    if x > 0 do
        "positive"
    else
        "negative"
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == "positive" // when called with positive number
}

fn test_case_expressions() {
    lx_code := '
def test_case(status) do
    case status do
        :ok -> "success"
        :error -> "failure"
        _ -> "unknown"
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}

fn test_with_expressions() {
    lx_code := '
def test_with() do
    with {:ok, value} <- some_operation() do
        "success: #{value}"
    else
        {:error, reason} -> "error: #{reason}"
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}
```

### 2. **Testes de Concorrência**
```v
fn test_spawn_and_send() {
    lx_code := '
def test_concurrency() do
    pid = spawn(fn -> receive_loop() end)
    pid ! {:message, "hello"}
    "sent"
end

def receive_loop() do
    receive do
        {:message, data} -> data
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}

fn test_supervisor_and_worker() {
    lx_code := '
supervisor test_supervisor do
    strategy :one_for_one
    children [
        worker: [test_worker]
    ]
end

worker test_worker do
    def start_link(_) do
        {:ok, spawn_link(fn -> worker_loop() end)}
    end

    def worker_loop() do
        receive do
            :stop -> :ok
            _ -> worker_loop()
        end
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}
```

### 3. **Testes de Binários**
```v
fn test_binary_literals() {
    lx_code := '
def test_binary() do
    binary = <<1, 2, 3>>
    <<a, b, c>> = binary
    a + b + c
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == 6
}

fn test_binary_pattern_matching() {
    lx_code := '
def test_binary_pattern(data) do
    <<version:8, size:16/big, payload:size/binary>> = data
    %{version: version, size: size, payload: payload}
end'

    result := compile_and_test(lx_code)
    assert result.success
}

fn test_binary_qualifications() {
    lx_code := '
def test_binary_quals() do
    value = 12345
    binary = <<value:16/integer-little>>
    <<result:16/integer-little>> = binary
    result
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == 12345
}
```

### 4. **Testes de Tipos Customizados**
```v
fn test_custom_types() {
    lx_code := '
type status :: :ok | :error
type result(T) :: {:some, T} | :none

def test_types(status :: status) do
    case status do
        :ok -> {:some, "success"}
        :error -> :none
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}

fn test_generic_types() {
    lx_code := '
type option(T) :: :none | {:some, T}
type list(T) :: [] | {T, list(T)}

def head(list :: list(T)) :: option(T) do
    case list do
        [] -> :none
        {first, _} -> {:some, first}
    end
end'

    result := compile_and_test(lx_code)
    assert result.success
}
```

### 5. **Testes de Sistema de Módulos**
```v
fn test_module_system() {
    lx_code := '
deps [:cowboy]

def test_deps() do
    :cowboy.start_clear(:http, [], %{})
end'

    result := compile_and_test(lx_code)
    assert result.success
}

fn test_application_config() {
    lx_code := '
application {
    description: "Test App",
    vsn: "1.0.0",
    applications: [:kernel, :stdlib],
    deps: [:cowboy]
}

def main() do
    "application configured"
end'

    result := compile_and_test(lx_code)
    assert result.success
}
```

### 6. **Testes de Funcionalidades Avançadas**
```v
fn test_string_interpolation() {
    lx_code := '
def test_interpolation(name, age) do
    "Hello, #{name}! You are #{age} years old."
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == "Hello, John! You are 25 years old."
}

fn test_anonymous_functions() {
    lx_code := '
def test_anon_functions() do
    double = fn(x) -> x * 2 end
    numbers = [1, 2, 3, 4, 5]
    doubled = map(double, numbers)
    doubled
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == [2, 4, 6, 8, 10]
}

fn test_list_comprehensions() {
    lx_code := '
def test_comprehensions() do
    numbers = [1, 2, 3, 4, 5]
    squares = [x * x || x <- numbers, x > 2]
    squares
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == [9, 16, 25]
}
```

### 7. **Testes de Integração**
```v
fn test_complex_integration() {
    lx_code := '
type user_status :: :active | :inactive | :pending

def process_users(users) do
    active_users = [user || user <- users, user.status == :active]

    case active_users do
        [] -> {:error, "No active users"}
        [first | _] -> {:ok, first}
        _ -> {:ok, length(active_users)}
    end
end

def test_integration() do
    users = [
        %{name: "Alice", status: :active},
        %{name: "Bob", status: :inactive},
        %{name: "Charlie", status: :active}
    ]

    result = process_users(users)
    result
end'

    result := compile_and_test(lx_code)
    assert result.success
    assert result.output == {:ok, 2}
}
```

### 8. **Testes de Erro**
```v
fn test_syntax_errors() {
    lx_code := '
def invalid_syntax() do
    if x > 0
        "positive"
    end
end'

    result := compile_and_test(lx_code)
    assert !result.success
    assert result.errors.len > 0
    assert result.errors[0].contains('expected do')
}

fn test_type_errors() {
    lx_code := '
def type_error() do
    x = 42
    y = "string"
    x + y
end'

    result := compile_and_test(lx_code)
    assert !result.success
    assert result.errors.len > 0
    assert result.errors[0].contains('type mismatch')
}
```

## Critérios de Aceitação

### ✅ Funcionalidades Obrigatórias
- [ ] If expressions funcionando
- [ ] Case expressions funcionando
- [ ] With expressions funcionando
- [ ] Match expressions funcionando
- [ ] Spawn funcionando
- [ ] Send operator funcionando
- [ ] Receive expressions funcionando
- [ ] Supervisors funcionando
- [ ] Workers funcionando
- [ ] Binários básicos funcionando
- [ ] Pattern matching em binários funcionando
- [ ] Tipos customizados funcionando
- [ ] Sistema de módulos funcionando
- [ ] Dependências funcionando
- [ ] String interpolation funcionando
- [ ] Anonymous functions funcionando
- [ ] List comprehensions funcionando
- [ ] Directives funcionando
- [ ] Test framework funcionando

### ✅ Testes Obrigatórios
- [ ] Testes de controle de fluxo
- [ ] Testes de concorrência
- [ ] Testes de binários
- [ ] Testes de tipos customizados
- [ ] Testes de sistema de módulos
- [ ] Testes de funcionalidades avançadas
- [ ] Testes de integração
- [ ] Testes de performance
- [ ] Testes de erro
- [ ] Testes de sintaxe inválida

### ✅ Documentação Obrigatória
- [ ] Documentação de sintaxe
- [ ] Exemplos de uso
- [ ] Guia de migração
- [ ] Referência completa
- [ ] Casos de teste documentados
- [ ] Guia de troubleshooting

## Estimativa de Tempo
- **Controle de Fluxo**: 1 semana
- **Concorrência**: 1.5 semanas
- **Binários**: 1 semana
- **Tipos Customizados**: 1 semana
- **Sistema de Módulos**: 1 semana
- **Funcionalidades Avançadas**: 1 semana
- **Testes**: 1 semana
- **Documentação**: 3-4 dias
- **Total**: 7-8 semanas

## Dependências
- Task 1-9 completas
- Task 10 (HM completo) completa
- Sistema de tipos robusto
- Parser e AST estáveis

## Próximos Passos
1. Implementar controle de fluxo
2. Implementar concorrência
3. Implementar binários
4. Implementar tipos customizados
5. Implementar sistema de módulos
6. Implementar funcionalidades avançadas
7. Implementar testes completos
8. Documentar funcionalidades