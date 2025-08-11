# LX0 Compiler

LX0 é um compilador para a linguagem LX que gera código Erlang funcional. O compilador é implementado usando Yecc (parser generator) e Leex (lexer generator) do Erlang.

## Características

- **Parser LALR(1)**: Implementado usando Yecc
- **Lexer**: Implementado usando Leex
- **Sistema de Tipos**: Suporte a type annotations e type definitions
- **Records**: Definição e uso de estruturas de dados
- **Pattern Matching**: Suporte completo a pattern matching
- **Guards**: Expressões guard em funções e case
- **Estruturas de Dados**: Listas, tuplas, mapas, binários
- **Controle de Fluxo**: case, if, block expressions
- **Code Generation**: Geração de código Erlang funcional

## Instalação

```bash
# Clone o repositório
git clone <repository-url>
cd lx0

# Compile o projeto
make compile

# Crie o executável
make install
```

## Uso

### Compilar um arquivo LX

```bash
# Compilar arquivo .lx para .erl
./lx0 compile examples/simple.lx

# Compilar com output personalizado
./lx0 compile examples/simple.lx --output my_module.erl
```

### Parsear um arquivo (mostrar AST)

```bash
# Mostrar AST sem gerar código
./lx0 parse examples/simple.lx
```

### Ajuda

```bash
./lx0 --help
```

## Sintaxe da Linguagem LX

### Funções

```lx
# Função simples
def hello do
    "Hello, World!"
end

# Função com parâmetros
def add(a, b) do
    a + b
end

# Função com type annotations
def multiply(a :: integer, b :: integer) :: integer do
    a * b
end

# Função privada
defp internal_helper(x) do
    x * 2
end
```

### Records

```lx
# Definição de record
record User {
    name :: string,
    age :: integer,
    email :: string
}

# Criação de record
def create_user(name, age, email) do
    User{name: name, age: age, email: email}
end

# Acesso a campos
def get_user_name(user) do
    user.name
end
```

### Type Definitions

```lx
# Type union
type status :: :ok | :error | :pending

# Type alias
type user_id :: integer

# List types
type user_list :: list(User)
```

### Pattern Matching

```lx
# Case expression
def classify_number(n) do
    case n do
        0 -> :zero
        n when n > 0 -> :positive
        _ -> :negative
    end
end

# Pattern matching em funções
def process_result do
    {:ok, value} -> value
    {:error, reason} -> "Error: #{reason}"
end
```

### Estruturas de Dados

```lx
# Listas
def create_list do
    [1, 2, 3, 4, 5]
end

# Tuplas
def create_tuple do
    {:ok, "success"}
end

# Mapas
def create_map do
    %{name: "John", age: 30}
end

# Binários
def create_binary do
    <<1, 2, 3, 4>>
end
```

### Block Expressions

```lx
def calculate do
    do
        a = 10
        b = 20
        result = a + b
        result * 2
    end
end
```

## Exemplos

### Exemplo Simples

```lx
# examples/simple.lx
record User {
    name :: string,
    age :: integer
}

def create_user(name :: string, age :: integer) do
    User{name: name, age: age}
end

def get_user_info(user) do
    "User #{user.name} is #{user.age} years old"
end
```

### Exemplo Complexo

```lx
# examples/complex.lx
record Person {
    name :: string,
    age :: integer,
    email :: string,
    active :: boolean
}

def create_person(name, age, email) do
    Person{name: name, age: age, email: email, active: true}
end

def validate_age(age) when age >= 0 and age <= 150 do
    :ok
end

def validate_age(_) do
    :error
end

def process_person(person) do
    case person do
        Person{name: name, age: age} when age >= 18 ->
            "Adult: #{name}"
        Person{name: name} ->
            "Minor: #{name}"
    end
end
```

## Testes

Execute os testes para verificar se o compilador está funcionando corretamente:

```bash
make test
```

## Estrutura do Projeto

```
lx0/
├── src/
│   ├── lx0_compiler.erl      # Módulo principal do compilador
│   ├── lx0_codegen.erl       # Gerador de código Erlang
│   └── lx0_cli.erl          # Interface de linha de comando
├── leex/
│   └── lx0_lexer.xrl        # Definição do lexer
├── yecc/
│   └── lx0_parser.yrl       # Definição do parser
├── include/
│   └── lx0.hrl              # Definições comuns
├── test/
│   └── lx0_compiler_test.erl # Testes do compilador
├── examples/
│   ├── simple.lx            # Exemplo simples
│   └── complex.lx           # Exemplo complexo
├── Makefile                 # Scripts de build
├── rebar.config            # Configuração do rebar3
└── README.md               # Este arquivo
```

## Desenvolvimento

### Compilar o Projeto

```bash
make compile
```

### Limpar Artefatos

```bash
make clean
```

### Gerar Parser e Lexer

```bash
make generate_parsers
```

### Executar Testes

```bash
make test
```

## Tecnologias Utilizadas

- **Erlang/OTP**: Linguagem base
- **Yecc**: Parser generator (LALR(1))
- **Leex**: Lexer generator
- **rebar3**: Build tool
- **EUnit**: Framework de testes

## Licença

Este projeto está sob a licença MIT.