# LX Compiler and Runtime

Um compilador e runtime para a linguagem LX, implementado em Erlang com AST estilo Elixir e sistema de tipos Hindley-Milner.

## Características

- **AST Elixir-Style**: Estrutura `{tipo, meta, args}` com metadados completos (linha, coluna, tipo)
- **Sistema de Macros**: Tudo é definido via macros, exceto `defmacro`
- **Type Safety**: Verificação de tipos em tempo de compilação
- **Hindley-Milner**: Sistema de tipos com unificação automática
- **Posições Precisas**: Linha e coluna corretas para debugging

## Instalação

1. Certifique-se de ter Erlang instalado
2. Clone o repositório
3. Navegue para a pasta `lx`
4. Compile o projeto:

```bash
make compile
```

## Uso

### Executável Principal

O executável `lx` pode compilar e executar arquivos `.lx`:

```bash
./lx <arquivo.lx> [args...]
```

### Exemplos

#### Exemplo Básico (calculator.lx)
```lx
x = 10
y = 5
sum = x + y
product = x * y
difference = x - y
quotient = x / y
```

Execute:
```bash
./lx examples/calculator.lx
```

Saída:
```
Compiling examples/calculator.lx...
Compilation successful!
AST: [...]
Macro specs: []

Executing...
Result: {ok,#{x => 10,y => 5,sum => 15,product => 50,difference => 5,quotient => 2.0}}
```

#### Exemplo com Strings (simple_string.lx)
```lx
first = "Hello"
second = " World"
greeting = first ++ second
```

Execute:
```bash
./lx examples/simple_string.lx
```

Saída:
```
Compiling examples/simple_string.lx...
Compilation successful!
AST: [...]
Macro specs: []

Executing...
Result: {ok,#{first => "Hello",second => " World",greeting => "Hello World"}}
```

## Pipeline de Compilação

O compilador segue um pipeline de 5 fases:

1. **Lexical Analysis**: Tokenização com posições precisas
2. **Syntactic Analysis**: Parsing para AST Elixir-style
3. **Macro Expansion**: Expansão de macros com verificação de tipos
4. **Type Analysis**: Inferência de tipos com Hindley-Milner
5. **Code Generation**: Geração de código (retorna AST tipado)

## Tipos Suportados

### Tipos Básicos
- `integer`: Números inteiros
- `float`: Números de ponto flutuante
- `string`: Strings
- `boolean`: Valores booleanos (true/false)
- `atom`: Átomos
- `nil`: Valor nulo

### Tipos Compostos
- `{fun, [arg_types], return_type}`: Funções
- `{list, element_type}`: Listas
- `{tuple, [element_types]}`: Tuplas
- `{type_var, name}`: Variáveis de tipo (polimorfismo)

## Operadores Suportados

### Aritméticos
- `+`: Adição
- `-`: Subtração
- `*`: Multiplicação
- `/`: Divisão

### Concatenação
- `++`: Concatenação de strings

## Estrutura do Projeto

```
lx/
├── lx                    # Executável principal
├── include/lx.hrl        # Definições de tipos e macros
├── leex/lx_lexer.xrl     # Lexer com suporte a tipos
├── yecc/lx_parser.yrl    # Parser com AST Elixir-style
├── src/
│   ├── lx.erl            # Módulo principal do compilador
│   ├── lx_lexer.erl      # Interface do lexer
│   ├── lx_parser.erl     # Interface do parser
│   ├── lx_types.erl      # Sistema de tipos Hindley-Milner
│   └── lx_macros.erl     # Sistema de macros com tipos
├── test/lx_ast_meta_tests.erl  # Testes do sistema
├── examples/             # Exemplos de uso
├── rebar.config          # Configuração do rebar3
└── Makefile             # Comandos de build
```

## Comandos Make

- `make compile`: Compila todos os módulos
- `make clean`: Remove arquivos compilados
- `make test`: Executa os testes
- `make generate`: Regenera lexer e parser
- `make example`: Executa um exemplo

## Desenvolvimento

Para desenvolvimento, você pode usar:

```bash
# Compilar tudo
make compile

# Testar um arquivo específico
./lx examples/calculator.lx

# Executar testes
make test
```

## Licença

Este projeto é parte do ecossistema LX Language.