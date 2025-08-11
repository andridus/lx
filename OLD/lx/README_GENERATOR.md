# Lx Language Generator

Este documento descreve a implementação do generator para a linguagem Lx, que converte código Lx em código Erlang executável.

## Arquitetura

O sistema de geração de código Lx segue uma arquitetura de três estágios:

1. **Parser/Lexer** - Converte código Lx em AST (Abstract Syntax Tree)
2. **AST Transformer** - Converte o AST do Lx para Core Erlang AST
3. **Code Generator** - Gera código Erlang a partir do Core Erlang AST

## Módulos Principais

### lx_ast_transformer.erl
Responsável por transformar o AST do Lx para Core Erlang AST.

**Funcionalidades:**
- Conversão de literais (integers, floats, atoms, strings)
- Conversão de estruturas de dados (tuples, lists, maps)
- Conversão de operadores binários
- Conversão de chamadas de função
- Conversão de estruturas de controle (case, try, etc.)

### lx_generator.erl
Responsável por gerar código Erlang a partir do Core Erlang AST.

**Funcionalidades:**
- Geração de módulos Erlang completos
- Geração de expressões e operadores
- Geração de estruturas de dados
- Geração de chamadas de função
- Tratamento especial para operadores aritméticos

### lx_compiler.erl
Orquestra todo o processo de compilação.

**Funcionalidades:**
- `compile/2` - Compila código Lx para AST
- `compile_to_erlang/2` - Compila código Lx para código Erlang
- `compile_and_run/1` - Compila e executa código Lx diretamente

## Uso

### Comando `generate`
Gera código Erlang a partir de um arquivo .lx:

```bash
lx generate arquivo.lx
```

Exemplo:
```bash
lx generate examples/test.lx
# Gera: test.erl
```

### Comando `run`
Compila e executa um arquivo .lx diretamente:

```bash
lx run arquivo.lx
```

Exemplo:
```bash
lx run examples/test.lx
# Result: 2
```

### Comando padrão
Mostra o AST de um arquivo .lx:

```bash
lx arquivo.lx
```

## Exemplos

### Exemplo Simples
**test.lx:**
```lx
1 + 1
```

**test.erl gerado:**
```erlang
-module(test).
-export([main/0]).

main() ->
    (1 + 1).
```

### Exemplo Complexo
**complex_test.lx:**
```lx
1 + 2 * 3
{1, 2, 3}
[1, 2, 3, 4]
%{a: 1, b: 2}
```

**complex_test.erl gerado:**
```erlang
-module(complex_test).
-export([main/0]).

main() ->
    (1 + (2 * 3)),
    {1, 2, 3},
    [1 | [2 | [3 | [4 | []]]]],
    #{a => 1, b => 2}.
```

## Pipeline de Compilação

1. **Lexical Analysis** (`lx_lexer`)
   - Tokenização do código fonte Lx

2. **Syntactic Analysis** (`lx_parser`)
   - Geração do AST do Lx

3. **AST Transformation** (`lx_ast_transformer`)
   - Conversão para Core Erlang AST

4. **Code Generation** (`lx_generator`)
   - Geração de código Erlang

5. **Compilation** (compiler Erlang)
   - Compilação para BEAM

6. **Execution** (runtime Erlang)
   - Execução do código compilado

## Estruturas Suportadas

### Literais
- Integers: `1`, `42`, `-10`
- Floats: `3.14`, `2.0`
- Atoms: `:ok`, `:error`
- Strings: `"hello"`

### Estruturas de Dados
- Tuples: `{1, 2, 3}`
- Lists: `[1, 2, 3, 4]`
- Maps: `%{key: value, "key": value}`

### Operadores
- Aritméticos: `+`, `-`, `*`, `/`
- Comparação: `==`, `!=`, `<`, `>`, `<=`, `>=`

### Chamadas de Função
- Locais: `funcao(arg1, arg2)`
- Remotas: `modulo:funcao(arg1, arg2)`

## Limitações Atuais

- Não suporta definição de funções
- Não suporta pattern matching complexo
- Não suporta guards
- Não suporta receive/send
- Não suporta records

## Próximos Passos

1. Implementar suporte a definição de funções
2. Adicionar suporte a pattern matching
3. Implementar guards
4. Adicionar suporte a receive/send
5. Implementar records
6. Adicionar suporte a macros
7. Implementar sistema de módulos