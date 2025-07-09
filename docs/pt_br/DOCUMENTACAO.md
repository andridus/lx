# Documentação do Compilador LXC

## Visão Geral

O LXC (LX Compiler) é um compilador para a linguagem de programação funcional LX, que compila código LX para Erlang. A linguagem LX é inspirada em Elixir e Erlang, oferecendo uma sintaxe mais limpa e recursos modernos de programação funcional.

## Estrutura do Projeto

```
lx-lang/lxc/
├── main.v              # Ponto de entrada do compilador
├── v.mod               # Arquivo de configuração do módulo V
├── compiler/           # Módulo principal do compilador
├── frontend/           # Análise léxica e sintática
├── ast/                # Definições da árvore sintática abstrata
├── analysis/           # Análise semântica e verificação de tipos
├── backend/            # Geração de código
├── errors/             # Sistema de tratamento de erros
├── utils/              # Utilitários
├── tests/              # Testes
└── ex/                 # Exemplos de código
```

## Arquitetura do Compilador

O compilador LXC segue uma arquitetura tradicional em pipeline com as seguintes fases:

### 1. Análise Léxica (Lexer)
**Localização**: `frontend/lexer/`

O lexer converte o código fonte em tokens. Principais componentes:

- **lexer.v**: Implementação principal do analisador léxico
- **tokens.v**: Definições dos tipos de tokens
- **keywords.v**: Mapeamento de palavras-chave
- **operators.v**: Definições de operadores
- **states.v**: Estados do autômato finito
- **transitions.v**: Transições entre estados

**Tipos de Tokens Suportados**:
- `IdentToken`: Identificadores (variáveis, funções)
- `UpperIdentToken`: Identificadores maiúsculos (records)
- `StringToken`: Strings literais
- `IntToken`: Números inteiros
- `FloatToken`: Números de ponto flutuante
- `BoolToken`: Valores booleanos
- `AtomToken`: Átomos
- `NilToken`: Valor nil
- `KeywordToken`: Palavras-chave da linguagem
- `OperatorToken`: Operadores
- `PunctuationToken`: Pontuação
- `DirectiveToken`: Diretivas (@reflection, @spec, etc.)

### 2. Análise Sintática (Parser)
**Localização**: `frontend/parser/` e `frontend/parser1/`

O parser converte tokens em uma Árvore Sintática Abstrata (AST). Há duas implementações:

- **parser/**: Parser mais completo com recuperação de erros
- **parser1/**: Parser simplificado usado atualmente

**Principais Funcionalidades**:
- Análise de expressões com precedência de operadores
- Parsing de definições de funções, records e tipos
- Suporte a pattern matching
- Tratamento de blocos e estruturas de controle

### 3. Árvore Sintática Abstrata (AST)
**Localização**: `ast/`

Define todas as estruturas de dados que representam o código LX:

#### Expressões (Expr)
- **VariableExpr**: Referência a variáveis
- **LiteralExpr**: Literais (strings, números, etc.)
- **BinaryExpr**: Operações binárias
- **CallExpr**: Chamadas de função
- **MatchExpr**: Pattern matching
- **ListExpr**: Listas e operações com listas
- **TupleExpr**: Tuplas
- **MapExpr**: Mapas (dicionários)
- **RecordExpr**: Records
- **FunExpr**: Funções anônimas
- **IfExpr**: Expressões condicionais
- **CaseExpr**: Expressões case
- **ReceiveExpr**: Recebimento de mensagens
- **SendExpr**: Envio de mensagens

#### Padrões (Pattern)
- **VarPattern**: Variáveis em padrões
- **LiteralPattern**: Literais em padrões
- **ListPattern**: Padrões de listas
- **TuplePattern**: Padrões de tuplas
- **RecordPattern**: Padrões de records
- **WildcardPattern**: Padrão coringa (_)

#### Declarações (Stmt)
- **FunctionStmt**: Definições de funções
- **RecordDefStmt**: Definições de records
- **TypeDefStmt**: Definições de tipos
- **TypeAliasStmt**: Alias de tipos
- **ModuleStmt**: Módulo

### 4. Análise Semântica
**Localização**: `analysis/`

#### Verificação de Variáveis
**Arquivo**: `variable_checker.v`

Verifica se todas as variáveis estão definidas antes do uso e detecta variáveis não utilizadas.

#### Linter
**Localização**: `analysis/linter/`

Verifica estilo de código e boas práticas:
- Convenções de nomenclatura
- Padrões de código recomendados
- Detecção de código potencialmente problemático

#### Verificação de Tipos
**Localização**: `analysis/typechecker/`

Implementa verificação de tipos usando inferência de tipos estilo Hindley-Milner:

- **checker.v**: Verificador principal
- **context.v**: Contexto de tipos
- **types.v**: Definições de tipos
- **unification.v**: Algoritmo de unificação
- **substitution.v**: Substituições de tipos

**Tipos Suportados**:
- Primitivos: `integer`, `float`, `string`, `boolean`, `atom`, `nil`
- Compostos: `list`, `tuple`, `map`, `record`
- Especiais: `function`, `pid`, `reference`, `port`, `binary`

#### Validação de Diretivas
**Localização**: `analysis/directives/`

Valida diretivas especiais como `@spec`, `@reflection`, `@supervisor`, etc.

### 5. Geração de Código
**Localização**: `backend/`

#### Gerador de Erlang
**Localização**: `backend/erlang/`

Converte o AST em código Erlang:

- **generator.v**: Gerador principal
- **expressions.v**: Geração de expressões
- **patterns.v**: Geração de padrões
- **statements.v**: Geração de declarações
- **formatting.v**: Formatação do código gerado

**Funcionalidades**:
- Conversão de sintaxe LX para Erlang
- Geração de especificações de tipo (-spec)
- Geração de definições de records
- Mapeamento de funções e módulos

### 6. Sistema de Erros
**Localização**: `errors/`

Sistema robusto de tratamento de erros:

- **errors.v**: Definições de tipos de erro
- **formatter.v**: Formatação de mensagens de erro
- **suggestions.v**: Sugestões de correção

**Tipos de Erros**:
- `LexicalError`: Erros léxicos
- `SyntaxError`: Erros de sintaxe
- `TypeError`: Erros de tipo
- `UnboundVariableError`: Variáveis não definidas
- `UnboundFunctionError`: Funções não definidas
- `PatternError`: Erros de pattern matching
- `RecordError`: Erros relacionados a records

## Características da Linguagem LX

### Sintaxe Básica

```lx
# Definição de função
def soma(a, b) do
  a + b
end

# Função privada
defp helper(x) do
  x * 2
end

# Definição de record
record Pessoa {
  nome :: string,
  idade :: integer
}

# Definição de tipo
type Lista :: list
```

### Pattern Matching

```lx
def processar_lista(lista) do
  case lista do
    [] -> :vazio
    [head | tail] -> {:head, head, :tail, tail}
  end
end
```

### Concorrência

```lx
# Envio de mensagem
pid ! mensagem

# Recebimento de mensagem
receive do
  {:ok, resultado} -> resultado
  {:erro, razao} -> {:erro, razao}
after
  5000 -> :timeout
end
```

### Tipos e Especificações

```lx
def calcular(a :: integer, b :: integer) :: integer do
  a + b
end
```

### Diretivas Especiais

- `@reflection`: Informações de reflexão

## Uso do Compilador

### Compilação Básica

```bash
lxc arquivo.lx
```

### Compilação com Debug

```bash
lxc arquivo.lx --debug-tokens
```

### Estrutura de Saída

O compilador gera arquivos `.erl` no mesmo diretório do arquivo fonte:

```
entrada.lx → entrada.erl
```

## Exemplo Completo

**Arquivo: exemplo.lx**
```lx
record Pessoa {
  nome :: string,
  idade :: integer
}

def criar_pessoa(nome, idade) do
  Pessoa{nome: nome, idade: idade}
end

def saudar(pessoa) do
  case pessoa do
    Pessoa{nome: nome, idade: idade} when idade >= 18 ->
      "Olá, #{nome}! Você é maior de idade."
    Pessoa{nome: nome} ->
      "Olá, #{nome}! Você é menor de idade."
  end
end
```

**Saída: exemplo.erl**
```erlang
-module(exemplo).
-export([criar_pessoa/2, saudar/1]).

-record(pessoa, {nome, idade}).

-spec criar_pessoa(string(), integer()) -> #pessoa{}.
criar_pessoa(Nome, Idade) ->
    #pessoa{nome = Nome, idade = Idade}.

-spec saudar(#pessoa{}) -> string().
saudar(Pessoa) ->
    case Pessoa of
        #pessoa{nome = Nome, idade = Idade} when Idade >= 18 ->
            "Olá, " ++ Nome ++ "! Você é maior de idade.";
        #pessoa{nome = Nome} ->
            "Olá, " ++ Nome ++ "! Você é menor de idade."
    end.
```

## Testes

O projeto inclui testes abrangentes em `tests/` cobrindo:

- Análise léxica
- Análise sintática
- Verificação de tipos
- Geração de código
- Casos de erro

Para executar os testes:

```bash
v test .
```

## Desenvolvimento

### Adicionando Novos Recursos

1. **Tokens**: Adicionar novos tokens em `frontend/lexer/tokens.v`
2. **Parser**: Estender o parser em `frontend/parser1/`
3. **AST**: Adicionar novos nós em `ast/ast.v`
4. **Análise**: Implementar verificações em `analysis/`
5. **Geração**: Adicionar suporte no backend em `backend/erlang/`

### Estrutura de Módulos

O projeto usa módulos V organizados hierarquicamente:

```
lx (módulo raiz)
├── compiler
├── frontend.lexer
├── frontend.parser1
├── ast
├── analysis
├── backend.erlang
└── errors
```

### Convenções de Código

- Usar nomes descritivos em inglês nos comentários
- Seguir as convenções do V para nomes de variáveis e funções
- Documentar todas as funções públicas
- Incluir testes para novos recursos

## Limitações Conhecidas

1. **Macros**: Não suportadas atualmente
2. **Protocolos**: Implementação limitada
3. **OTP**: Suporte básico
4. **Application**: Não suportada atualmente
5. **Debugging**: Informações limitadas de debug

## Futuras Melhorias

1. Suporte completo a macros
2. Melhor integração com OTP
3. Otimizações de código
4. Suporte a módulos dinâmicos
5. Integração com ferramentas de debug

## Conclusão

O compilador LXC representa uma implementação sólida de um compilador funcional moderno, oferecendo uma ponte entre a sintaxe limpa da linguagem LX e a robustez da máquina virtual Erlang. Com sua arquitetura modular e extensível, o projeto está bem posicionado para crescimento e melhorias futuras.
