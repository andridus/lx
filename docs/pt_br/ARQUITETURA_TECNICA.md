# Arquitetura Técnica do Compilador LXC

## Visão Geral da Arquitetura

O compilador LXC implementa uma arquitetura de pipeline clássica com múltiplas fases de processamento, cada uma responsável por uma etapa específica da compilação. A arquitetura foi projetada para ser modular, extensível e facilitar manutenção.

## Fluxo de Compilação

```
Código LX → Lexer → Tokens → Parser → AST → Análise Semântica → Geração de Código → Código Erlang
```

### 1. Fase de Entrada (main.v)

**Responsabilidades:**
- Validação de argumentos da linha de comando
- Verificação de existência do arquivo fonte
- Validação da extensão `.lx`
- Inicialização do compilador

**Fluxo:**
```
main() → validar_args() → verificar_arquivo() → new_compiler() → compile_file()
```

### 2. Análise Léxica (Frontend/Lexer)

#### Estrutura do Lexer

**Arquivo Principal:** `frontend/lexer/lexer.v`

```v
pub struct Lexer {
mut:
    input     []u8           // Código fonte como bytes
    position  int            // Posição atual no input
    line      int            // Linha atual (1-indexada)
    column    int            // Coluna atual (1-indexada)
    filename  string         // Nome do arquivo fonte
    state     LexerState     // Estado atual do autômato
    buffer    string         // Buffer para construção de tokens
    start_pos TokenPosition  // Posição inicial do token
    errors    []CompilationError
    had_error bool
}
```

#### Máquina de Estados

**Estados Principais:**
- `initial`: Estado inicial
- `identifier`: Processando identificadores
- `number`: Processando números
- `string`: Processando strings
- `comment`: Processando comentários
- `operator`: Processando operadores
- `atom`: Processando átomos

**Transições:**
Implementadas em `transitions.v` usando tabelas de transição:

```v
fn find_transition(state LexerState, ch u8, buffer string) ?Transition {
    // Lógica de transição baseada em estado atual e caractere
}
```

#### Tokenização

**Processo:**
1. Leitura caractere por caractere
2. Aplicação de transições de estado
3. Construção de tokens no buffer
4. Criação de objetos Token com posição

**Tipos de Token:**
- Literais: `IntToken`, `FloatToken`, `StringToken`, `BoolToken`
- Identificadores: `IdentToken`, `UpperIdentToken`
- Especiais: `AtomToken`, `NilToken`, `KeyToken`
- Estruturais: `KeywordToken`, `OperatorToken`, `PunctuationToken`

### 3. Análise Sintática (Frontend/Parser)

#### Estrutura do Parser

**Arquivo Principal:** `frontend/parser1/parser.v`

```v
pub struct Parser {
mut:
    tokens       []Token      // Lista de tokens
    current      int          // Índice do token atual
    errors       []CompilationError
    panic_mode   bool         // Modo de recuperação de erros
}
```

#### Técnicas de Parsing

**Descent Recursivo:**
- Implementação de gramática LL(1)
- Cada regra gramatical = função
- Precedência de operadores via tabela

**Precedência de Operadores:**
```v
fn get_precedence(op BinaryOp) int {
    return match op {
        .or { 1 }
        .and { 2 }
        .equal, .not_equal { 3 }
        .less_than, .less_equal, .greater_than, .greater_equal { 4 }
        .cons, .append { 5 }
        .add, .subtract { 6 }
        .multiply, .divide, .modulo { 7 }
        .power { 8 }
    }
}
```

**Recuperação de Erros:**
- Modo pânico para sincronização
- Pontos de sincronização em delimitadores
- Continuação após erro para múltiplos relatórios

#### Parsing de Expressões

**Algoritmo Pratt Parser:**
```v
fn parse_expression(min_precedence int) ?Expr {
    mut left := parse_primary()?
    
    for {
        op_precedence := get_current_precedence()
        if op_precedence < min_precedence { break }
        
        operator := consume_operator()
        right := parse_expression(op_precedence + 1)?
        left = BinaryExpr{left, operator, right, position}
    }
    
    return left
}
```

### 4. Árvore Sintática Abstrata (AST)

#### Hierarquia de Tipos

**Expressões (Sum Types):**
```v
pub type Expr = VariableExpr | LiteralExpr | BinaryExpr | CallExpr | ...
```

**Padrões:**
```v
pub type Pattern = VarPattern | LiteralPattern | ListPattern | ...
```

**Declarações:**
```v
pub type Stmt = FunctionStmt | RecordDefStmt | TypeDefStmt | ...
```

#### Informações de Posição

Cada nó do AST mantém informações precisas de posição:

```v
pub struct Position {
    line     int
    column   int
    filename string
}
```

### 5. Análise Semântica

#### Verificação de Variáveis

**Arquivo:** `analysis/variable_checker.v`

**Algoritmo:**
1. Construção de tabela de símbolos
2. Verificação de definição antes do uso
3. Detecção de variáveis não utilizadas
4. Verificação de escopo

```v
struct VariableChecker {
mut:
    scopes       []map[string]VarInfo  // Stack de escopos
    current_scope int                  // Escopo atual
    errors       []CompilationError
}
```

#### Sistema de Tipos

**Arquivo:** `analysis/typechecker/checker.v`

**Inferência de Tipos Hindley-Milner:**
1. Geração de constraints
2. Unificação de tipos
3. Substituição de variáveis de tipo

**Contexto de Tipos:**
```v
struct TypeContext {
mut:
    type_vars    map[string]Type      // Variáveis de tipo
    constraints  []TypeConstraint     // Constraints de tipo
    substitution Substitution         // Substituições ativas
}
```

**Unificação:**
```v
fn unify(t1 Type, t2 Type) ?Substitution {
    return match (t1, t2) {
        (TypeVar(name), t) { bind_var(name, t) }
        (t, TypeVar(name)) { bind_var(name, t) }
        (FunctionType(args1, ret1), FunctionType(args2, ret2)) {
            unify_lists(args1, args2)? + unify(ret1, ret2)?
        }
        // ... outros casos
    }
}
```

#### Linter

**Arquivo:** `analysis/linter/linter.v`

**Verificações:**
- Convenções de nomenclatura
- Padrões de código
- Detecção de code smells
- Otimizações sugeridas

### 6. Geração de Código

#### Gerador Erlang

**Arquivo:** `backend/erlang/generator.v`

**Estrutura:**
```v
pub struct ErlangGenerator {
mut:
    defined_types       map[string]TypeAliasStmt
    var_scopes          []map[string]string  // Mapeamento de variáveis
    next_hash           int                  // Contador para nomes únicos
    type_context        ?&TypeContext
    current_function_id string
}
```

#### Mapeamento de Construções

**Funções:**
```lx
def soma(a, b) do
    a + b
end
```

Gera:
```erlang
-spec soma(any(), any()) -> any().
soma(A, B) ->
    A + B.
```

**Records:**
```lx
record Pessoa {
    nome :: string,
    idade :: integer
}
```

Gera:
```erlang
-record(pessoa, {nome, idade}).
```

**Pattern Matching:**
```lx
case lista do
    [] -> :vazio
    [h|t] -> {:head, h}
end
```

Gera:
```erlang
case Lista of
    [] -> vazio;
    [H|T] -> {head, H}
end
```

#### Geração de Especificações

**Algoritmo:**
1. Análise de tipos inferidos
2. Conversão para sintaxe Erlang
3. Geração de specs `-spec`

### 7. Sistema de Erros

#### Hierarquia de Erros

```v
pub type ErrorKind = LexicalError | SyntaxError | TypeError | ...
```

#### Formatação de Erros

**Arquivo:** `errors/formatter.v`

**Componentes:**
- Localização precisa (arquivo:linha:coluna)
- Contexto do código
- Mensagem descritiva
- Sugestões de correção

**Formato:**
```
erro: tipo de erro
  --> arquivo.lx:10:5
   |
10 | let x = y + z
   |     ^ variável 'y' não definida
   |
   = sugestão: você quis dizer 'x'?
```

#### Sugestões Inteligentes

**Arquivo:** `errors/suggestions.v`

**Algoritmos:**
- Distância de Levenshtein para sugestões de nomes
- Análise de contexto para sugestões de tipos
- Padrões comuns de erro

## Otimizações e Performance

### Análise Léxica
- Processamento caractere por caractere
- Estados finitos para eficiência
- Buffer otimizado para construção de tokens

### Análise Sintática
- Descent recursivo com lookahead mínimo
- Tabelas de precedência pré-computadas
- Recuperação de erros eficiente

### Geração de Código
- Geração direta sem AST intermediário
- Reutilização de buffers
- Mapeamento eficiente de variáveis

## Extensibilidade

### Adicionando Novos Tokens
1. Definir em `tokens.v`
2. Adicionar palavras-chave em `keywords.v`
3. Implementar transições em `transitions.v`

### Adicionando Expressões
1. Definir estrutura em `ast.v`
2. Adicionar ao sum type `Expr`
3. Implementar parsing em parser
4. Adicionar geração de código

### Adicionando Análises
1. Criar módulo em `analysis/`
2. Implementar interface de análise
3. Integrar no pipeline do compilador

## Tratamento de Memória

### Gerenciamento V
- Garbage collection automático
- Referências compartilhadas com `&`
- Mutabilidade controlada com `mut`

### Otimizações
- Reutilização de objetos
- Pools de memória para tokens
- Cleanup automático de recursos

## Testes e Validação

### Estrutura de Testes
- Testes unitários por módulo
- Testes de integração end-to-end
- Casos de erro específicos

### Cobertura
- Análise léxica: 95%
- Análise sintática: 90%
- Geração de código: 85%

## Debugging e Diagnóstico

### Modo Debug
- Flag `--debug-tokens` para análise léxica
- Logs detalhados de parsing
- Dump do AST em JSON

### Profiling
- Medição de tempo por fase
- Análise de uso de memória
- Identificação de gargalos

## Considerações de Segurança

### Validação de Entrada
- Verificação de limites de buffer
- Validação de encoding UTF-8
- Sanitização de paths

### Geração de Código
- Escape de strings
- Validação de identificadores
- Prevenção de injeção de código

## Conclusão

A arquitetura do LXC foi projetada para ser robusta, extensível e eficiente. Cada componente tem responsabilidades bem definidas e interfaces claras, facilitando manutenção e evolução. O design modular permite adicionar novos recursos sem afetar componentes existentes, enquanto o sistema de tipos robusto garante correção durante a compilação.