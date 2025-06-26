# Estratégia de Reescrita do Compilador LX em V

## Visão Geral

Este documento descreve a estratégia completa para reescrever o compilador LX de OCaml para V, mantendo toda a funcionalidade existente enquanto aproveita as vantagens da linguagem V.

## Análise da Arquitetura Atual

### Compilador OCaml Atual

O compilador LX atual em OCaml possui a seguinte estrutura:

```
lx/compiler/
├── ast.ml              # Abstract Syntax Tree (AST)
├── lexer.mll           # Analisador léxico (OCamlLex)
├── parser.mly          # Analisador sintático (Menhir)
├── typechecker.ml      # Verificação de tipos
├── compiler.ml         # Compilação para Erlang
├── linter.ml           # Análise estática
├── error.ml            # Sistema de tratamento de erros
├── app_generator.ml    # Geração de arquivos .app.src
├── otp_validator.ml    # Validação de componentes OTP
├── dependency_resolver.ml # Resolução de dependências
├── beam_analyzer.ml    # Análise de arquivos BEAM
├── rebar_manager.ml    # Gerenciamento de projetos Rebar
├── error_formatter.ml  # Formatação de mensagens de erro
└── debug.ml            # Utilitários de debug
```

### Funcionalidades Principais

1. **Lexical Analysis**: Reconhecimento de tokens LX
2. **Syntactic Analysis**: Parsing de código LX para AST
3. **Type Checking**: Inferência de tipos Hindley-Milner
4. **Static Analysis**: Linting e validações
5. **Code Generation**: Compilação para Erlang
6. **OTP Validation**: Validação de componentes OTP
7. **Project Generation**: Geração de projetos Erlang

## Mapeamento OCaml → V

### Estruturas de Dados

| OCaml | V | Observações |
|-------|---|-------------|
| `type record = { field: type }` | `struct Record { field Type }` | Structs V são similares a records OCaml |
| `type variant = A \| B of type` | `union Variant { A, B(Type) }` | Unions V para sum types |
| `'a list` | `[]Type` | Arrays V em vez de listas encadeadas |
| `('a, 'b) Hashtbl.t` | `map[string]Type` | Maps V para hash tables |
| `'a option` | `?Type` | Option types nativos do V |
| `('a, 'b) result` | `!Type` | Result types nativos do V |
| `fun x -> expr` | `fn(x Type) Type { expr }` | Funções V são mais explícitas |

### Padrões de Programação

| OCaml Pattern | V Equivalent | Observações |
|---------------|--------------|-------------|
| `match expr with \| A -> ...` | `match expr { A { ... } }` | Match expressions V |
| `let rec f x = ...` | `fn f(x Type) Type { ... }` | Recursão explícita |
| `exception Error of string` | `!Type` return values | Error handling via Result |
| `module M = struct ... end` | `module M { ... }` | Módulos V |
| `open Module` | `import module` | Imports V |

## Arquitetura Proposta em V

### Estrutura de Módulos

```
lx_compiler/
├── ast/
│   ├── ast.v          # Definições AST
│   ├── position.v     # Posições e spans
│   └── types.v        # Tipos básicos
├── lexer/
│   ├── lexer.v        # Analisador léxico
│   ├── tokens.v       # Definições de tokens
│   └── keywords.v     # Palavras-chave
├── parser/
│   ├── parser.v       # Analisador sintático
│   ├── grammar.v      # Regras gramaticais
│   └── precedence.v   # Precedência de operadores
├── typechecker/
│   ├── typechecker.v  # Verificação de tipos
│   ├── types.v        # Sistema de tipos
│   ├── unification.v  # Algoritmo de unificação
│   └── context.v      # Contexto de tipos
├── compiler/
│   ├── compiler.v     # Geração de código
│   ├── erlang.v       # Templates Erlang
│   └── scope.v        # Gerenciamento de escopo
├── linter/
│   ├── linter.v       # Análise estática
│   ├── rules.v        # Regras de linting
│   └── context.v      # Contexto de análise
├── error/
│   ├── error.v        # Sistema de erros
│   ├── formatter.v    # Formatação de erros
│   └── suggestions.v  # Sugestões de correção
├── otp/
│   ├── validator.v    # Validação OTP
│   ├── worker.v       # Validação de workers
│   └── supervisor.v   # Validação de supervisors
├── generator/
│   ├── app.v          # Geração de .app.src
│   ├── rebar.v        # Geração de rebar.config
│   └── templates.v    # Templates de projeto
├── dependency/
│   ├── resolver.v     # Resolução de dependências
│   ├── beam.v         # Análise de BEAM
│   └── hex.v          # Integração Hex
├── utils/
│   ├── string.v       # Utilitários de string
│   ├── file.v         # Utilitários de arquivo
│   └── debug.v        # Utilitários de debug
└── main.v             # Ponto de entrada
```

### Definições de Tipos Principais

#### AST (Abstract Syntax Tree)

```v
// Posição no código fonte
struct Position {
    line int
    column int
    filename ?string
}

// Literais básicos
union Literal {
    LString(string)
    LInt(int)
    LFloat(f64)
    LBool(bool)
    LAtom(string)
    LNil
}

// Expressões
union Expr {
    Literal(Literal)
    Var(string)
    Assign {
        name string
        value Expr
        position ?Position
    }
    PatternMatch {
        pattern Pattern
        value Expr
        position ?Position
        unsafe bool
    }
    Fun {
        params []string
        body Expr
    }
    App {
        func Expr
        args []Expr
    }
    ExternalCall {
        module string
        function string
        args []Expr
        position ?Position
    }
    Tuple([]Expr)
    List([]Expr)
    Cons {
        head Expr
        tail Expr
    }
    Match {
        expr Expr
        clauses []MatchClause
    }
    If {
        condition Expr
        then_branch Expr
        else_branch ?Expr
    }
    With {
        steps []WithStep
        success_body Expr
        else_branch ?Expr
    }
    For {
        pattern Pattern
        var ?string
        iterable Expr
        body Expr
        guard ?GuardExpr
    }
    Sequence([]Expr)
    Block([]Expr)
    BinOp {
        left Expr
        op string
        right Expr
    }
    UnaryOp {
        op string
        expr Expr
    }
    Send {
        target Expr
        message Expr
    }
    Receive {
        clauses []ReceiveClause
        timeout ?TimeoutClause
    }
    RecordCreate {
        name string
        fields []RecordField
    }
    RecordAccess {
        expr Expr
        field string
    }
    RecordUpdate {
        expr Expr
        updates []RecordField
    }
    MapCreate([]MapField)
    MapAccess {
        map Expr
        key Expr
    }
    BinaryCreate([]BinaryElement)
    FunExpression {
        params []string
        body Expr
    }
    FunExpressionClauses([]FunClause)
}

// Padrões
union Pattern {
    PWildcard
    PVar(string)
    PAtom(string)
    PLiteral(Literal)
    PTuple([]Pattern)
    PList([]Pattern)
    PCons {
        head Pattern
        tail Pattern
    }
    PRecord {
        name string
        fields []PatternField
    }
    PMap([]MapPatternField)
    PBinary([]BinaryPatternElement)
}

// Guards
union GuardExpr {
    GuardAnd(GuardExpr, GuardExpr)
    GuardOr(GuardExpr, GuardExpr)
    GuardAndalso(GuardExpr, GuardExpr)
    GuardOrelse(GuardExpr, GuardExpr)
    GuardNot(GuardExpr)
    GuardBinOp {
        left GuardValue
        op string
        right GuardValue
    }
    GuardCall {
        function string
        args []GuardValue
    }
    GuardAtom(GuardAtom)
}

// Componentes OTP
union OtpComponent {
    Worker {
        name string
        functions []FunctionDef
        specs []Spec
        position ?Position
    }
    Supervisor {
        name ?string
        strategy OtpStrategy
        children ChildrenSpec
        position ?Position
    }
}

// Programa completo
struct Program {
    deps ?[]Dependency
    items []ModuleItem
}
```

#### Sistema de Tipos

```v
// Variáveis de tipo
struct TypeVar {
    id int
}

// Tipos LX
union LxType {
    TVar(TypeVar)
    TInteger
    TFloat
    TString
    TBool
    TAtom
    TPid
    TNil
    TOption(LxType)
    TFun {
        param LxType
        return LxType
    }
    TTuple([]LxType)
    TList(LxType)
    TRecord([]RecordTypeField)
    TNamedRecord(string)
    TMap {
        key LxType
        value LxType
    }
    TConcreteMap([]MapTypeField)
    TBinary
    TBitstring
    TOtpState
    TOtpReply(LxType)
    TOtpCast
    TOtpInfo
}

// Contexto de tipos
struct TypeContext {
    variables map[string]LxType
    parent ?&TypeContext
    type_vars map[int]LxType
}

// Substituição de tipos
struct Substitution {
    mappings map[int]LxType
}
```

#### Sistema de Erros

```v
// Tipos de erro
union ErrorKind {
    SyntaxError(string)
    UnexpectedToken {
        found string
        expected ?string
    }
    UnterminatedString
    UnexpectedCharacter(u8)
    ReservedWordError(string)
    ParseError(string)
    TypeError {
        message string
        suggestion ?string
    }
    RecordFieldError {
        field string
        record_type string
        available_fields []string
    }
    UnboundVariable {
        variable string
        similar_names []string
    }
    TypeMismatch {
        expected string
        found string
        context ?string
    }
    ArityMismatch {
        function_name string
        expected int
        found int
    }
    MissingField {
        field string
        record_type string
        required_fields []string
    }
    VariableRedefinition {
        variable string
        first_position ?Position
    }
    VariableShadowing {
        variable string
        parent_position ?Position
    }
    PatternMatchError(string)
}

// Erro de compilação
struct CompilationError {
    kind ErrorKind
    position Position
    message string
    suggestion ?string
    context ?string
}

// Result type para operações que podem falhar
type CompileResult = !string
type ParseResult = !Expr
type TypeCheckResult = !LxType
```

## Desafios Técnicos e Soluções

### 1. Lexer (Analisador Léxico)

**Desafio**: OCamlLex não tem equivalente em V

**Solução**: Implementar lexer manual com autômatos finitos

```v
// Estados do lexer
enum LexerState {
    Initial
    InIdentifier
    InNumber
    InFloat
    InString
    InComment
    InAtom
}

// Token
union Token {
    // Identificadores
    Ident(string)
    UpperIdent(string)

    // Literais
    String(string)
    Int(int)
    Float(f64)
    Bool(bool)
    Atom(string)
    Nil

    // Operadores
    Plus
    Minus
    Mult
    Div
    Eq
    Neq
    Lt
    Gt
    Leq
    Geq
    Assign
    Arrow
    Send
    Cons
    Pipe

    // Delimitadores
    LParen
    RParen
    LBrace
    RBrace
    LBracket
    RBracket
    Comma
    Semicolon
    Dot
    Colon

    // Palavras-chave
    Def
    Defp
    Fn
    Case
    If
    Else
    Do
    End
    When
    Receive
    After
    Worker
    Supervisor
    Record
    Unsafe

    // Especiais
    EOF
    Error(string)
}

// Lexer
struct Lexer {
    input []u8
    position int
    line int
    column int
    filename ?string
    state LexerState
}

fn (mut lexer Lexer) next_token() Token {
    // Implementação do autômato finito
    // ...
}
```

### 2. Parser (Analisador Sintático)

**Desafio**: Menhir não tem equivalente em V

**Solução**: Implementar parser recursivo descendente

```v
// Parser
struct Parser {
    tokens []Token
    position int
    current Token
    errors []CompilationError
}

// Tabela de precedência
struct PrecedenceTable {
    levels map[string]int
}

fn (mut parser Parser) parse_expression() !Expr {
    return parser.parse_expression_with_precedence(0)
}

fn (mut parser Parser) parse_expression_with_precedence(min_precedence int) !Expr {
    mut left := parser.parse_primary_expression()?

    for {
        if parser.position >= parser.tokens.len {
            break
        }

        op := parser.tokens[parser.position]
        precedence := parser.get_precedence(op)

        if precedence < min_precedence {
            break
        }

        parser.advance()
        right := parser.parse_expression_with_precedence(precedence + 1)?

        left = Expr.BinOp{
            left: left
            op: parser.token_to_string(op)
            right: right
        }
    }

    return left
}
```

### 3. Type Inference

**Desafio**: Sistema Hindley-Milner complexo

**Solução**: Adaptar algoritmos para V

```v
// Unificação de tipos
fn unify(t1 LxType, t2 LxType) !Substitution {
    match (t1, t2) {
        (LxType.TVar(v1), LxType.TVar(v2)) {
            if v1.id == v2.id {
                return Substitution{}
            } else {
                mut sub := Substitution{}
                sub.mappings[v1.id] = LxType.TVar(v2)
                return sub
            }
        }
        (LxType.TVar(v), t) {
            if occurs_check(v, t) {
                return error('Occurs check failed')
            }
            mut sub := Substitution{}
            sub.mappings[v.id] = t
            return sub
        }
        (t, LxType.TVar(v)) {
            return unify(LxType.TVar(v), t)
        }
        (LxType.TFun{param: p1, return: r1}, LxType.TFun{param: p2, return: r2}) {
            sub1 := unify(p1, p2)?
            sub2 := unify(apply_substitution(r1, sub1), apply_substitution(r2, sub1))?
            return compose_substitutions(sub1, sub2)
        }
        (LxType.TTuple(ts1), LxType.TTuple(ts2)) {
            if ts1.len != ts2.len {
                return error('Tuple arity mismatch')
            }
            mut sub := Substitution{}
            for i, t1 in ts1 {
                sub1 := unify(t1, ts2[i])?
                sub = compose_substitutions(sub, sub1)
            }
            return sub
        }
        (LxType.TList(t1), LxType.TList(t2)) {
            return unify(t1, t2)
        }
        (t1, t2) {
            if t1 == t2 {
                return Substitution{}
            } else {
                return error('Type mismatch: ${t1} vs ${t2}')
            }
        }
    }
}
```

### 4. Code Generation

**Desafio**: Geração eficiente de código Erlang

**Solução**: Templates e string builders

```v
// Gerador de código Erlang
struct ErlangGenerator {
    mut builder strings.Builder
    scope Scope
    indent int
}

// Escopo para renomeação de variáveis
struct Scope {
    variables map[string]string
    parent ?&Scope
    used_hashes map[string]bool
    var_record_types map[string]string
}

fn (mut gen ErlangGenerator) generate_expr(expr Expr) !string {
    match expr {
        Expr.Literal(lit) {
            return gen.generate_literal(lit)
        }
        Expr.Var(name) {
            return gen.scope.get_renamed_var(name)
        }
        Expr.Assign{name, value, position} {
            if gen.is_ignored_var(name) {
                return gen.generate_expr(value)
            }
            renamed := gen.scope.add_var(name)
            value_str := gen.generate_expr(value)?
            return '${renamed} = ${value_str}'
        }
        Expr.Send{target, message} {
            target_str := gen.generate_expr(target)?
            message_str := gen.generate_expr(message)?
            return '${target_str} ! ${message_str}'
        }
        Expr.Receive{clauses, timeout} {
            mut result := 'receive\n'
            gen.indent++

            for clause in clauses {
                result += gen.generate_receive_clause(clause)?
            }

            if timeout := timeout {
                result += gen.generate_timeout_clause(timeout)?
            }

            gen.indent--
            result += '\nend'
            return result
        }
        // ... outros casos
    }
}
```

## Plano de Implementação

### Fase 1: Fundação (2-3 semanas)

**Objetivos:**
- Setup do ambiente V
- Estruturas de dados básicas
- Sistema de erros
- Utilitários básicos

**Entregáveis:**
- [ ] Estrutura de projeto V
- [ ] Definições AST básicas
- [ ] Sistema de erros com Result types
- [ ] Utilitários de string e arquivo
- [ ] Testes unitários básicos

**Métricas:**
- Compilação sem erros
- Testes passando
- Documentação básica

### Fase 2: Lexer (1-2 semanas)

**Objetivos:**
- Implementação do analisador léxico
- Reconhecimento de todos os tokens LX
- Tratamento de strings e comentários

**Entregáveis:**
- [ ] Lexer manual em V
- [ ] Reconhecimento de tokens
- [ ] Tratamento de strings
- [ ] Tratamento de comentários
- [ ] Testes de casos edge

**Métricas:**
- 100% cobertura de tokens LX
- Performance comparável ao OCamlLex
- Tratamento correto de erros léxicos

### Fase 3: Parser (2-3 semanas)

**Objetivos:**
- Parser recursivo descendente
- Precedência de operadores
- Recuperação de erros

**Entregáveis:**
- [ ] Parser manual em V
- [ ] Tabela de precedência
- [ ] Recuperação de erros
- [ ] Validação de AST
- [ ] Testes de parsing

**Métricas:**
- Parse correto de toda sintaxe LX
- Mensagens de erro úteis
- Performance aceitável

### Fase 4: Type System (3-4 semanas)

**Objetivos:**
- Sistema de tipos básico
- Unificação de tipos
- Type inference
- Context management

**Entregáveis:**
- [ ] Sistema de tipos LX
- [ ] Algoritmo de unificação
- [ ] Type inference
- [ ] Context management
- [ ] Testes de type checking

**Métricas:**
- Type inference correto
- Mensagens de erro de tipo úteis
- Performance aceitável

### Fase 5: Compiler (2-3 semanas)

**Objetivos:**
- Geração de código Erlang
- Gerenciamento de escopo
- Templates de código

**Entregáveis:**
- [ ] Gerador de código Erlang
- [ ] Gerenciamento de escopo
- [ ] Templates de código
- [ ] Otimizações básicas
- [ ] Testes de geração

**Métricas:**
- Código Erlang válido
- Compatibilidade com OTP
- Performance de compilação

### Fase 6: Linter & OTP (2-3 semanas)

**Objetivos:**
- Análise estática
- Validação OTP
- Regras de linting

**Entregáveis:**
- [ ] Linter básico
- [ ] Validação OTP
- [ ] Regras de linting
- [ ] Sugestões de correção
- [ ] Testes de linting

**Métricas:**
- Detecção de problemas
- Sugestões úteis
- Performance aceitável

### Fase 7: Project Generation (1-2 semanas)

**Objetivos:**
- Geração de .app.src
- Configuração Rebar
- Templates de projeto

**Entregáveis:**
- [ ] Gerador de .app.src
- [ ] Configuração Rebar
- [ ] Templates de projeto
- [ ] CLI interface
- [ ] Testes de geração

**Métricas:**
- Projetos válidos
- Compatibilidade com Rebar
- Interface CLI funcional

### Fase 8: Testing & Polish (2-3 semanas)

**Objetivos:**
- Testes de integração
- Performance optimization
- Documentação completa

**Entregáveis:**
- [ ] Testes de integração
- [ ] Performance optimization
- [ ] Documentação completa
- [ ] Bug fixes
- [ ] Release preparation

**Métricas:**
- Cobertura de testes > 90%
- Performance melhorada
- Documentação completa

## Considerações de Performance

### Memory Management

**Estratégias:**
- Usar structs stack-allocated quando possível
- Minimizar heap allocations
- Implementar object pooling para estruturas frequentes

**Otimizações:**
```v
// Object pooling para strings frequentes
struct StringPool {
    mut pool map[string]string
}

fn (mut sp StringPool) intern(s string) string {
    if cached := sp.pool[s] {
        return cached
    }
    sp.pool[s] = s
    return s
}
```

### String Operations

**Estratégias:**
- Usar `strings.Builder` para concatenação
- Implementar string pooling
- Otimizar operações de string

**Otimizações:**
```v
// Builder otimizado para código Erlang
struct ErlangBuilder {
    mut builder strings.Builder
    indent_level int
}

fn (mut eb ErlangBuilder) write_indent() {
    for i := 0; i < eb.indent_level; i++ {
        eb.builder.write_string('    ')
    }
}
```

### Data Structures

**Escolhas:**
- Arrays para listas pequenas
- Maps para lookup frequente
- Structs para dados estruturados

**Otimizações:**
```v
// Cache para type variables
struct TypeCache {
    mut cache map[string]LxType
}

fn (mut tc TypeCache) get_or_create(key string, creator fn() LxType) LxType {
    if cached := tc.cache[key] {
        return cached
    }
    result := creator()
    tc.cache[key] = result
    return result
}
```

## Riscos e Mitigações

### Risco 1: Complexidade do Type System

**Risco:** Sistema Hindley-Milner pode ser muito complexo para implementar em V

**Mitigação:**
- Implementar versão simplificada primeiro
- Usar generics V quando apropriado
- Manter compatibilidade com testes existentes

**Fallback:** Usar type checking básico se necessário

### Risco 2: Performance do Parser

**Risco:** Parser manual pode ser mais lento que Menhir

**Mitigação:**
- Profiling e otimização
- Implementar caching quando apropriado
- Usar estruturas de dados eficientes

**Fallback:** Implementar parser mais simples

### Risco 3: Compatibilidade com Erlang

**Risco:** Código gerado pode não ser compatível com Erlang/OTP

**Mitigação:**
- Testes extensivos com código Erlang real
- Validação com compilador Erlang
- Manter testes de regressão

**Fallback:** Manter versão OCaml como backup

### Risco 4: Complexidade da Migração

**Risco:** Migração pode ser mais complexa que esperado

**Mitigação:**
- Implementação incremental
- Validação contínua
- Manter compatibilidade com testes

**Fallback:** Migração parcial se necessário

## Métricas de Sucesso

### Funcionalidade

- [ ] 100% compatibilidade com sintaxe LX existente
- [ ] Geração correta de código Erlang
- [ ] Validação OTP completa
- [ ] Todas as funcionalidades do compilador OCaml

### Performance

- [ ] Compilação mais rápida que versão OCaml
- [ ] Menor uso de memória
- [ ] Startup time otimizado
- [ ] Compilação incremental eficiente

### Qualidade

- [ ] Cobertura de testes > 90%
- [ ] Zero regressões de funcionalidade
- [ ] Documentação completa
- [ ] Código limpo e bem estruturado

### Usabilidade

- [ ] Interface CLI funcional
- [ ] Mensagens de erro úteis
- [ ] Integração com IDEs
- [ ] Ferramentas de desenvolvimento

## Cronograma Detalhado

### Semana 1-2: Setup e Fundação
- Setup do ambiente V
- Estrutura de projeto
- Definições AST básicas
- Sistema de erros

### Semana 3-4: Lexer
- Implementação do lexer
- Reconhecimento de tokens
- Testes de lexer

### Semana 5-7: Parser
- Parser recursivo descendente
- Precedência de operadores
- Testes de parser

### Semana 8-11: Type System
- Sistema de tipos
- Unificação
- Type inference
- Testes de type checking

### Semana 12-14: Compiler
- Geração de código
- Gerenciamento de escopo
- Testes de compilação

### Semana 15-17: Linter & OTP
- Análise estática
- Validação OTP
- Testes de linting

### Semana 18-19: Project Generation
- Geração de projetos
- CLI interface
- Testes de geração

### Semana 20-22: Testing & Polish
- Testes de integração
- Performance optimization
- Documentação
- Release preparation

## Conclusão

A reescrita do compilador LX em V é um projeto ambicioso mas viável. A arquitetura modular do compilador atual facilita a migração incremental, e as características da linguagem V (como Result types, generics e structs) são bem adequadas para a implementação.

O plano de 22 semanas fornece uma rota clara para a migração completa, com validação contínua e mitigação de riscos. A manutenção da compatibilidade com o código existente e a preservação de todas as funcionalidades são prioridades absolutas.

O resultado final será um compilador LX mais rápido, mais fácil de manter e com melhor integração com o ecossistema V, mantendo toda a funcionalidade existente.