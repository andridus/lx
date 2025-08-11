# Plano de Refatoração da AST do Compilador LX

## Sumário Executivo

Este documento detalha o plano para simplificar e unificar a AST (Abstract Syntax Tree) do compilador LX, reduzindo a complexidade de ~35 tipos de expressões para uma estrutura minimalista baseada em um único tipo `Node`, mantendo a semântica correta das linguagens funcionais.

## Motivação

### Problemas Atuais na AST Existente
- **35+ tipos de Expr** diferentes (VariableExpr, LiteralExpr, BinaryExpr, CallExpr, etc.)
- **10+ tipos de Pattern** diferentes
- **Múltiplos tipos redundantes** para operações similares
- **Complexidade desnecessária** no parser (2400+ linhas só em block_expressions.v)
- **Análise semântica dispersa** entre múltiplos arquivos
- **Backend complexo** com geradores específicos para cada tipo
- **Dificuldade de manutenção** e extensão da linguagem

### Estrutura Atual do Compilador

O compilador LX é composto por:

#### 1. Frontend
- **Lexer** (`frontend/lexer/`): Tokenização com suporte a múltiplos tipos de tokens
- **Parser** (`frontend/parser/`): Parser complexo com 2400+ linhas, múltiplos contextos
  - `block_expressions.v`: Parsing de expressões (2415 linhas)
  - `pattern_expressions.v`: Parsing de patterns
  - `function_clauses.v`: Parsing de funções
  - `module_statements.v`: Parsing de módulos

#### 2. Analysis
- **HM Type Inferencer** (`analysis/hm_*.v`): Sistema Hindley-Milner complexo (1674 linhas)
- **Variable Checker** (`analysis/variable_*.v`): Checagem de escopo
- **Linter** (`analysis/linter_main.v`): 485 linhas de regras de lint
- **Type Table** (`analysis/type_table.v`): Gerenciamento de tipos por AST ID

#### 3. Backend
- **Erlang Generator** (`backend/erlang/`): Geração de código Erlang
- **Múltiplos geradores** específicos para cada tipo de AST

### Expressões Redundantes Identificadas

| Categoria | Tipos Atuais | Problemas |
|-----------|--------------|-----------|
| **Variáveis** | `VariableExpr`, `AssignExpr` | Confusão entre uso e definição |
| **Operadores** | `BinaryExpr`, `UnaryExpr`, `CallExpr` | Operadores não são chamadas unificadas |
| **Estruturas** | `RecordLiteralExpr`, `MapLiteralExpr`, `ListLiteralExpr`, `TupleExpr` | Estruturas similares com códigos duplicados |
| **Acessos** | `RecordAccessExpr`, `MapAccessExpr` | Acessos similares tratados separadamente |
| **Pattern Matching** | `IfExpr`, `CaseExpr`, `WithExpr`, `MatchExpr`, `SimpleMatchExpr`, `MatchRescueExpr` | Múltiplos tipos para mesmo conceito |
| **Controle** | Múltiplos tipos de controle de fluxo | Semântica similar dispersa |

## Nova AST Minimalista

### Estrutura Unificada

```v
// Um único tipo de nó para toda a AST
Node {
   id   : int            // ID único para mapeamento no TypeTable (equivalente ao ast_id atual)
   kind : .bind | .record | .map | .integer | .atom | .tuple | .function | .call | .match | .ident | .literal | ...
   value: any            // valor do literal ou nome do identificador, se aplicável
   opts: FunctionOpts | CallerOpts | MapOpts | ... // opções específicas do nó, se necessário
   left :  AstNode       // filho esquerdo (ou único filho, para unários)
   right : AstNode       // filho direito (ou lista de filhos, para n-ários)
   position : Position
   type : Type           // tipo inferido ou declarado
}

// Tipo raiz da AST
AstNode = []Node | Node | Literal
```

### Convenções da Nova AST

- **id**: ID único para mapeamento no TypeTable (substitui o ast_id atual)
- **kind**: Define o tipo lógico do nó (.call, .bind, .ident, .integer, .record, etc.)
- **value**: Armazena valores de literais (42, "hello") ou nomes de identificadores ("x")
- **opts**: Opções específicas do nó (CallerOpts, RecordOpts, etc.) quando necessário
- **left/right**: Filhos do nó (podem ser listas ou nulos conforme o caso)
- **position**: Posição no código fonte para debug
- **type**: Tipo inferido pelo sistema de tipos

### Exemplos de Mapeamento

#### Identificadores e Literais
```v
// ANTES
VariableExpr{name: "x", position: pos, ast_id: 123}
LiteralExpr{value: IntegerLiteral{42}, position: pos, ast_id: 124}

// DEPOIS
Node{id: 123, kind: .ident, value: "x", position: pos}
Node{id: 124, kind: .integer, value: 42, position: pos}
```

#### Operações Binárias
```v
// ANTES
BinaryExpr{
    left: VariableExpr{name: "a"},
    op: .add,
    right: VariableExpr{name: "b"}
}

// DEPOIS
Node{
    id: 125,
    kind: .call,
    opts: CallerOpts{function: "+", module: "erlang"},
    left: Node{id: 126, kind: .ident, value: "a"},
    right: Node{id: 127, kind: .ident, value: "b"}
}
```

#### Estruturas de Dados
```v
// ANTES
RecordLiteralExpr{name: "Person", fields: [...]}
MapLiteralExpr{entries: [...]}
ListLiteralExpr{elements: [...]}

// DEPOIS
Node{id: 128, kind: .record, opts: RecordOpts{name: "Person"}, left: [field_nodes]}
Node{id: 129, kind: .map, left: [entry_nodes]}
Node{id: 130, kind: .list, left: [element_nodes]}
```

#### Binding vs Referência
```v
// ANTES (confuso)
VariableExpr{name: "x"} // Usado para ambos

// DEPOIS (claro)
// Definição: x = 42
Node{
    id: 131,
    kind: .bind,
    left: Node{id: 132, kind: .ident, value: "x"},
    right: Node{id: 133, kind: .integer, value: 42}
}

// Uso: x + 1
Node{
    id: 134,
    kind: .call,
    opts: CallerOpts{function: "+"},
    left: Node{id: 135, kind: .ident, value: "x"},
    right: Node{id: 136, kind: .integer, value: 1}
}
```

## Plano de Implementação Detalhado

### Fase 1: Preparação e Nova AST (3-4 dias)

#### 1.1 Backup e Setup
- [ ] Backup completo do código atual
- [ ] Criar branch `ast-refactoring`
- [ ] Criar `ast/ast_v2.v` com nova estrutura

#### 1.2 Implementar Nova AST Core
```v
// ast/ast_v2.v
pub struct Node {
pub:
    id       int       // ID único para TypeTable (substitui ast_id)
    kind     NodeKind
    value    any
    opts     NodeOpts
    left     AstNode
    right    AstNode
    position Position
    type_    Type
}

pub enum NodeKind {
    // Literais
    integer, float, string, boolean, atom, nil
    // Identificadores
    ident
    // Operações
    call, bind
    // Estruturas
    record, map, list, tuple
    // Controle
    match, case, if, with, for
    // Outros
    block, function, receive, send
}

pub type NodeOpts = CallerOpts | RecordOpts | MapOpts | MatchOpts | BlockOpts
pub type AstNode = Node | []Node | Literal
```

#### 1.3 Definir Opts Específicos
```v
pub struct CallerOpts {
    function string
    module   string
    external bool
}

pub struct RecordOpts {
    name   string
    fields []RecordField
}

pub struct MapOpts {
    entries []MapEntry
    is_update bool
    base_map AstNode
}

pub struct MatchOpts {
    patterns []Pattern
    guards   []Guard
    rescue   ?RescueClause
}
```

#### 1.4 Sistema de IDs
```v
// ast/id_generator.v
pub struct IDGenerator {
mut:
    next_id int = 1
}

pub fn (mut gen IDGenerator) generate() int {
    id := gen.next_id
    gen.next_id++
    return id
}

// Função global para criar nodes com ID automático
pub fn new_node(kind NodeKind, value any) Node {
    return Node{
        id: global_id_gen.generate(),
        kind: kind,
        value: value,
        position: Position{},
        type_: Type{}
    }
}
```

#### 1.5 Utilitários de Conversão
- [ ] Criar funções de conversão AST antiga → nova
- [ ] Implementar métodos `str()` para debug
- [ ] Sistema de geração automática de IDs
- [ ] Testes unitários das novas estruturas

### Fase 2: Parser Refatorado (5-6 dias)

#### 2.1 Análise do Parser Atual
O parser atual tem **complexidade desnecessária**:
- `block_expressions.v`: 2415 linhas para parsing de expressões
- Múltiplos contextos (.mod, .expression)
- Parsing específico para cada tipo de AST
- Precedência de operadores dispersa

#### 2.2 Novo Parser Simplificado
```v
// frontend/parser/unified_parser.v
pub struct UnifiedParser {
mut:
    lexer   Lexer
    current Token
    pos     int
}

pub fn (mut p UnifiedParser) parse_node() ?Node {
    return match p.current {
        IdentToken { p.parse_ident() }
        IntToken { p.parse_integer() }
        OperatorToken { p.parse_operator() }
        KeywordToken { p.parse_keyword() }
        else { p.parse_literal() }
    }
}
```

#### 2.3 Parsing Unificado por Tipo
- [ ] **Literais**: `parse_literal()` → `Node{kind: .integer/.string/etc, value: val}`
- [ ] **Identificadores**: `parse_ident()` → `Node{kind: .ident, value: name}`
- [ ] **Operadores**: `parse_operator()` → `Node{kind: .call, opts: CallerOpts{}}`
- [ ] **Estruturas**: `parse_structure()` → `Node{kind: .record/.map/.list}`
- [ ] **Controle**: `parse_control()` → `Node{kind: .match/.case/.if}`

#### 2.4 Precedência Unificada
```v
fn (mut p UnifiedParser) parse_binary_expr(min_prec int) ?Node {
    mut left := p.parse_unary()?

    while p.is_binary_op() && p.get_precedence() >= min_prec {
        op := p.advance()
        right := p.parse_binary_expr(p.get_precedence() + 1)?

        left = Node{
            kind: .call,
            opts: CallerOpts{function: op.str()},
            left: left,
            right: right
        }
    }
    return left
}
```

#### 2.5 Eliminação de Contextos
- [ ] Remover contextos `.mod` vs `.expression`
- [ ] Parser único que decide tipo por token atual
- [ ] Simplificar lógica de delimitadores

#### 2.6 Novos Arquivos Parser
- [ ] `unified_parser.v`: Parser principal (300-400 linhas)
- [ ] `node_builders.v`: Construtores de Node específicos
- [ ] `precedence.v`: Tabela de precedência unificada
- [ ] `token_handlers.v`: Handlers específicos por token

### Fase 3: Analysis Refatorado (4-5 dias)

#### 3.1 Análise do Analysis Atual
O sistema atual é **excessivamente complexo**:
- `hm_inferencer.v`: 1674 linhas de inferência HM
- `type_table.v`: 254 linhas de gerenciamento de tipos
- Múltiplos arquivos para diferentes aspectos (substitution, unification)
- Análise dispersa entre vários componentes

#### 3.2 Novo Analysis Simplificado

##### 3.2.1 Visitor Pattern para Node
```v
// analysis/node_visitor.v
pub interface NodeVisitor {
    visit_node(node Node) ?Node
    visit_children(node Node) ?[]Node
}

pub struct TypeInferenceVisitor {
mut:
    type_env map[string]Type
    errors   []Error
}

pub fn (mut v TypeInferenceVisitor) visit_node(node Node) ?Node {
    return match node.kind {
        .ident { v.visit_ident(node) }
        .call { v.visit_call(node) }
        .bind { v.visit_bind(node) }
        .record { v.visit_record(node) }
        else { v.visit_literal(node) }
    }
}
```

##### 3.2.2 Type Inference Unificado
```v
// analysis/type_inference.v
pub fn (mut v TypeInferenceVisitor) visit_call(node Node) ?Node {
    opts := node.opts as CallerOpts

    // Inferir tipos dos argumentos
    left_type := v.infer_type(node.left)?
    right_type := v.infer_type(node.right)?

    // Determinar tipo do resultado
    result_type := match opts.function {
        '+', '-', '*', '/' {
            v.unify_numeric_types(left_type, right_type)?
        }
        '==', '!=', '<', '>' {
            Type.boolean
        }
        else {
            v.lookup_function_type(opts.function)?
        }
    }

    // Armazenar tipo no TypeTable usando o ID do node
    v.type_table.assign_type(node.id, result_type)

    return Node{
        ...node,
        type_: result_type
    }
}
```

##### 3.2.3 Simplificação do HM System
```v
// analysis/hm_simple.v
pub struct SimpleHM {
mut:
    type_vars map[string]Type
    constraints []Constraint
}

pub fn (mut hm SimpleHM) infer(node Node) ?Type {
    return match node.kind {
        .ident { hm.infer_ident(node) }
        .call { hm.infer_call(node) }
        .bind { hm.infer_bind(node) }
        else { hm.infer_literal(node) }
    }
}
```

#### 3.3 Compatibilidade com TypeTable Existente
```v
// O TypeTable atual usa ast_id, agora usará node.id
// analysis/type_table_adapter.v
pub fn (mut tt TypeTable) assign_node_type(node Node, type_info TypeInfo) {
    tt.assign_type(node.id, type_info)  // Usa node.id ao invés de ast_id
}

pub fn (tt TypeTable) get_node_type(node Node) ?TypeInfo {
    return tt.get_type(node.id)  // Busca por node.id
}

// Função helper para extrair ID de qualquer node
pub fn get_node_id(node Node) int {
    return node.id  // Substitui get_expr_ast_id()
}
```

#### 3.4 Novos Arquivos Analysis
- [ ] `node_visitor.v`: Interface visitor para Node (100 linhas)
- [ ] `type_inference.v`: Inferência de tipos unificada (300 linhas)
- [ ] `hm_simple.v`: HM simplificado (200 linhas)
- [ ] `scope_checker.v`: Checagem de escopo (150 linhas)
- [ ] `type_table_adapter.v`: Adaptador para TypeTable (50 linhas)
- [ ] `semantic_analyzer.v`: Coordenador principal (200 linhas)

### Fase 4: Backend Refatorado (4-5 dias)

#### 4.1 Análise do Backend Atual
- Múltiplos arquivos para diferentes tipos de AST
- `expressions.v`, `statements.v`, `patterns.v` separados
- Lógica de geração dispersa

#### 4.2 Novo Backend Unificado

##### 4.2.1 Generator Unificado
```v
// backend/erlang/unified_generator.v
pub struct UnifiedGenerator {
mut:
    output strings.Builder
    indent int
}

pub fn (mut g UnifiedGenerator) generate_node(node Node) string {
    return match node.kind {
        .ident { g.generate_ident(node) }
        .call { g.generate_call(node) }
        .bind { g.generate_bind(node) }
        .record { g.generate_record(node) }
        .integer { node.value.str() }
        .string { '"${node.value}"' }
        else { g.generate_literal(node) }
    }
}
```

##### 4.2.2 Geração por Tipo de Node
```v
fn (mut g UnifiedGenerator) generate_call(node Node) string {
    opts := node.opts as CallerOpts
    left := g.generate_node(node.left)
    right := g.generate_node(node.right)

    return match opts.function {
        '+' { '${left} + ${right}' }
        '==' { '${left} =:= ${right}' }
        '!=' { '${left} =/= ${right}' }
        else {
            if opts.external {
                '${opts.module}:${opts.function}(${left}, ${right})'
            } else {
                '${opts.function}(${left}, ${right})'
            }
        }
    }
}

fn (mut g UnifiedGenerator) generate_bind(node Node) string {
    left := g.generate_node(node.left)   // variável
    right := g.generate_node(node.right)  // valor
    return '${left} = ${right}'
}
```

#### 4.3 Novos Arquivos Backend
- [ ] `unified_generator.v`: Gerador principal (400 linhas)
- [ ] `node_generators.v`: Geradores específicos por tipo
- [ ] `erlang_mapping.v`: Mapeamento LX → Erlang
- [ ] `code_formatter.v`: Formatação do código gerado

### Fase 5: Integração e Migração (3-4 dias)

#### 5.1 Adaptação dos Testes
- [ ] Atualizar ~30 arquivos de teste para nova AST
- [ ] Manter compatibilidade com utilitários existentes
- [ ] Adicionar testes específicos para Node

#### 5.2 Adaptação das Utilities
```v
// Adaptar utils/debug.v para Node
pub fn debug_node(node Node) string {
    return match node.kind {
        .ident { 'Ident(${node.value})' }
        .call {
            opts := node.opts as CallerOpts
            'Call(${opts.function})'
        }
        .bind { 'Bind(${node.left} = ${node.right})' }
        else { 'Node(${node.kind})' }
    }
}
```

#### 5.3 Migração Incremental
- [ ] Fase 5.1: Parser novo coexistindo com antigo
- [ ] Fase 5.2: Analysis adaptado para ambas ASTs
- [ ] Fase 5.3: Backend adaptado para ambas ASTs
- [ ] Fase 5.4: Remoção da AST antiga

### Fase 6: Otimização e Limpeza (2-3 dias)

#### 6.1 Performance
- [ ] Benchmarks antes/depois
- [ ] Otimização de alocações
- [ ] Cache de Nodes comuns

#### 6.2 Limpeza
- [ ] Remover arquivos antigos da AST
- [ ] Consolidar imports
- [ ] Atualizar documentação

#### 6.3 Testes Finais
- [ ] Testes end-to-end completos
- [ ] Verificação de todos os exemplos em `ex/`
- [ ] Testes de regressão

## Benefícios Esperados

### Quantitativos
- **Redução de 80%+ nos tipos de AST** (de 35+ para 1 Node + variantes)
- **Redução de 70%+ no código do parser** (de 2400+ para ~800 linhas)
- **Redução de 60%+ no código de analysis** (de 3000+ para ~1000 linhas)
- **Redução de 50%+ no código do backend**
- **Melhoria de performance** em parsing e análise

### Qualitativos
- **Código mais limpo e maintível**
- **Semântica mais consistente** com linguagens funcionais
- **Facilidade para adicionar novas features**
- **Debugging e desenvolvimento mais eficientes**
- **Onboarding mais simples** para novos desenvolvedores

## Riscos e Mitigações

### Riscos Identificados
- **Quebra de compatibilidade**: Toda a pipeline precisa ser atualizada
- **Bugs durante transição**: Lógica complexa de conversão
- **Performance temporária**: Nova estrutura pode ser inicialmente menos eficiente
- **Complexidade das opts**: Tipos union podem ser confusos

### Estratégias de Mitigação
- **Implementação incremental**: Uma fase por vez com testes
- **Dual AST temporária**: Manter ambas durante transição
- **Testes extensivos**: Comparar saída antes/depois para todos os casos
- **Backup completo**: Possibilidade de rollback a qualquer momento
- **Profiling contínuo**: Monitorar performance durante transição
- **Code review rigoroso**: Revisão de cada fase antes de prosseguir

## Métricas de Sucesso

### Quantitativas
- [ ] Redução de 80%+ nos tipos de AST
- [ ] Redução de 70%+ no código do parser
- [ ] Redução de 60%+ no código de analysis
- [ ] Redução de 50%+ no código do backend
- [ ] Manutenção ou melhoria da performance (±5%)
- [ ] 100% dos testes passando
- [ ] Redução de 50%+ no tempo de compilação

### Qualitativas
- [ ] Código mais legível e maintível
- [ ] Semântica consistente com linguagens funcionais
- [ ] Facilidade para adicionar novas features
- [ ] Debugging mais simples
- [ ] Documentação mais clara
- [ ] Onboarding mais rápido para novos desenvolvedores

## Cronograma Detalhado

| Fase | Duração | Marcos Principais |
|------|---------|------------------|
| **Fase 1: Preparação** | 3-4 dias | Nova AST definida, utilitários criados |
| **Fase 2: Parser** | 5-6 dias | Parser unificado funcionando com testes básicos |
| **Fase 3: Analysis** | 4-5 dias | Type inference e checkers adaptados |
| **Fase 4: Backend** | 4-5 dias | Geração Erlang funcionando |
| **Fase 5: Integração** | 3-4 dias | Todos os testes passando |
| **Fase 6: Limpeza** | 2-3 dias | Código limpo e documentado |
| **TOTAL** | **21-27 dias** | **AST minimalista completa** |

## Conclusão

Esta refatoração representa uma **simplificação fundamental e modernização** da AST do compilador LX. A mudança de 35+ tipos específicos para uma estrutura minimalista baseada em `Node` resultará em:

- **Código drasticamente mais simples** e maintível
- **Semântica mais consistente** e alinhada com linguagens funcionais
- **Base sólida** para futuras extensões da linguagem
- **Desenvolvimento mais ágil** e debugging mais eficiente
- **Onboarding mais rápido** para novos desenvolvedores

O investimento de 3-4 semanas de desenvolvimento resultará em uma base de código mais robusta, simples e extensível que beneficiará o projeto a longo prazo.