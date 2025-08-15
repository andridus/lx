# LX1 Implementation Roadmap

## Visão Geral

O LX1 é uma implementação incremental e simplificada do compilador LX, usando a nova AST minimalista baseada em `Node`. Cada task é autossuficiente e implementa Lexer → Parser → Analysis → Generator de forma completa.

## Estrutura do Projeto LX1

```
lx1/
├── docs/
│   ├── tasks/           # Documentação de cada task
│   └── examples/        # Exemplos de código para cada task
├── lx1/
│   ├── ast/            # AST minimalista (Node)
│   ├── lexer/          # Lexer unificado
│   ├── parser/         # Parser minimalista
│   ├── analysis/       # Análise simplificada
│   ├── generator/      # Gerador Erlang unificado
│   └── main.v          # CLI principal
├── tests/              # Testes para cada task
└── examples/           # Exemplos práticos
```

## Roadmap por Complexidade

### BÁSICO (Fundação)

#### **TASK 1: Funções com Literais**
- **Objetivo**: Funções simples com apenas literais como corpo
- **Duração**: 2-3 dias
- **Código**: `def nome() do literal end`
- **Exemplos**: `def answer() do 42 end`
- **Componentes**: AST básica, Lexer mínimo, Parser simples, Analysis básica, Generator mínimo

#### **TASK 2: Variáveis Locais**
- **Objetivo**: Binding e uso de variáveis dentro de funções
- **Duração**: 2-3 dias
- **Código**: `x = 42; x`
- **Exemplos**: `def calc() do x = 10; x end`
- **Componentes**: Binding nodes, Scope checker, Variable resolution

#### **TASK 3: Chamada de Funções**
- **Objetivo**: Operadores binários (+, -, *, /) são na verdade chamadas de funcoes
- **Duração**: 2-3 dias
- **Código**: `1 + 2 * 3`
- **Exemplos**: `def sum() do a = 5; b = 3; a + b end`
- **Componentes**: Binary operators, Precedence, Type checking

#### **TASK 4: Diretivas de compilador**
- **Objetivo**: Permitir o codigo lx influenciar no compilador
- **Duração**: 2-3 dias
- **Código**: `$print(x)`
- **Exemplos**: `def sum() do a = 5; $print(a) end`
- **Componentes**: Binary operators, Precedence, Type checking
### ESTRUTURAS (Dados)

#### **TASK 5: Listas Simples**
- **Objetivo**: Criação e acesso a listas
- **Duração**: 3-4 dias
- **Código**: `[1, 2, 3]`
- **Exemplos**: `def numbers() do [1, 2, 3] end`
- **Componentes**: List nodes, List literals, Basic list operations

#### **TASK 6: Tuplas**
- **Objetivo**: Criação e acesso a tuplas
- **Duração**: 2-3 dias
- **Código**: `{1, 2, 3}`
- **Exemplos**: `def point() do {10, 20} end`
- **Componentes**: Tuple nodes, Tuple literals

#### **TASK 7: Maps Básicos**
- **Objetivo**: Maps com chaves atom
- **Duração**: 3-4 dias
- **Código**: `%{name: "João", age: 30}`
- **Exemplos**: `def user() do %{id: 1, name: "Ana"} end`
- **Componentes**: Map nodes, Map literals, Key-value pairs

#### **TASK 8: Records** ✅
- **Objetivo**: Definição e uso de records
- **Duração**: 5-6 dias
- **Código**: `record User { name :: string }`
- **Exemplos**: Records básicos sem herança
- **Componentes**: Record definitions, Record literals, Field access, Type checking

#### **TASK 9: Implementacao de funcoes com argumentos e multiplos head** ✅
- **Objetivo**: Funcoes podem ter argumentos e multiplos heads
- **Duração**: 5-6 dias
- **Código**: `def a() do 1 end\n def b(arg1) do arg1 end\n def c do () -> 1\n(arg) -> arg end`
- **Exemplos**: `
  def a do
    1
  end

  def b(arg1, arg2) do
    arg1 + arg2
  end

  def b1(arg1 :: type1, arg2 :: type2) :: typeb1 do
    arg1 + arg2
  end

  def c do
    () -> 1
    (arg1, arg2) -> arg1 + arg2
    (arg1 :: integer, arg2) :: integer -> arg1 + arg2
  end
`
- **Componentes**: Function, Blocks, Type checking

#### **TASK 10: Guards Simples**
- **Objetivo**: Guards em funções e case
- **Duração**: 3-4 dias
- **Código**: `def func(x) when x > 0 do ... end`
- **Exemplos**: Guards com comparações básicas
- **Componentes**: Guard expressions, Guard evaluation
### CONTROLE (Fluxo)

#### **TASK 11: Condicionais Simples (if)**
- **Objetivo**: Expressões if/else
- **Duração**: 3-4 dias
- **Código**: `if cond do expr1 else expr2 end`
- **Exemplos**: `def check(x) do if x > 0 do "pos" else "neg" end end`
- **Componentes**: Conditional nodes, Boolean evaluation

#### **TASK 12: Pattern Matching Básico (case)**
- **Objetivo**: Case expressions com patterns simples
- **Duração**: 4-5 dias
- **Código**: `case x do 1 -> "one"; _ -> "other" end`
- **Exemplos**: Pattern matching com literais
- **Componentes**: Case nodes, Pattern nodes, Pattern matching

#### **TASK 13: Chamadas de Função**
- **Objetivo**: Chamadas entre funções do mesmo módulo
- **Duração**: 3-4 dias
- **Código**: `func1(); func2(arg)`
- **Exemplos**: `def main() do helper() end; def helper() do 42 end`
- **Componentes**: Function calls, Function resolution

### ESTRUTURAS AVANÇADAS

#### **TASK 17: List Comprehensions**
- **Objetivo**: For expressions básicas
- **Duração**: 4-5 dias
- **Código**: `for x in list do x * 2 end`
- **Exemplos**: Transformações de listas
- **Componentes**: For nodes, List iteration

#### **TASK 18: Pattern Matching Avançado**
- **Objetivo**: Destructuring de listas e tuplas
- **Duração**: 4-5 dias
- **Código**: `case {a, b} do {1, x} -> x; _ -> 0 end`
- **Exemplos**: Pattern matching complexo
- **Componentes**: Advanced patterns, Destructuring


### MÓDULOS E ORGANIZAÇÃO

#### **TASK 19: Múltiplas Funções**
- **Objetivo**: Módulos com múltiplas funções
- **Duração**: 2-3 dias
- **Código**: Múltiplas definições de função
- **Exemplos**: Módulos organizados
- **Componentes**: Multiple function definitions

#### **TASK 20: Exports Básicos**
- **Objetivo**: Controle de visibilidade
- **Duração**: 3-4 dias
- **Código**: Funções públicas e privadas
- **Exemplos**: Modules com interface controlada
- **Componentes**: Export lists, Visibility control

### CONCORRÊNCIA (OTP Básico)

#### **TASK 21: Send/Receive Básico**
- **Objetivo**: Message passing simples
- **Duração**: 5-6 dias
- **Código**: `pid ! message; receive pattern -> action end`
- **Exemplos**: Comunicação básica entre processos
- **Componentes**: Send/receive nodes, Message handling

#### **TASK 22: Spawning Simples**
- **Objetivo**: Criação de processos
- **Duração**: 4-5 dias
- **Código**: `spawn(fun)`
- **Exemplos**: Processes básicos
- **Componentes**: Process creation, PID management

### INTEGRAÇÕES

#### **TASK 23: Chamadas Externas**
- **Objetivo**: Chamadas para módulos Erlang
- **Duração**: 3-4 dias
- **Código**: `:erlang.now()`
- **Exemplos**: Integração com stdlib do Erlang
- **Componentes**: External calls, Module resolution

#### **TASK 24 : Aplicações OTP**
- **Objetivo**: Estrutura básica de aplicação
- **Duração**: 6-7 dias
- **Código**: `application`, `supervisor`, `gen_server`
- **Exemplos**: Aplicação OTP mínima
- **Componentes**: OTP behaviors, Application structure

## Critérios de Sucesso para Cada Task

### Critérios Técnicos
- [ ] **Lexer**: Reconhece todos os tokens necessários
- [ ] **Parser**: Gera AST correta para os construtos
- [ ] **Analysis**: Type checking e scope checking funcionam
- [ ] **Generator**: Gera código Erlang válido e executável
- [ ] **Testes**: 100% dos testes passando

### Critérios de Qualidade
- [ ] **Código Limpo**: Legível e bem documentado
- [ ] **Performance**: Tempo de compilação razoável
- [ ] **Compatibilidade**: Saída compatível com Erlang/OTP
- [ ] **Debugging**: Mensagens de erro claras
- [ ] **Exemplos**: Funcionam como esperado

## Estratégia de Implementação

### Desenvolvimento Incremental
1. Implementar uma task por vez
2. Não prosseguir sem testes 100% funcionando
3. Cada task é autossuficiente
4. Reusar código das tasks anteriores

### Testes First
1. Escrever testes antes da implementação
2. Usar exemplos do LX atual como inspiração
3. Cada task tem sua própria suite de testes
4. Testes de regressão para tasks anteriores

### Qualidade e Performance
1. Code review para cada task
2. Benchmarks de performance
3. Profiling de memory usage
4. Compatibilidade com Erlang validada

## Estimativas

- **Total de Tasks**: 21
- **Duração Estimada**: 75-95 dias (15-19 semanas)
- **Duração Média por Task**: 3.6-4.5 dias
- **Milestone Major**: A cada 5 tasks
- **Reviews**: A cada task completada

## Próximos Passos

1. **Aprovação do Roadmap**: Review e ajustes
2. **Setup do Projeto**: Estrutura inicial do lx1
3. **Task 1 Kick-off**: Implementação da primeira task
4. **Process Definition**: Definir workflow de desenvolvimento

---

Este roadmap garante uma progressão lógica e incremental, construindo complexidade gradualmente enquanto mantém cada etapa testável e funcional.