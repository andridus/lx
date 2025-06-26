# Fase 5: Codegen (Geração de Código) - Tarefas e Testes

## Objetivos
Gerar código Erlang válido e eficiente a partir do AST LX, suportando todas as construções da linguagem.

## Tarefas

### T5.1: Estrutura Base do Codegen
**Descrição**: Implementar estrutura base para geração de código
**Entregáveis**:
- [ ] `compiler/compiler.v` - Estrutura principal do gerador
- [ ] `compiler/scope.v` - Gerenciamento de escopo e variáveis
- [ ] `compiler/erlang.v` - Templates de código Erlang
- [ ] `compiler/optimizer.v` - Otimizações básicas

**Testes**:
- Geração de código para literais básicos
- Gerenciamento de escopo e renomeação de variáveis
- Templates de código Erlang válidos
- Otimizações básicas de código

### T5.2: Expressões Básicas
**Descrição**: Implementar geração para expressões fundamentais
**Entregáveis**:
- [ ] Geração de literais (inteiros, strings, átomos, booleanos, nil)
- [ ] Geração de variáveis e atribuições
- [ ] Geração de chamadas de função
- [ ] Geração de chamadas externas (mod.fun)

**Testes**:
- Literais: `42` → `42`, `:ok` → `ok`, `"abc"` → `"abc"`
- Variáveis: `x` → `X` (renomeação)
- Atribuições: `x = 10` → `X = 10`
- Chamadas: `f(2)` → `f(2)`, `math.pow(2, 8)` → `math:pow(2, 8)`

### T5.3: Estruturas de Dados
**Descrição**: Implementar geração para estruturas de dados
**Entregáveis**:
- [ ] Geração de listas e tuplas
- [ ] Geração de records (definição, criação, acesso, atualização)
- [ ] Geração de maps (atom keys, string keys, mistos)
- [ ] Geração de cons (head | tail)

**Testes**:
- Listas: `[1,2,3]` → `[1,2,3]`
- Tuplas: `{a, b}` → `{A, B}`
- Records: `Person{name: "Alice"}` → `#person{name="Alice"}`
- Maps: `%{name: "Alice"}` → `#{name => "Alice"}`
- Cons: `[head | tail]` → `[Head | Tail]`

### T5.4: Operadores e Expressões
**Descrição**: Implementar geração para operadores e expressões complexas
**Entregáveis**:
- [ ] Geração de operadores aritméticos, lógicos, de comparação
- [ ] Geração de operadores de pattern matching
- [ ] Geração de operadores de concatenação e acesso
- [ ] Otimização de expressões

**Testes**:
- Operadores: `a + b` → `A + B`
- Comparações: `x == y` → `X =:= Y`
- Pattern matching: `{a, b} = tuple` → `{A, B} = Tuple`
- Concatenação: `"hello" <> "world"` → `"hello" ++ "world"`

### T5.5: Pattern Matching
**Descrição**: Implementar geração para pattern matching
**Entregáveis**:
- [ ] Geração de case expressions
- [ ] Geração de with expressions
- [ ] Geração de receive expressions
- [ ] Geração de padrões complexos

**Testes**:
- Case: `case x do 1 -> :one _ -> :other end`
- With: `with {:ok, user} <= get_user(id) do ... end`
- Receive: `receive do :msg -> handle() end`
- Padrões: tuplas, listas, records, maps

### T5.6: Estruturas de Controle
**Descrição**: Implementar geração para estruturas de controle
**Entregáveis**:
- [ ] Geração de if/else
- [ ] Geração de for loops e comprehensions
- [ ] Geração de guards
- [ ] Geração de blocos e sequências

**Testes**:
- If/else: `if x > 0 do x else 0 end`
- For: `for x in xs do x*2 end`
- Guards: `when x > 0` → `when X > 0`
- Blocos: `do a = 1; b = 2; a + b end`

### T5.7: Funções
**Descrição**: Implementar geração para funções
**Entregáveis**:
- [ ] Geração de funções simples e multi-cláusula
- [ ] Geração de fun expressions anônimas
- [ ] Geração de funções de alta ordem
- [ ] Geração de closures

**Testes**:
- Funções: `def f(x) do x + 1 end` → `f(X) -> X + 1.`
- Multi-cláusula: `def fact(0) do 1 end; def fact(N) do N * fact(N-1) end`
- Fun expressions: `fn(x) do x * 2 end`
- Closures: captura de variáveis do escopo

### T5.8: Componentes OTP
**Descrição**: Implementar geração para componentes OTP
**Entregáveis**:
- [ ] Geração de workers (init, handle_call, handle_cast)
- [ ] Geração de supervisors (estratégias, children)
- [ ] Geração de .app.src
- [ ] Geração de rebar.config

**Testes**:
- Workers: módulos com funções OTP obrigatórias
- Supervisors: estratégias e children válidos
- .app.src: aplicação configurada corretamente
- rebar.config: dependências e configurações

### T5.9: Binaries e Otimizações
**Descrição**: Implementar geração para binaries e otimizações
**Entregáveis**:
- [ ] Geração de binaries e pattern matching binário
- [ ] Otimizações de código gerado
- [ ] Cache de estruturas frequentes
- [ ] Compilação incremental

**Testes**:
- Binaries: `<<1,2,3>>` → `<<1,2,3>>`
- Pattern matching: `<<a:8, b:16>>` → `<<A:8, B:16>>`
- Otimizações: eliminação de código morto
- Performance: geração rápida para arquivos grandes

### T5.10: Integração e Validação
**Descrição**: Validar geração de código com projetos reais
**Entregáveis**:
- [ ] Testes de round-trip (LX → Erlang → execução)
- [ ] Integração com rebar3
- [ ] Validação com projetos OTP reais
- [ ] Documentação de código gerado

**Testes**:
- Round-trip: código LX válido gera Erlang válido
- Rebar3: projetos compilam sem ajustes manuais
- OTP: componentes funcionam corretamente
- Performance: geração eficiente

## Critérios de Aceitação

- Todo código LX válido deve gerar código Erlang válido e executável
- Cobertura de testes > 95% para geração de código
- Código gerado deve ser legível, idiomático e compatível com OTP
- Integração com rebar3 e Hex deve funcionar sem ajustes manuais
- Documentação de exemplos de código gerado para cada construção LX

## Atualização de Changelog

Ao final de **cada tarefa concluída**, deve ser registrada uma entrada no arquivo `LX_CHANGELOG_V.md`:

**Formato:**
```
## [Data] [Fase] Descrição resumida
- Resumo do que foi implementado/testado/validado
- Principais funcionalidades adicionadas
- Impacto no projeto (performance, funcionalidade, etc.)
- Dependências resolvidas ou criadas
```

**Exemplos:**
```
## [2024-08-05] [Codegen] Estrutura base e gerenciamento de escopo
- Implementada estrutura principal do gerador de código Erlang
- Sistema de gerenciamento de escopo e renomeação de variáveis
- Templates básicos de código Erlang para literais e variáveis
- Base estabelecida para geração de expressões complexas

## [2024-08-07] [Codegen] Expressões básicas e literais
- Geração de código para todos os tipos de literais LX
- Sistema de renomeação de variáveis para evitar conflitos
- Geração de chamadas de função e chamadas externas (mod.fun)
- Otimizações básicas de código gerado

## [2024-08-09] [Codegen] Estruturas de dados (listas, tuplas, records, maps)
- Implementada geração de listas, tuplas e cons
- Geração de records: definição, criação, acesso, atualização
- Geração de maps com atom keys, string keys e tipos mistos
- Suporte a estruturas aninhadas e complexas

## [2024-08-11] [Codegen] Operadores e expressões complexas
- Geração de todos os operadores LX (aritméticos, lógicos, comparação)
- Sistema de precedência e associatividade correta
- Geração de operadores de pattern matching e concatenação
- Otimização de expressões para código mais eficiente

## [2024-08-13] [Codegen] Pattern matching e estruturas de controle
- Geração de case, with, receive expressions
- Implementação de pattern matching em todas as construções
- Geração de if/else, for loops, comprehensions
- Suporte a guards e expressões condicionais

## [2024-08-15] [Codegen] Funções e componentes OTP
- Geração de funções simples, multi-cláusula e anônimas
- Implementação de workers e supervisors com funções obrigatórias
- Geração de .app.src e rebar.config
- Suporte a funções de alta ordem e closures

## [2024-08-17] [Codegen] Binaries e otimizações finais
- Geração de binaries e pattern matching binário
- Otimizações avançadas de código gerado
- Sistema de cache para estruturas frequentes
- Validação completa com projetos OTP reais
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais

**Duração Estimada**: 2-3 semanas
**Dependências**: Fase 4 (Typechecker)
**Riscos**: Médios - geração de código pode ser complexa