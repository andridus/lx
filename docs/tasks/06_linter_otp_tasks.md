# Fase 6: Linter & OTP - Tarefas e Testes

## Objetivos
Implementar análise estática, regras de linting e validação de componentes OTP.

## Tarefas

### T6.1: Estrutura Base do Linter
**Descrição**: Implementar estrutura base para análise estática
**Entregáveis**:
- [ ] `linter/linter.v` - Estrutura principal do linter
- [ ] `linter/rules.v` - Sistema de regras configuráveis
- [ ] `linter/context.v` - Contexto de análise
- [ ] `linter/reporter.v` - Sistema de reportagem de problemas

**Testes**:
- Estrutura base do linter funcionando
- Sistema de regras configurável
- Contexto de análise com escopo
- Reportagem de problemas com posicionamento

### T6.2: Regras de Estilo e Convenções
**Descrição**: Implementar regras de estilo e convenções de nomenclatura
**Entregáveis**:
- [ ] Validação de nomes de variáveis (snake_case)
- [ ] Validação de nomes de funções e módulos
- [ ] Validação de nomes de records (Uppercase)
- [ ] Validação de nomes de workers e supervisors

**Testes**:
- Variáveis: `valid_var` ✓, `InvalidVar` ✗
- Funções: `valid_function` ✓, `InvalidFunction` ✗
- Records: `ValidRecord` ✓, `invalid_record` ✗
- Workers: `valid_worker` ✓, `InvalidWorker` ✗

### T6.3: Análise de Código e Uso
**Descrição**: Implementar análise de uso de código e variáveis
**Entregáveis**:
- [ ] Detecção de variáveis não utilizadas
- [ ] Detecção de shadowing de variáveis
- [ ] Detecção de funções não utilizadas
- [ ] Análise de aridade de funções

**Testes**:
- Variáveis não usadas: `x = 10; y = 20` (x não usado)
- Shadowing: `x = 1; fn() do x = 2 end` (shadowing detectado)
- Funções não usadas: `def unused() do ... end`
- Aridade incorreta: `f(1,2)` para `def f(x) do ... end`

### T6.4: Análise de Pattern Matching
**Descrição**: Implementar análise de pattern matching
**Entregáveis**:
- [ ] Detecção de padrões não exaustivos
- [ ] Detecção de padrões sobrepostos
- [ ] Validação de uso de unsafe
- [ ] Sugestões de simplificação de padrões

**Testes**:
- Padrões não exaustivos: `case x do 1 -> :one end`
- Padrões sobrepostos: `case x do 1 -> :one; 1 -> :one_again end`
- Uso de unsafe: `unsafe %{field: value} = map`
- Padrões redundantes: `{a, _} = {1, 2}` (sugerir `{a, _}`)

### T6.5: Validação de OTP
**Descrição**: Implementar validação de componentes OTP
**Entregáveis**:
- [ ] Validação de workers (funções obrigatórias)
- [ ] Validação de supervisors (estratégias, children)
- [ ] Validação de estados OTP
- [ ] Validação de mensagens e respostas

**Testes**:
- Workers sem init: `worker my_worker do ... end`
- Workers sem handle_call: `worker w do def init(_) do ... end end`
- Supervisors sem estratégia: `supervisor s do ... end`
- Children inválidos: `children [invalid_worker]`

### T6.6: Análise de Contratos
**Descrição**: Implementar validação de contratos de funções
**Entregáveis**:
- [ ] Validação de spec (tipos de entrada/saída)
- [ ] Validação de requires (pré-condições)
- [ ] Validação de ensures (pós-condições)
- [ ] Validação de matches (padrões de entrada)

**Testes**:
- Spec inválido: `spec f(integer) -> string` mas retorna integer
- Requires violado: `requires x > 0` mas chamada com x = 0
- Ensures violado: `ensures result > 0` mas retorna 0
- Matches inválido: `matches {:ok, _}` mas recebe {:error, _}

### T6.7: Sugestões de Correção
**Descrição**: Implementar sistema de sugestões automáticas
**Entregáveis**:
- [ ] Sugestões para variáveis não utilizadas
- [ ] Sugestões para nomes incorretos
- [ ] Sugestões para pattern matching
- [ ] Sugestões para OTP

**Testes**:
- Variável não usada: sugerir prefixar com `_`
- Nome incorreto: sugerir nome correto baseado em convenção
- Pattern não exaustivo: sugerir padrão catch-all
- Worker sem função: sugerir implementar função obrigatória

### T6.8: Integração e Performance
**Descrição**: Integrar linter ao fluxo de build e otimizar performance
**Entregáveis**:
- [ ] Integração com CLI
- [ ] Integração com build system
- [ ] Otimizações de performance
- [ ] Configuração de regras

**Testes**:
- CLI: `lx lint file.lx`
- Build: linter executado automaticamente
- Performance: análise rápida em arquivos grandes
- Configuração: regras habilitadas/desabilitadas

## Critérios de Aceitação

- Todos os problemas de linting e OTP devem ser detectados
- Sugestões de correção devem ser úteis e acionáveis
- Cobertura de testes > 90% para regras de linting e OTP
- Performance aceitável para projetos grandes
- Documentação clara das regras e exemplos de código corrigido

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
## [2024-08-20] [Linter] Estrutura base e sistema de regras
- Implementada estrutura principal do linter com sistema de regras configurável
- Sistema de contexto de análise com escopo e posicionamento
- Reportagem de problemas com detalhes e sugestões
- Base estabelecida para implementação de regras específicas

## [2024-08-22] [Linter] Regras de estilo e convenções de nomenclatura
- Validação de nomes de variáveis, funções, módulos e records
- Detecção de violações de convenções (snake_case, Uppercase, etc.)
- Sugestões automáticas para nomes incorretos
- Cobertura completa de convenções LX

## [2024-08-24] [Linter] Análise de uso de código e variáveis
- Detecção de variáveis e funções não utilizadas
- Análise de shadowing de variáveis em diferentes escopos
- Validação de aridade de funções em chamadas
- Sugestões para prefixar variáveis não usadas com _

## [2024-08-26] [Linter] Análise de pattern matching
- Detecção de padrões não exaustivos em case, with, receive
- Identificação de padrões sobrepostos ou inatingíveis
- Validação de uso de unsafe com sugestões de alternativas
- Sugestões de simplificação de padrões redundantes

## [2024-08-28] [Linter] Validação de componentes OTP
- Validação de workers: funções obrigatórias (init, handle_call, handle_cast)
- Validação de supervisors: estratégias e children válidos
- Checagem de estados OTP e mensagens compatíveis
- Detecção de nomes duplicados de workers/supervisors

## [2024-08-30] [Linter] Análise de contratos e especificações
- Validação de spec (tipos de entrada/saída) em funções
- Checagem de requires (pré-condições) e ensures (pós-condições)
- Validação de matches (padrões de entrada) em funções
- Detecção de violações de contratos em tempo de compilação

## [2024-09-01] [Linter] Sistema de sugestões e integração
- Sugestões automáticas para todos os problemas detectados
- Integração completa com CLI e sistema de build
- Otimizações de performance para projetos grandes
- Configuração flexível de regras e severidade
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais

**Duração Estimada**: 2-3 semanas
**Dependências**: Fase 5 (Codegen)
**Riscos**: Baixos - análise estática bem definida