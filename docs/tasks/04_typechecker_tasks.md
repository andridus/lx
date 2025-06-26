# Fase 4: Typechecker - Tarefas e Testes

## Objetivos
Implementar o sistema de tipos Hindley-Milner, unificação, inferência, checagem de tipos e mensagens de erro para toda a sintaxe LX.

## Tarefas

### T4.1: Estruturas de Tipos Base
**Descrição**: Definir todas as estruturas de tipos necessárias para o sistema
**Entregáveis**:
- [ ] `typechecker/types.v` - Definições de tipos LX
- [ ] `typechecker/context.v` - Contexto de tipos e escopo
- [ ] `typechecker/substitution.v` - Sistema de substituição de tipos
- [ ] `typechecker/unification.v` - Algoritmo de unificação

**Testes**:
- Criação e manipulação de tipos básicos (integer, string, boolean, atom, nil)
- Sistema de variáveis de tipo para polimorfismo
- Contexto de tipos com escopo e shadowing
- Substituição de tipos com composição correta

### T4.2: Algoritmo de Unificação
**Descrição**: Implementar unificação de tipos Hindley-Milner
**Entregáveis**:
- [ ] Algoritmo de unificação para tipos simples e complexos
- [ ] Occurs check para evitar tipos circulares
- [ ] Sistema de composição de substituições
- [ ] Otimizações para performance

**Testes**:
- Unificação de tipos básicos (integer = integer, string = string)
- Unificação de variáveis de tipo (T1 = T2)
- Unificação de tipos funcionais (T1 -> T2 = T3 -> T4)
- Unificação de tipos complexos (listas, tuplas, records)
- Detecção de tipos circulares (T = list(T))

### T4.3: Inferência de Tipos Básicos
**Descrição**: Implementar inferência para literais e variáveis
**Entregáveis**:
- [ ] Inferência automática de tipos para literais
- [ ] Sistema de contexto para variáveis
- [ ] Inferência em atribuições e sequências
- [ ] Tratamento de escopo e shadowing

**Testes**:
- Inferência de tipos para todos os literais (42, "abc", :ok, true, nil)
- Inferência em atribuições simples (x = 10)
- Inferência em sequências de expressões
- Tratamento correto de escopo e shadowing de variáveis

### T4.4: Inferência de Funções
**Descrição**: Implementar inferência para funções e aplicações
**Entregáveis**:
- [ ] Inferência de tipos para funções simples
- [ ] Suporte a funções multi-cláusula
- [ ] Inferência de funções polimórficas
- [ ] Inferência de funções de alta ordem

**Testes**:
- Funções simples: `def f(x) do x + 1 end`
- Funções polimórficas: `def id(x) do x end`
- Funções multi-cláusula com guards
- Funções de alta ordem: `def twice(f, x) do f(f(x)) end`
- Aplicações de função com aridade correta

### T4.5: Estruturas de Dados
**Descrição**: Implementar inferência para estruturas de dados
**Entregáveis**:
- [ ] Inferência de tipos para listas e tuplas
- [ ] Inferência de tipos para records
- [ ] Inferência de tipos para maps
- [ ] Validação de campos e chaves

**Testes**:
- Listas: `[1,2,3]`, `[head | tail]`, listas vazias
- Tuplas: `{a, b}`, `{1, "abc", :ok}`
- Records: definição, criação, acesso, atualização
- Maps: atom keys, string keys, tipos mistos
- Validação de campos inexistentes em records/maps

### T4.6: Pattern Matching
**Descrição**: Implementar checagem de tipos em pattern matching
**Entregáveis**:
- [ ] Validação de tipos em padrões simples
- [ ] Validação de tipos em padrões complexos
- [ ] Checagem de exaustividade de padrões
- [ ] Validação de guards

**Testes**:
- Padrões básicos: wildcard, variáveis, literais
- Padrões complexos: tuplas, listas, records, maps
- Exaustividade em case, with, receive
- Guards com expressões booleanas válidas
- Detecção de padrões sobrepostos

### T4.7: Estruturas de Controle
**Descrição**: Implementar inferência para estruturas de controle
**Entregáveis**:
- [ ] Inferência em if/else, case, with, for
- [ ] Checagem de tipos em guards
- [ ] Inferência em comprehensions
- [ ] Validação de expressões condicionais

**Testes**:
- If/else com condições booleanas
- Case com padrões e tipos compatíveis
- With expressions com pattern matching
- For loops e comprehensions
- Guards em funções e cláusulas

### T4.8: Componentes OTP
**Descrição**: Implementar validação de tipos para componentes OTP
**Entregáveis**:
- [ ] Validação de tipos em workers
- [ ] Validação de tipos em supervisors
- [ ] Checagem de estados OTP
- [ ] Validação de mensagens e respostas

**Testes**:
- Workers: init, handle_call, handle_cast com tipos corretos
- Supervisors: estratégias e children válidos
- Estados de workers e supervisors
- Mensagens e respostas com tipos compatíveis
- Contratos de funções (spec, requires, ensures)

### T4.9: Mensagens de Erro
**Descrição**: Implementar sistema detalhado de mensagens de erro
**Entregáveis**:
- [ ] Mensagens claras para todos os tipos de erro
- [ ] Sugestões de correção automáticas
- [ ] Posicionamento preciso de erros
- [ ] Documentação de regras de tipo

**Testes**:
- Erros de aridade incorreta
- Erros de tipos incompatíveis
- Erros de variáveis não declaradas
- Erros de campos inexistentes
- Sugestões úteis para correção

## Critérios de Aceitação

- Todo código LX válido deve ser aceito e tipado corretamente
- Todo erro de tipo deve ser detectado e reportado com mensagem útil e sugestão
- Cobertura de testes > 95% para casos de tipo
- Performance aceitável para arquivos grandes
- Documentação clara das regras de tipo e exemplos de erros

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
## [2024-07-22] [Typechecker] Estruturas de tipos e sistema base
- Implementadas estruturas de tipos: LxType, TypeVar, Substitution, Context
- Sistema de variáveis de tipo para inferência polimórfica
- Base para algoritmo de unificação Hindley-Milner
- Estrutura preparada para inferência de tipos

## [2024-07-24] [Typechecker] Unificação e occurs check
- Implementado algoritmo de unificação de tipos
- Sistema de occurs check para evitar tipos circulares
- Substituição de tipos com composição correta
- Base para inferência de tipos em expressões complexas

## [2024-07-26] [Typechecker] Inferência de literais e variáveis
- Inferência automática de tipos para todos os literais
- Sistema de contexto para variáveis e escopo
- Inferência de tipos em atribuições e sequências
- Suporte a tipos básicos: integer, string, boolean, atom, nil

## [2024-07-28] [Typechecker] Inferência de funções e aplicações
- Inferência de tipos para funções simples e multi-cláusula
- Suporte a funções polimórficas e de alta ordem
- Inferência de tipos em aplicações de função
- Sistema de aridade e tipos de retorno

## [2024-07-30] [Typechecker] Estruturas de dados e pattern matching
- Inferência de tipos para listas, tuplas, records, maps
- Checagem de tipos em pattern matching
- Validação de campos em records e chaves em maps
- Suporte a tipos aninhados e complexos

## [2024-08-01] [Typechecker] Estruturas de controle e OTP
- Inferência de tipos em if/else, case, with, for
- Checagem de tipos em guards e expressões condicionais
- Validação de tipos em componentes OTP (workers, supervisors)
- Inferência de tipos em comprehensions e fun expressions

## [2024-08-03] [Typechecker] Mensagens de erro e sugestões
- Sistema detalhado de mensagens de erro de tipo
- Sugestões de correção para erros comuns
- Posicionamento preciso de erros de tipo
- Documentação de regras de tipo e exemplos
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais

**Duração Estimada**: 3-4 semanas
**Dependências**: Fase 3 (Parser)
**Riscos**: Altos - sistema Hindley-Milner complexo