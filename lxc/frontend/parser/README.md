# LX Parser - Estrutura Didática

Este diretório contém o parser da linguagem LX, organizado de forma didática e modular para facilitar a compreensão e manutenção.

## Estrutura dos Arquivos

### Entrypoint
- **`main_parser.v`** - **ENTRYPOINT PRINCIPAL** - Wrappers públicos das funções chamadas externamente
- **`main_parser_internal.v`** - Implementação interna do MainParser

### Core Infrastructure
- **`core_parser.v`** - Infraestrutura básica do parser (Parser base, métodos de navegação)
- **`error_recovery.v`** - Recuperação de erros e sincronização
- **`operator_precedence.v`** - Regras de precedência e associatividade de operadores

### Expression Parsing
- **`expr_base.v`** - Parser base de expressões (assignment, logical operators)
- **`expr_comparison_arithmetic.v`** - Expressões de comparação e aritméticas
- **`expr_send_cons.v`** - Expressões de envio de mensagens (!) e cons (|)
- **`expr_primary_postfix.v`** - Expressões primárias e operações postfix (chamadas, acesso)
- **`expr_atoms_literals.v`** - Expressões atômicas e literais
- **`expr_data_structures.v`** - Estruturas de dados (tuplas, listas, maps, records)
- **`expr_control_flow.v`** - Controle de fluxo (if, case, with, for, receive)
- **`expr_patterns.v`** - Parsing de padrões para pattern matching

### Statement Parsing
- **`stmt_base.v`** - Parser base de statements e tipos
- **`stmt_function_definitions.v`** - Definições de funções (def, defp)
- **`stmt_other_definitions.v`** - Outras definições (record, worker, supervisor, spec, test)
- **`stmt_clause_body.v`** - Corpos de cláusulas e expressões restritas
- **`stmt_simple_expressions.v`** - Expressões simples para statements

### Legacy Files (Compatibilidade)
- **`legacy_calls.v`** - Funcionalidades de chamadas para compatibilidade
- **`legacy_data_structures.v`** - Estruturas de dados para compatibilidade
- **`legacy_primary.v`** - Parsing primário para compatibilidade

## Princípios de Organização

1. **Arquivos Pequenos**: Cada arquivo tem no máximo 300 linhas
2. **Contexto Específico**: Cada arquivo trata de um contexto específico
3. **Nomes Descritivos**: Nomes de arquivo indicam claramente o conteúdo
4. **Compatibilidade**: Arquivos legacy mantêm funcionalidade existente
5. **Modularidade**: Cada arquivo pode ser entendido independentemente
6. **Entrypoint Único**: `main_parser.v` é o único ponto de entrada público

## Entrypoint Público

O arquivo `main_parser.v` contém **APENAS** os wrappers das funções públicas:

### Funções Públicas Exportadas:
- `new_main_parser(tokens)` - Cria um novo parser principal
- `new_expression_parser(tokens)` - Cria um novo parser de expressões
- `new_precedence_table()` - Cria uma nova tabela de precedência
- `parse_module()` - Parse de módulos completos
- `parse_expression()` - Parse de expressões
- `parse_pattern()` - Parse de padrões
- `get_errors()` - Retorna erros de compilação
- `has_errors()` - Verifica se há erros

### Implementação Interna:
- Toda a implementação real está em `main_parser_internal.v`
- Os wrappers públicos delegam para funções internas
- Mantém a separação entre interface pública e implementação

## Fluxo de Parsing

1. **MainParser** (wrapper público) recebe chamadas externas
2. **MainParserInternal** executa a implementação real
3. **StatementParser** processa statements de alto nível
4. **ExpressionParser** processa expressões complexas
5. **Parsers especializados** tratam contextos específicos

## Convenções de Nomenclatura

- `expr_*` - Arquivos relacionados a expressões
- `stmt_*` - Arquivos relacionados a statements
- `legacy_*` - Arquivos de compatibilidade
- `*_base` - Arquivos base/fundamentais
- `*_definitions` - Arquivos de definições
- `*_expressions` - Arquivos de expressões específicas
- `*_internal` - Implementação interna

## Manutenção

- **NUNCA** adicione implementação diretamente no `main_parser.v`
- Use `main_parser_internal.v` para implementação do MainParser
- Adicione novos arquivos seguindo as convenções
- Mantenha arquivos pequenos e focados
- Use nomes descritivos para funções e arquivos
- Documente mudanças significativas