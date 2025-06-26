# Fase 1: Fundação - Tarefas e Testes

## Objetivos
Setup do ambiente V, estruturas de dados básicas, sistema de erros e utilitários básicos.

## Tarefas

### T1.1: Setup do Projeto V
**Descrição**: Configurar estrutura de projeto V com módulos básicos
**Entregáveis**:
- [ ] Estrutura de diretórios `lx_compiler/`
- [ ] Arquivo `v.mod` com dependências
- [ ] `main.v` com CLI básico
- [ ] Makefile para build e testes

**Testes**:
```v
// test_foundation.v
module main

import testing

fn test_basic_cli() {
    // Testar CLI básico
    result := exec_command('v run main.v --help')
    assert result.exit_code == 0
    assert result.output.contains('Usage:')
}

fn test_build_system() {
    // Testar sistema de build
    result := exec_command('make build')
    assert result.exit_code == 0
    assert os.exists('lx_compiler')
}
```

### T1.2: Definições AST Básicas
**Descrição**: Implementar estruturas AST fundamentais para LX
**Entregáveis**:
- [ ] `ast/position.v` - Posições e spans
- [ ] `ast/types.v` - Tipos básicos LX
- [ ] `ast/ast.v` - Estruturas AST principais

**Testes**:
```v
// test_ast.v
module main

import testing

fn test_position_creation() {
    pos := Position{
        line: 10
        column: 5
        filename: 'test.lx'
    }
    assert pos.line == 10
    assert pos.column == 5
    assert pos.filename == 'test.lx'
}

fn test_literal_types() {
    // Testar todos os tipos de literais
    string_lit := Literal.LString('hello')
    int_lit := Literal.LInt(42)
    float_lit := Literal.LFloat(3.14)
    bool_lit := Literal.LBool(true)
    atom_lit := Literal.LAtom('ok')
    nil_lit := Literal.LNil

    assert string_lit.str() == 'LString("hello")'
    assert int_lit.str() == 'LInt(42)'
    assert float_lit.str() == 'LFloat(3.14)'
    assert bool_lit.str() == 'LBool(true)'
    assert atom_lit.str() == 'LAtom(ok)'
    assert nil_lit.str() == 'LNil'
}

fn test_basic_expressions() {
    // Testar expressões básicas
    var_expr := Expr.Var('x')
    literal_expr := Expr.Literal(Literal.LInt(10))
    assign_expr := Expr.Assign{
        name: 'x'
        value: Expr.Literal(Literal.LInt(5))
        position: Position{line: 1, column: 1}
    }

    assert var_expr.str() == 'Var(x)'
    assert literal_expr.str() == 'Literal(LInt(10))'
    assert assign_expr.name == 'x'
}

fn test_pattern_matching() {
    // Testar padrões básicos
    wildcard := Pattern.PWildcard
    var_pattern := Pattern.PVar('x')
    atom_pattern := Pattern.PAtom('ok')
    literal_pattern := Pattern.PLiteral(Literal.LInt(42))

    assert wildcard.str() == 'PWildcard'
    assert var_pattern.str() == 'PVar(x)'
    assert atom_pattern.str() == 'PAtom(ok)'
    assert literal_pattern.str() == 'PLiteral(LInt(42))'
}
```

### T1.3: Sistema de Erros
**Descrição**: Implementar sistema robusto de tratamento de erros
**Entregáveis**:
- [ ] `error/error.v` - Tipos de erro
- [ ] `error/formatter.v` - Formatação de erros
- [ ] `error/suggestions.v` - Sugestões de correção

**Testes**:
```v
// test_error_system.v
module main

import testing

fn test_error_kinds() {
    // Testar todos os tipos de erro
    syntax_err := ErrorKind.SyntaxError('Unexpected token')
    type_err := ErrorKind.TypeError{
        message: 'Type mismatch'
        suggestion: 'Use integer instead of string'
    }
    unbound_err := ErrorKind.UnboundVariable{
        variable: 'undefined_var'
        similar_names: ['defined_var', 'other_var']
    }

    assert syntax_err.str().contains('SyntaxError')
    assert type_err.message == 'Type mismatch'
    assert unbound_err.variable == 'undefined_var'
}

fn test_error_positioning() {
    // Testar posicionamento de erros
    pos := Position{line: 5, column: 10, filename: 'test.lx'}
    error := CompilationError{
        kind: ErrorKind.SyntaxError('Test error')
        position: pos
        message: 'Test message'
    }

    assert error.position.line == 5
    assert error.position.column == 10
    assert error.position.filename == 'test.lx'
}

fn test_error_formatting() {
    // Testar formatação de erros
    error := CompilationError{
        kind: ErrorKind.TypeError{message: 'Type error'}
        position: Position{line: 1, column: 1, filename: 'test.lx'}
        message: 'Expected integer, got string'
    }

    formatted := format_error(error)
    assert formatted.contains('test.lx:1:1')
    assert formatted.contains('Type error')
    assert formatted.contains('Expected integer, got string')
}

fn test_error_suggestions() {
    // Testar sugestões de correção
    error := CompilationError{
        kind: ErrorKind.UnboundVariable{
            variable: 'count'
            similar_names: ['counter', 'counts']
        }
        position: Position{line: 1, column: 1}
        message: 'Undefined variable'
    }

    suggestions := generate_suggestions(error)
    assert suggestions.len > 0
    assert suggestions[0].contains('Did you mean')
    assert suggestions[0].contains('counter')
}
```

### T1.4: Utilitários Básicos
**Descrição**: Implementar utilitários de string, arquivo e debug
**Entregáveis**:
- [ ] `utils/string.v` - Utilitários de string
- [ ] `utils/file.v` - Utilitários de arquivo
- [ ] `utils/debug.v` - Utilitários de debug

**Testes**:
```v
// test_utils.v
module main

import testing

fn test_string_utilities() {
    // Testar utilitários de string
    assert escape_string('hello\nworld') == 'hello\\nworld'
    assert unescape_string('hello\\nworld') == 'hello\nworld'
    assert is_valid_identifier('valid_var') == true
    assert is_valid_identifier('123invalid') == false
    assert is_valid_identifier('ValidRecord') == true
    assert is_valid_identifier('invalid-var') == false
}

fn test_file_utilities() {
    // Testar utilitários de arquivo
    content := 'test content'
    filename := 'test_file.lx'

    write_file(filename, content)
    assert os.exists(filename)

    read_content := read_file(filename)
    assert read_content == content

    os.rm(filename)
}

fn test_debug_utilities() {
    // Testar utilitários de debug
    debug_info := DebugInfo{
        level: .info
        message: 'Test debug message'
        context: 'test_context'
    }

    formatted := format_debug(debug_info)
    assert formatted.contains('INFO')
    assert formatted.contains('Test debug message')
    assert formatted.contains('test_context')
}

fn test_string_pooling() {
    // Testar string pooling para otimização
    mut pool := StringPool{}

    str1 := pool.intern('hello')
    str2 := pool.intern('hello')
    str3 := pool.intern('world')

    assert str1 == str2  // Mesma referência
    assert str1 != str3  // Referências diferentes
    assert pool.pool.len == 2  // Apenas 2 strings únicas
}
```

## Métricas de Sucesso

### Funcionalidade
- [ ] Estrutura de projeto compilando sem erros
- [ ] Todas as estruturas AST básicas implementadas
- [ ] Sistema de erros funcional com formatação
- [ ] Utilitários básicos operacionais

### Testes
- [ ] Cobertura de testes > 95% para esta fase
- [ ] Todos os testes passando
- [ ] Testes de edge cases implementados
- [ ] Performance dos utilitários aceitável

### Documentação
- [ ] Documentação das estruturas AST
- [ ] Documentação do sistema de erros
- [ ] Exemplos de uso dos utilitários
- [ ] Guia de contribuição para esta fase

## Critérios de Aceitação

1. **Compilação**: Projeto deve compilar sem warnings
2. **Testes**: Todos os testes devem passar
3. **Performance**: Utilitários devem ser eficientes
4. **Documentação**: Código bem documentado
5. **Estrutura**: Organização modular clara

## Próximos Passos

Após conclusão desta fase, o projeto terá:
- Base sólida para implementação do lexer
- Sistema de erros robusto
- Utilitários essenciais
- Estrutura de testes estabelecida

**Duração Estimada**: 2-3 semanas
**Dependências**: Nenhuma
**Riscos**: Baixos - funcionalidades básicas bem definidas

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
## [2024-06-10] [Foundation] Setup inicial do projeto V
- Configurada estrutura de diretórios e módulos básicos do compilador LX
- Implementado CLI básico com comandos help e build
- Criado sistema de build com Makefile e v.mod
- Base estabelecida para desenvolvimento dos próximos módulos

## [2024-06-12] [Foundation] Estruturas AST e tipos básicos
- Implementadas structs de AST: Position, Literal, Expr, Pattern
- Adicionado suporte a todos os tipos de literais (string, int, float, bool, atom, nil)
- Sistema de expressões básicas: Var, Assign, PatternMatch
- Base estrutural completa para parser e typechecker

## [2024-06-14] [Foundation] Sistema de erros robusto
- Implementado sistema de erros com ErrorKind e CompilationError
- Adicionado formatação de erros com posicionamento preciso
- Sistema de sugestões de correção para erros comuns
- Tratamento de erros léxicos, sintáticos e semânticos

## [2024-06-16] [Foundation] Utilitários básicos e otimizações
- Implementados utilitários de string, arquivo e debug
- Adicionado string pooling para otimização de memória
- Validação de identificadores e escape de strings
- Sistema de debug com níveis e contexto
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais