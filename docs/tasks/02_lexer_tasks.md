# Fase 2: Lexer - Tarefas e Testes

## Objetivos
Implementação do analisador léxico manual em V, reconhecimento de todos os tokens LX e tratamento de strings e comentários.

## Tarefas

### T2.1: Definições de Tokens
**Descrição**: Definir todos os tipos de tokens da linguagem LX
**Entregáveis**:
- [ ] `lexer/tokens.v` - Enumeração de todos os tokens
- [ ] `lexer/keywords.v` - Palavras-chave reservadas
- [ ] `lexer/operators.v` - Operadores e pontuação

**Testes**:
```v
// test_tokens.v
module main

import testing

fn test_token_enumeration() {
    // Testar todos os tipos de tokens
    assert Token.Ident('x').str() == 'Ident(x)'
    assert Token.UpperIdent('Module').str() == 'UpperIdent(Module)'
    assert Token.String('hello').str() == 'String("hello")'
    assert Token.Int(42).str() == 'Int(42)'
    assert Token.Float(3.14).str() == 'Float(3.14)'
    assert Token.Bool(true).str() == 'Bool(true)'
    assert Token.Atom('ok').str() == 'Atom(ok)'
    assert Token.Nil.str() == 'Nil'
}

fn test_keyword_recognition() {
    // Testar reconhecimento de palavras-chave
    keywords := [
        'def', 'defp', 'case', 'if', 'else', 'do', 'end',
        'with', 'for', 'when', 'receive', 'after', 'true', 'false', 'nil',
        'unsafe', 'record', 'worker', 'supervisor', 'strategy', 'children',
        'one_for_one', 'one_for_all', 'rest_for_one', 'spec', 'requires',
        'ensures', 'matches', 'describe', 'test', 'assert'
    ]

    for keyword in keywords {
        assert is_keyword(keyword) == true
        assert get_keyword_token(keyword) != Token.Error('')
    }
}

fn test_operator_recognition() {
    // Testar reconhecimento de operadores
    operators := [
        '=', '<-', '<=', '->', '!', '::', '.', '++', '|',
        '+', '-', '*', '/', '==', '!=', '<', '>', '<=', '>=',
        'and', 'or', 'not', 'andalso', 'orelse'
    ]

    for op in operators {
        assert is_operator(op) == true
        assert get_operator_token(op) != Token.Error('')
    }
}

fn test_punctuation_tokens() {
    // Testar tokens de pontuação
    assert Token.LParen.str() == 'LParen'
    assert Token.RParen.str() == 'RParen'
    assert Token.LBrace.str() == 'LBrace'
    assert Token.RBrace.str() == 'RBrace'
    assert Token.LBracket.str() == 'LBracket'
    assert Token.RBracket.str() == 'RBracket'
    assert Token.Comma.str() == 'Comma'
    assert Token.Semicolon.str() == 'Semicolon'
    assert Token.Dot.str() == 'Dot'
    assert Token.Colon.str() == 'Colon'
}
```

### T2.2: Implementação do Lexer
**Descrição**: Implementar autômato finito para análise léxica
**Entregáveis**:
- [ ] `lexer/lexer.v` - Implementação principal do lexer
- [ ] `lexer/states.v` - Estados do autômato
- [ ] `lexer/transitions.v` - Transições entre estados

**Testes**:
```v
// test_lexer_core.v
module main

import testing

fn test_lexer_initialization() {
    input := 'x = 42'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    assert lexer.input.len == 5
    assert lexer.position == 0
    assert lexer.line == 1
    assert lexer.column == 1
    assert lexer.filename == 'test.lx'
    assert lexer.state == .initial
}

fn test_basic_token_scanning() {
    input := 'x = 42'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    // Testar sequência de tokens
    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')

    token2 := lexer.next_token()
    assert token2 == Token.Assign

    token3 := lexer.next_token()
    assert token3 == Token.Int(42)

    token4 := lexer.next_token()
    assert token4 == Token.EOF
}

fn test_whitespace_handling() {
    input := '  x  =  42  '
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    // Whitespace deve ser ignorado
    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')

    token2 := lexer.next_token()
    assert token2 == Token.Assign

    token3 := lexer.next_token()
    assert token3 == Token.Int(42)
}

fn test_newline_handling() {
    input := 'x\n= 42'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')
    assert lexer.line == 1

    token2 := lexer.next_token()
    assert token2 == Token.Assign
    assert lexer.line == 2
    assert lexer.column == 1
}
```

### T2.3: Reconhecimento de Literais
**Descrição**: Implementar reconhecimento de todos os tipos de literais
**Entregáveis**:
- [ ] Reconhecimento de strings com escapes
- [ ] Reconhecimento de números inteiros e floats
- [ ] Reconhecimento de booleanos e atoms
- [ ] Reconhecimento de nil

**Testes**:
```v
// test_literals.v
module main

import testing

fn test_string_literals() {
    // Testar strings básicas
    input := '"hello world"'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.String('hello world')

    // Testar strings com escapes
    input2 := '"hello\\nworld\\twith\\"quotes\\""'
    mut lexer2 := Lexer{
        input: input2.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token2 := lexer2.next_token()
    assert token2 == Token.String('hello\nworld\twith"quotes"')
}

fn test_numeric_literals() {
    // Testar inteiros
    input := '42 -123 0'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Int(42)

    token2 := lexer.next_token()
    assert token2 == Token.Int(-123)

    token3 := lexer.next_token()
    assert token3 == Token.Int(0)

    // Testar floats
    input2 := '3.14 -2.5 0.0'
    mut lexer2 := Lexer{
        input: input2.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token4 := lexer2.next_token()
    assert token4 == Token.Float(3.14)

    token5 := lexer2.next_token()
    assert token5 == Token.Float(-2.5)

    token6 := lexer2.next_token()
    assert token6 == Token.Float(0.0)
}

fn test_boolean_literals() {
    input := 'true false'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Bool(true)

    token2 := lexer.next_token()
    assert token2 == Token.Bool(false)
}

fn test_atom_literals() {
    input := ':ok :error :timeout'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Atom('ok')

    token2 := lexer.next_token()
    assert token2 == Token.Atom('error')

    token3 := lexer.next_token()
    assert token3 == Token.Atom('timeout')
}

fn test_nil_literal() {
    input := 'nil'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.Nil
}
```

### T2.4: Reconhecimento de Identificadores
**Descrição**: Implementar reconhecimento de variáveis, módulos e records
**Entregáveis**:
- [ ] Reconhecimento de variáveis (lowercase)
- [ ] Reconhecimento de módulos (lowercase)
- [ ] Reconhecimento de records (uppercase)
- [ ] Reconhecimento de identificadores especiais

**Testes**:
```v
// test_identifiers.v
module main

import testing

fn test_variable_identifiers() {
    input := 'x count _unused _123'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')

    token2 := lexer.next_token()
    assert token2 == Token.Ident('count')

    token3 := lexer.next_token()
    assert token3 == Token.Ident('_unused')

    token4 := lexer.next_token()
    assert token4 == Token.Ident('_123')
}

fn test_module_identifiers() {
    input := 'math string list'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Ident('math')

    token2 := lexer.next_token()
    assert token2 == Token.Ident('string')

    token3 := lexer.next_token()
    assert token3 == Token.Ident('list')
}

fn test_record_identifiers() {
    input := 'Person UserData Config'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.UpperIdent('Person')

    token2 := lexer.next_token()
    assert token2 == Token.UpperIdent('UserData')

    token3 := lexer.next_token()
    assert token3 == Token.UpperIdent('Config')
}

fn test_special_identifiers() {
    input := '__MODULE__'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.Ident('__MODULE__')
}
```

### T2.5: Reconhecimento de Operadores e Pontuação
**Descrição**: Implementar reconhecimento de todos os operadores e símbolos
**Entregáveis**:
- [ ] Operadores de atribuição e pattern matching
- [ ] Operadores aritméticos e de comparação
- [ ] Operadores lógicos
- [ ] Pontuação e delimitadores

**Testes**:
```v
// test_operators.v
module main

import testing

fn test_assignment_operators() {
    input := '= <-'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Assign

    token2 := lexer.next_token()
    assert token2 == Token.PatternMatch
}

fn test_arithmetic_operators() {
    input := '+ - * /'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Plus

    token2 := lexer.next_token()
    assert token2 == Token.Minus

    token3 := lexer.next_token()
    assert token3 == Token.Mult

    token4 := lexer.next_token()
    assert token4 == Token.Div
}

fn test_comparison_operators() {
    input := '== != < > <= >='
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Eq

    token2 := lexer.next_token()
    assert token2 == Token.Neq

    token3 := lexer.next_token()
    assert token3 == Token.Lt

    token4 := lexer.next_token()
    assert token4 == Token.Gt

    token5 := lexer.next_token()
    assert token5 == Token.Leq

    token6 := lexer.next_token()
    assert token6 == Token.Geq
}

fn test_logical_operators() {
    input := 'and or not andalso orelse'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.And

    token2 := lexer.next_token()
    assert token2 == Token.Or

    token3 := lexer.next_token()
    assert token3 == Token.Not

    token4 := lexer.next_token()
    assert token4 == Token.Andalso

    token5 := lexer.next_token()
    assert token5 == Token.Orelse
}

fn test_punctuation() {
    input := '() {} [] , ; . :'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    assert lexer.next_token() == Token.LParen
    assert lexer.next_token() == Token.RParen
    assert lexer.next_token() == Token.LBrace
    assert lexer.next_token() == Token.RBrace
    assert lexer.next_token() == Token.LBracket
    assert lexer.next_token() == Token.RBracket
    assert lexer.next_token() == Token.Comma
    assert lexer.next_token() == Token.Semicolon
    assert lexer.next_token() == Token.Dot
    assert lexer.next_token() == Token.Colon
}
```

### T2.6: Tratamento de Comentários
**Descrição**: Implementar reconhecimento e tratamento de comentários
**Entregáveis**:
- [ ] Reconhecimento de comentários inline (#)
- [ ] Ignorar comentários durante análise
- [ ] Manter posicionamento correto

**Testes**:
```v
// test_comments.v
module main

import testing

fn test_inline_comments() {
    input := 'x = 42 # This is a comment'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')

    token2 := lexer.next_token()
    assert token2 == Token.Assign

    token3 := lexer.next_token()
    assert token3 == Token.Int(42)

    token4 := lexer.next_token()
    assert token4 == Token.EOF
    // Comentário deve ser ignorado
}

fn test_comment_only_line() {
    input := '# This is a comment line\nx = 42'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token1 := lexer.next_token()
    assert token1 == Token.Ident('x')
    assert lexer.line == 2  // Deve estar na linha 2

    token2 := lexer.next_token()
    assert token2 == Token.Assign

    token3 := lexer.next_token()
    assert token3 == Token.Int(42)
}

fn test_comment_with_code() {
    input := 'x = 42 # Set x to 42\ny = 10 # Set y to 10'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    // Primeira linha
    assert lexer.next_token() == Token.Ident('x')
    assert lexer.next_token() == Token.Assign
    assert lexer.next_token() == Token.Int(42)

    // Segunda linha
    assert lexer.next_token() == Token.Ident('y')
    assert lexer.next_token() == Token.Assign
    assert lexer.next_token() == Token.Int(10)

    assert lexer.next_token() == Token.EOF
}
```

### T2.7: Tratamento de Erros Léxicos
**Descrição**: Implementar detecção e reportagem de erros léxicos
**Entregáveis**:
- [ ] Detecção de strings não terminadas
- [ ] Detecção de caracteres inválidos
- [ ] Detecção de números malformados
- [ ] Mensagens de erro úteis

**Testes**:
```v
// test_lexical_errors.v
module main

import testing

fn test_unterminated_string() {
    input := '"hello world'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.Error('Unterminated string')
    assert lexer.errors.len > 0
    assert lexer.errors[0].kind == ErrorKind.UnterminatedString
}

fn test_invalid_character() {
    input := 'x @ y'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    assert lexer.next_token() == Token.Ident('x')

    token := lexer.next_token()
    assert token == Token.Error('Unexpected character')
    assert lexer.errors.len > 0
    assert lexer.errors[0].kind == ErrorKind.UnexpectedCharacter
    assert lexer.errors[0].position.column == 3  // Posição do @
}

fn test_malformed_number() {
    input := '123.456.789'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.Error('Invalid number format')
    assert lexer.errors.len > 0
}

fn test_invalid_escape_sequence() {
    input := '"hello\\xworld"'
    mut lexer := Lexer{
        input: input.bytes()
        position: 0
        line: 1
        column: 1
        filename: 'test.lx'
        state: .initial
    }

    token := lexer.next_token()
    assert token == Token.Error('Invalid escape sequence')
    assert lexer.errors.len > 0
}
```

## Métricas de Sucesso

### Funcionalidade
- [ ] 100% dos tokens LX reconhecidos corretamente
- [ ] Tratamento correto de strings e comentários
- [ ] Detecção robusta de erros léxicos
- [ ] Performance comparável ao OCamlLex

### Testes
- [ ] Cobertura de testes > 98% para esta fase
- [ ] Todos os casos edge testados
- [ ] Testes de performance implementados
- [ ] Testes de recuperação de erros

### Performance
- [ ] Análise léxica em tempo O(n)
- [ ] Uso de memória otimizado
- [ ] String pooling implementado
- [ ] Cache de tokens frequentes

## Critérios de Aceitação

1. **Completude**: Todos os tokens LX devem ser reconhecidos
2. **Correção**: Análise léxica deve ser 100% precisa
3. **Performance**: Deve ser pelo menos tão rápido quanto OCamlLex
4. **Erros**: Mensagens de erro claras e úteis
5. **Robustez**: Deve lidar com entrada malformada graciosamente

## Próximos Passos

Após conclusão desta fase, o projeto terá:
- Lexer completo e robusto
- Base sólida para implementação do parser
- Sistema de testes léxicos estabelecido
- Performance otimizada

**Duração Estimada**: 1-2 semanas
**Dependências**: Fase 1 (Fundação)
**Riscos**: Médios - implementação manual pode ser complexa

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
## [2024-06-18] [Lexer] Definições de tokens e keywords
- Implementado sistema completo de tokens para linguagem LX
- Adicionado reconhecimento de todas as keywords (def, case, if, worker, etc.)
- Suporte a operadores aritméticos, lógicos, de comparação e atribuição
- Base léxica estabelecida para implementação do parser

## [2024-06-20] [Lexer] Implementação do autômato léxico
- Criado autômato finito para análise léxica manual em V
- Implementados estados para identificadores, números, strings, comentários
- Sistema de transições entre estados com tratamento de edge cases
- Performance otimizada comparável ao OCamlLex

## [2024-06-22] [Lexer] Reconhecimento de literais e identificadores
- Suporte completo a literais: strings com escapes, números, booleanos, átomos, nil
- Reconhecimento de variáveis (snake_case), módulos (lowercase), records (Uppercase)
- Tratamento de identificadores especiais como __MODULE__
- Validação de nomes de identificadores conforme especificação LX

## [2024-06-24] [Lexer] Operadores e pontuação
- Implementado reconhecimento de todos os operadores LX (=, <-, <=, ->, !, ::, etc.)
- Suporte a operadores aritméticos, lógicos, de comparação e pattern matching
- Reconhecimento de pontuação: parênteses, chaves, colchetes, vírgulas, etc.
- Precedência e associatividade de operadores estabelecida

## [2024-06-26] [Lexer] Tratamento de comentários e whitespace
- Implementado reconhecimento de comentários inline (#)
- Tratamento correto de whitespace, newlines e tabs
- Manutenção de posicionamento preciso durante análise
- Comentários ignorados sem afetar estrutura léxica

## [2024-06-28] [Lexer] Detecção e recuperação de erros
- Sistema robusto de detecção de erros léxicos (strings não terminadas, caracteres inválidos)
- Mensagens de erro detalhadas com posicionamento preciso
- Recuperação de erros para continuar análise quando possível
- Sugestões de correção para erros comuns
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais