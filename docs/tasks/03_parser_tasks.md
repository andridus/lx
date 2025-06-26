# Fase 3: Parser - Tarefas e Testes

## Objetivos
Implementação do parser recursivo descendente em V, precedência de operadores e recuperação de erros para todas as construções sintáticas LX.

## Tarefas

### T3.1: Parser Base e Estruturas
**Descrição**: Implementar estrutura base do parser e gerenciamento de tokens
**Entregáveis**:
- [ ] `parser/parser.v` - Estrutura principal do parser
- [ ] `parser/precedence.v` - Tabela de precedência de operadores
- [ ] `parser/grammar.v` - Regras gramaticais básicas

**Testes**:
```v
// test_parser_base.v
module main

import testing

fn test_parser_initialization() {
    tokens := [
        Token.Ident('x'),
        Token.Assign,
        Token.Int(42),
        Token.EOF
    ]

    mut parser := Parser{
        tokens: tokens
        position: 0
        current: tokens[0]
        errors: []
    }

    assert parser.tokens.len == 4
    assert parser.position == 0
    assert parser.current == Token.Ident('x')
    assert parser.errors.len == 0
}

fn test_token_advancement() {
    tokens := [
        Token.Ident('x'),
        Token.Assign,
        Token.Int(42),
        Token.EOF
    ]

    mut parser := Parser{
        tokens: tokens
        position: 0
        current: tokens[0]
        errors: []
    }

    assert parser.current == Token.Ident('x')
    parser.advance()
    assert parser.position == 1
    assert parser.current == Token.Assign
    parser.advance()
    assert parser.current == Token.Int(42)
}

fn test_precedence_table() {
    table := PrecedenceTable{}

    // Testar precedência de operadores
    assert table.get_precedence(Token.Plus) == 1
    assert table.get_precedence(Token.Mult) == 2
    assert table.get_precedence(Token.Assign) == 0
    assert table.get_precedence(Token.Eq) == 3
    assert table.get_precedence(Token.And) == 4
}

fn test_parser_error_handling() {
    tokens := [
        Token.Ident('x'),
        Token.Error('Unexpected token'),
        Token.EOF
    ]

    mut parser := Parser{
        tokens: tokens
        position: 0
        current: tokens[0]
        errors: []
    }

    result := parser.parse_expression()
    assert result.is_error()
    assert parser.errors.len > 0
}
```

### T3.2: Expressões Básicas
**Descrição**: Implementar parsing de expressões fundamentais
**Entregáveis**:
- [ ] Parsing de literais
- [ ] Parsing de variáveis
- [ ] Parsing de atribuições
- [ ] Parsing de chamadas de função

**Testes**:
```v
// test_basic_expressions.v
module main

import testing

fn test_literal_parsing() {
    // Testar parsing de todos os tipos de literais
    test_cases := [
        ('42', Expr.Literal(Literal.LInt(42))),
        ('3.14', Expr.Literal(Literal.LFloat(3.14))),
        ('"hello"', Expr.Literal(Literal.LString('hello'))),
        ('true', Expr.Literal(Literal.LBool(true))),
        ('false', Expr.Literal(Literal.LBool(false))),
        (':ok', Expr.Literal(Literal.LAtom('ok'))),
        ('nil', Expr.Literal(Literal.LNil))
    ]

    for test_case in test_cases {
        input, expected := test_case
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_expression()
        assert result.is_ok()
        assert result.unwrap() == expected
    }
}

fn test_variable_parsing() {
    input := 'x'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    assert result.unwrap() == Expr.Var('x')
}

fn test_assignment_parsing() {
    input := 'x = 42'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.Assign{
        name: 'x'
        value: Expr.Literal(Literal.LInt(42))
        position: Position{line: 1, column: 1}
    }
}

fn test_function_call_parsing() {
    input := 'sum(3, 4)'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.App{
        func: Expr.Var('sum')
        args: [Expr.Literal(Literal.LInt(3)), Expr.Literal(Literal.LInt(4))]
    }
}

fn test_external_call_parsing() {
    input := 'math.pow(2, 8)'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.ExternalCall{
        module: 'math'
        function: 'pow'
        args: [Expr.Literal(Literal.LInt(2)), Expr.Literal(Literal.LInt(8))]
        position: Position{line: 1, column: 1}
    }
}
```

### T3.3: Estruturas de Dados
**Descrição**: Implementar parsing de tuplas, listas e cons
**Entregáveis**:
- [ ] Parsing de tuplas
- [ ] Parsing de listas
- [ ] Parsing de cons (head | tail)
- [ ] Parsing de estruturas aninhadas

**Testes**:
```v
// test_data_structures.v
module main

import testing

fn test_tuple_parsing() {
    input := '{1, 2, 3}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.Tuple([
        Expr.Literal(Literal.LInt(1)),
        Expr.Literal(Literal.LInt(2)),
        Expr.Literal(Literal.LInt(3))
    ])
}

fn test_empty_tuple_parsing() {
    input := '{}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.Tuple([])
}

fn test_list_parsing() {
    input := '[1, 2, 3]'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.List([
        Expr.Literal(Literal.LInt(1)),
        Expr.Literal(Literal.LInt(2)),
        Expr.Literal(Literal.LInt(3))
    ])
}

fn test_empty_list_parsing() {
    input := '[]'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.List([])
}

fn test_cons_parsing() {
    input := '[head | tail]'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.Cons{
        head: Expr.Var('head')
        tail: Expr.Var('tail')
    }
}

fn test_nested_structures() {
    input := '{1, [2, 3], {4, 5}}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()

    // Verificar estrutura aninhada
    tuple := expr as Expr.Tuple
    assert tuple.len == 3
    assert tuple[0] == Expr.Literal(Literal.LInt(1))
    assert tuple[1] == Expr.List([Expr.Literal(Literal.LInt(2)), Expr.Literal(Literal.LInt(3))])
    assert tuple[2] == Expr.Tuple([Expr.Literal(Literal.LInt(4)), Expr.Literal(Literal.LInt(5))])
}
```

### T3.4: Operadores e Precedência
**Descrição**: Implementar parsing de expressões com operadores e precedência
**Entregáveis**:
- [ ] Parsing de operadores aritméticos
- [ ] Parsing de operadores de comparação
- [ ] Parsing de operadores lógicos
- [ ] Respeito à precedência de operadores

**Testes**:
```v
// test_operators.v
module main

import testing

fn test_arithmetic_operators() {
    test_cases := [
        ('1 + 2', 'BinOp(+, Literal(LInt(1)), Literal(LInt(2)))'),
        ('3 - 4', 'BinOp(-, Literal(LInt(3)), Literal(LInt(4)))'),
        ('5 * 6', 'BinOp(*, Literal(LInt(5)), Literal(LInt(6)))'),
        ('7 / 8', 'BinOp(/, Literal(LInt(7)), Literal(LInt(8)))')
    ]

    for test_case in test_cases {
        input, expected := test_case
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_expression()
        assert result.is_ok()
        assert result.unwrap().str() == expected
    }
}

fn test_comparison_operators() {
    test_cases := [
        ('1 == 2', 'BinOp(==, Literal(LInt(1)), Literal(LInt(2)))'),
        ('3 != 4', 'BinOp(!=, Literal(LInt(3)), Literal(LInt(4)))'),
        ('5 < 6', 'BinOp(<, Literal(LInt(5)), Literal(LInt(6)))'),
        ('7 > 8', 'BinOp(>, Literal(LInt(7)), Literal(LInt(8)))'),
        ('9 <= 10', 'BinOp(<=, Literal(LInt(9)), Literal(LInt(10)))'),
        ('11 >= 12', 'BinOp(>=, Literal(LInt(11)), Literal(LInt(12)))')
    ]

    for test_case in test_cases {
        input, expected := test_case
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_expression()
        assert result.is_ok()
        assert result.unwrap().str() == expected
    }
}

fn test_logical_operators() {
    test_cases := [
        ('true and false', 'BinOp(and, Literal(LBool(true)), Literal(LBool(false)))'),
        ('true or false', 'BinOp(or, Literal(LBool(true)), Literal(LBool(false)))'),
        ('not true', 'UnaryOp(not, Literal(LBool(true)))'),
        ('true andalso false', 'BinOp(andalso, Literal(LBool(true)), Literal(LBool(false)))'),
        ('true orelse false', 'BinOp(orelse, Literal(LBool(true)), Literal(LBool(false)))')
    ]

    for test_case in test_cases {
        input, expected := test_case
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_expression()
        assert result.is_ok()
        assert result.unwrap().str() == expected
    }
}

fn test_operator_precedence() {
    // Testar precedência: * > + > ==
    input := '1 + 2 * 3 == 7'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()

    // Deve ser interpretado como: (1 + (2 * 3)) == 7
    bin_op := expr as Expr.BinOp
    assert bin_op.op == '=='
    assert bin_op.right == Expr.Literal(Literal.LInt(7))

    left_bin_op := bin_op.left as Expr.BinOp
    assert left_bin_op.op == '+'
    assert left_bin_op.left == Expr.Literal(Literal.LInt(1))

    mult_bin_op := left_bin_op.right as Expr.BinOp
    assert mult_bin_op.op == '*'
    assert mult_bin_op.left == Expr.Literal(Literal.LInt(2))
    assert mult_bin_op.right == Expr.Literal(Literal.LInt(3))
}

fn test_associativity() {
    // Testar associatividade à esquerda
    input := '1 - 2 - 3'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()

    // Deve ser interpretado como: (1 - 2) - 3
    bin_op := expr as Expr.BinOp
    assert bin_op.op == '-'
    assert bin_op.right == Expr.Literal(Literal.LInt(3))

    left_bin_op := bin_op.left as Expr.BinOp
    assert left_bin_op.op == '-'
    assert left_bin_op.left == Expr.Literal(Literal.LInt(1))
    assert left_bin_op.right == Expr.Literal(Literal.LInt(2))
}
```

### T3.5: Pattern Matching
**Descrição**: Implementar parsing de pattern matching
**Entregáveis**:
- [ ] Parsing de padrões básicos
- [ ] Parsing de padrões de tuplas e listas
- [ ] Parsing de padrões de records
- [ ] Parsing de padrões de maps

**Testes**:
```v
// test_pattern_matching.v
module main

import testing

fn test_basic_patterns() {
    test_cases := [
        ('_', Pattern.PWildcard),
        ('x', Pattern.PVar('x')),
        (':ok', Pattern.PAtom('ok')),
        ('42', Pattern.PLiteral(Literal.LInt(42))),
        ('"hello"', Pattern.PLiteral(Literal.LString('hello')))
    ]

    for test_case in test_cases {
        input, expected := test_case
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_pattern()
        assert result.is_ok()
        assert result.unwrap() == expected
    }
}

fn test_tuple_patterns() {
    input := '{x, y, z}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_pattern()
    assert result.is_ok()
    pattern := result.unwrap()
    assert pattern == Pattern.PTuple([
        Pattern.PVar('x'),
        Pattern.PVar('y'),
        Pattern.PVar('z')
    ])
}

fn test_list_patterns() {
    input := '[head | tail]'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_pattern()
    assert result.is_ok()
    pattern := result.unwrap()
    assert pattern == Pattern.PCons{
        head: Pattern.PVar('head')
        tail: Pattern.PVar('tail')
    }
}

fn test_record_patterns() {
    input := 'Person{name: user_name, age: user_age}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_pattern()
    assert result.is_ok()
    pattern := result.unwrap()
    assert pattern == Pattern.PRecord{
        name: 'Person'
        fields: [
            PatternField{name: 'name', pattern: Pattern.PVar('user_name')},
            PatternField{name: 'age', pattern: Pattern.PVar('user_age')}
        ]
    }
}

fn test_map_patterns() {
    input := '%{name: user_name, age: user_age}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_pattern()
    assert result.is_ok()
    pattern := result.unwrap()
    assert pattern == Pattern.PMap([
        MapPatternField{key: Pattern.PAtom('name'), value: Pattern.PVar('user_name')},
        MapPatternField{key: Pattern.PAtom('age'), value: Pattern.PVar('user_age')}
    ])
}

fn test_pattern_matching_expressions() {
    input := '{x, y} = tuple'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.PatternMatch{
        pattern: Pattern.PTuple([Pattern.PVar('x'), Pattern.PVar('y')])
        value: Expr.Var('tuple')
        position: Position{line: 1, column: 1}
        unsafe: false
    }
}

fn test_unsafe_pattern_matching() {
    input := 'unsafe %{dynamic_field: value} = runtime_map'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.PatternMatch{
        pattern: Pattern.PMap([MapPatternField{
            key: Pattern.PVar('dynamic_field')
            value: Pattern.PVar('value')
        }])
        value: Expr.Var('runtime_map')
        position: Position{line: 1, column: 1}
        unsafe: true
    }
}
```

### T3.6: Estruturas de Controle
**Descrição**: Implementar parsing de estruturas de controle
**Entregáveis**:
- [ ] Parsing de `if`/`else`
- [ ] Parsing de `case`
- [ ] Parsing de `with`
- [ ] Parsing de `for`

**Testes**:
```v
// test_control_structures.v
module main

import testing

fn test_if_else_parsing() {
    input := 'if x > 0 do x else 0 end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.If{
        condition: Expr.BinOp{
            left: Expr.Var('x')
            op: '>'
            right: Expr.Literal(Literal.LInt(0))
        }
        then_branch: Expr.Var('x')
        else_branch: Expr.Literal(Literal.LInt(0))
    }
}

fn test_case_parsing() {
    input := 'case msg do :ok -> handle_ok() _ -> default() end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.Match{
        expr: Expr.Var('msg')
        clauses: [
            MatchClause{
                pattern: Pattern.PAtom('ok')
                guard: none
                body: Expr.App{func: Expr.Var('handle_ok'), args: []}
            },
            MatchClause{
                pattern: Pattern.PWildcard
                guard: none
                body: Expr.App{func: Expr.Var('default'), args: []}
            }
        ]
    }
}

fn test_with_parsing() {
    input := 'with {:ok, user} <= get_user(id) do user.name else "Unknown" end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.With{
        steps: [WithStep{
            pattern: Pattern.PTuple([Pattern.PAtom('ok'), Pattern.PVar('user')])
            value: Expr.App{func: Expr.Var('get_user'), args: [Expr.Var('id')]}
        }]
        success_body: Expr.ExternalCall{
            module: 'user'
            function: 'name'
            args: []
            position: Position{line: 1, column: 1}
        }
        else_branch: Expr.Literal(Literal.LString('Unknown'))
    }
}

fn test_for_parsing() {
    input := 'for x in list when x > 0 do x * 2 end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.For{
        pattern: Pattern.PVar('x')
        var: none
        iterable: Expr.Var('list')
        body: Expr.BinOp{
            left: Expr.Var('x')
            op: '*'
            right: Expr.Literal(Literal.LInt(2))
        }
        guard: GuardExpr.GuardBinOp{
            left: GuardValue.GVar('x')
            op: '>'
            right: GuardValue.GLiteral(Literal.LInt(0))
        }
    }
}
```

### T3.7: Funções e Cláusulas
**Descrição**: Implementar parsing de definições de função
**Entregáveis**:
- [ ] Parsing de funções simples
- [ ] Parsing de funções multi-cláusula
- [ ] Parsing de fun expressions
- [ ] Parsing de guards

**Testes**:
```v
// test_functions.v
module main

import testing

fn test_simple_function_parsing() {
    input := 'def greet(name) do "Hello, " ++ name end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_function_definition()
    assert result.is_ok()
    func_def := result.unwrap()
    assert func_def.name == 'greet'
    assert func_def.params == ['name']
    assert func_def.body == Expr.BinOp{
        left: Expr.Literal(Literal.LString('Hello, '))
        op: '++'
        right: Expr.Var('name')
    }
}

fn test_multi_clause_function_parsing() {
    input := 'def factorial do (0) do 1 end (N) when N > 0 do N * factorial(N - 1) end end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_function_definition()
    assert result.is_ok()
    func_def := result.unwrap()
    assert func_def.name == 'factorial'
    assert func_def.clauses.len == 2

    // Primeira cláusula
    clause1 := func_def.clauses[0]
    assert clause1.pattern == Pattern.PLiteral(Literal.LInt(0))
    assert clause1.guard == none
    assert clause1.body == Expr.Literal(Literal.LInt(1))

    // Segunda cláusula
    clause2 := func_def.clauses[1]
    assert clause2.pattern == Pattern.PVar('N')
    assert clause2.guard == GuardExpr.GuardBinOp{
        left: GuardValue.GVar('N')
        op: '>'
        right: GuardValue.GLiteral(Literal.LInt(0))
    }
}

fn test_fun_expression_parsing() {
    input := 'fn(x, y) do x + y end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.FunExpression{
        params: ['x', 'y']
        body: Expr.BinOp{
            left: Expr.Var('x')
            op: '+'
            right: Expr.Var('y')
        }
    }
}

fn test_multi_clause_fun_expression() {
    input := 'fn do (:ok) do "Success" end (:error) do "Failed" end end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.FunExpressionClauses([
        FunClause{
            pattern: Pattern.PAtom('ok')
            guard: none
            body: Expr.Literal(Literal.LString('Success'))
        },
        FunClause{
            pattern: Pattern.PAtom('error')
            guard: none
            body: Expr.Literal(Literal.LString('Failed'))
        }
    ])
}
```

### T3.8: Records e Maps
**Descrição**: Implementar parsing de records e maps
**Entregáveis**:
- [ ] Parsing de definições de record
- [ ] Parsing de criação de record
- [ ] Parsing de acesso a campos
- [ ] Parsing de maps

**Testes**:
```v
// test_records_maps.v
module main

import testing

fn test_record_definition_parsing() {
    input := 'record Person { name :: string, age :: integer }'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_record_definition()
    assert result.is_ok()
    record_def := result.unwrap()
    assert record_def.name == 'Person'
    assert record_def.fields.len == 2
    assert record_def.fields[0].name == 'name'
    assert record_def.fields[0].type == LxType.TString
    assert record_def.fields[1].name == 'age'
    assert record_def.fields[1].type == LxType.TInteger
}

fn test_record_creation_parsing() {
    input := 'Person{name: "Alice", age: 30}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.RecordCreate{
        name: 'Person'
        fields: [
            RecordField{name: 'name', value: Expr.Literal(Literal.LString('Alice'))},
            RecordField{name: 'age', value: Expr.Literal(Literal.LInt(30))}
        ]
    }
}

fn test_record_access_parsing() {
    input := 'person.name'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.RecordAccess{
        expr: Expr.Var('person')
        field: 'name'
    }
}

fn test_record_update_parsing() {
    input := '{person | age: 31}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.RecordUpdate{
        expr: Expr.Var('person')
        updates: [RecordField{name: 'age', value: Expr.Literal(Literal.LInt(31))}]
    }
}

fn test_map_creation_parsing() {
    input := '%{name: "Alice", age: 30}'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.MapCreate([
        MapField{key: Expr.Literal(Literal.LAtom('name')), value: Expr.Literal(Literal.LString('Alice'))},
        MapField{key: Expr.Literal(Literal.LAtom('age')), value: Expr.Literal(Literal.LInt(30))}
    ])
}

fn test_map_access_parsing() {
    input := 'map[:name]'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_ok()
    expr := result.unwrap()
    assert expr == Expr.MapAccess{
        map: Expr.Var('map')
        key: Expr.Literal(Literal.LAtom('name'))
    }
}
```

### T3.9: OTP Components
**Descrição**: Implementar parsing de componentes OTP
**Entregáveis**:
- [ ] Parsing de workers
- [ ] Parsing de supervisors
- [ ] Parsing de estratégias
- [ ] Parsing de children

**Testes**:
```v
// test_otp_components.v
module main

import testing

fn test_worker_parsing() {
    input := 'worker my_worker do def init(_) do {:ok, []} end end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_otp_component()
    assert result.is_ok()
    component := result.unwrap()
    assert component == OtpComponent.Worker{
        name: 'my_worker'
        functions: [FunctionDef{
            name: 'init'
            params: ['_']
            body: Expr.Tuple([Expr.Literal(Literal.LAtom('ok')), Expr.List([])])
        }]
        specs: []
        position: Position{line: 1, column: 1}
    }
}

fn test_supervisor_parsing() {
    input := 'supervisor top_sup { strategy one_for_one children [my_worker] }'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_otp_component()
    assert result.is_ok()
    component := result.unwrap()
    assert component == OtpComponent.Supervisor{
        name: 'top_sup'
        strategy: OtpStrategy.OneForOne
        children: ChildrenSpec.Simple(['my_worker'])
        position: Position{line: 1, column: 1}
    }
}

fn test_strategy_parsing() {
    strategies := ['one_for_one', 'one_for_all', 'rest_for_one']

    for strategy in strategies {
        input := 'strategy ${strategy}'
        tokens := lex(input)
        mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

        result := parser.parse_strategy()
        assert result.is_ok()
    }
}

fn test_children_parsing() {
    input := 'children { worker [a] supervisor [b] }'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_children()
    assert result.is_ok()
    children := result.unwrap()
    assert children == ChildrenSpec.Typed{
        workers: ['a']
        supervisors: ['b']
    }
}
```

### T3.10: Recuperação de Erros
**Descrição**: Implementar recuperação robusta de erros de parsing
**Entregáveis**:
- [ ] Detecção de tokens inesperados
- [ ] Recuperação de estruturas malformadas
- [ ] Mensagens de erro úteis
- [ ] Continuação do parsing após erros

**Testes**:
```v
// test_error_recovery.v
module main

import testing

fn test_unexpected_token_recovery() {
    input := 'x = 42 + @ y'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_error()
    assert parser.errors.len > 0
    assert parser.errors[0].kind == ErrorKind.UnexpectedToken{
        found: '@'
        expected: 'expression'
    }
}

fn test_missing_closing_brace_recovery() {
    input := 'x = {1, 2'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_expression()
    assert result.is_error()
    assert parser.errors.len > 0
    assert parser.errors[0].kind == ErrorKind.SyntaxError('Missing closing brace')
}

fn test_malformed_function_recovery() {
    input := 'def func( do x end'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    result := parser.parse_function_definition()
    assert result.is_error()
    assert parser.errors.len > 0
    assert parser.errors[0].kind == ErrorKind.SyntaxError('Missing closing parenthesis')
}

fn test_continue_after_error() {
    input := 'x = 42 + @ y = 10'
    tokens := lex(input)
    mut parser := Parser{tokens: tokens, position: 0, current: tokens[0], errors: []}

    // Deve continuar parsing após o erro
    result := parser.parse_expression()
    assert result.is_error()
    assert parser.errors.len > 0

    // Deve conseguir parsear a próxima expressão
    result2 := parser.parse_expression()
    assert result2.is_ok()
    assert result2.unwrap() == Expr.Assign{
        name: 'y'
        value: Expr.Literal(Literal.LInt(10))
        position: Position{line: 1, column: 1}
    }
}
```

## Métricas de Sucesso

### Funcionalidade
- [ ] 100% das construções sintáticas LX parseadas corretamente
- [ ] Precedência de operadores respeitada
- [ ] Recuperação robusta de erros
- [ ] Performance aceitável

### Testes
- [ ] Cobertura de testes > 95% para esta fase
- [ ] Todos os casos edge testados
- [ ] Testes de performance implementados
- [ ] Testes de recuperação de erros

### Performance
- [ ] Parsing em tempo O(n) para expressões simples
- [ ] Uso de memória otimizado
- [ ] Cache de estruturas frequentes
- [ ] Recuperação de erros eficiente

## Critérios de Aceitação

1. **Completude**: Todas as construções LX devem ser parseadas
2. **Correção**: AST gerado deve ser 100% correto
3. **Performance**: Deve ser pelo menos tão rápido quanto Menhir
4. **Erros**: Mensagens de erro claras e recuperação robusta
5. **Robustez**: Deve lidar com código malformado graciosamente

## Próximos Passos

Após conclusão desta fase, o projeto terá:
- Parser completo e robusto
- Base sólida para implementação do type checker
- Sistema de testes de parsing estabelecido
- Performance otimizada

**Duração Estimada**: 2-3 semanas
**Dependências**: Fase 2 (Lexer)
**Riscos**: Médios - parser manual pode ser complexo

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
## [2024-07-01] [Parser] Estrutura base e gerenciamento de tokens
- Implementada estrutura principal do parser recursivo descendente
- Sistema de gerenciamento de tokens com avanço e lookahead
- Tabela de precedência de operadores para parsing correto
- Base estabelecida para implementação das regras gramaticais

## [2024-07-03] [Parser] Expressões básicas e literais
- Parsing de literais: números, strings, booleanos, átomos, nil
- Reconhecimento de variáveis e atribuições simples
- Parsing de chamadas de função e chamadas externas (mod.fun)
- Sistema de posicionamento de erros em expressões

## [2024-07-05] [Parser] Estruturas de dados (tuplas, listas, cons)
- Implementado parsing de tuplas simples e aninhadas
- Reconhecimento de listas vazias e com elementos
- Parsing de cons (head | tail) para listas
- Validação de estruturas aninhadas complexas

## [2024-07-07] [Parser] Operadores e precedência
- Sistema completo de parsing de operadores aritméticos, lógicos e de comparação
- Implementação de precedência e associatividade correta
- Parsing de expressões complexas com múltiplos operadores
- Otimização para evitar ambiguidades gramaticais

## [2024-07-09] [Parser] Pattern matching e estruturas
- Parsing de padrões básicos: wildcard, variáveis, literais
- Reconhecimento de padrões de tuplas, listas e cons
- Parsing de pattern matching em atribuições e expressões
- Suporte a padrões aninhados e complexos

## [2024-07-11] [Parser] Estruturas de controle (if, case, with, for)
- Implementado parsing de if/else com expressões condicionais
- Reconhecimento de case com múltiplas cláusulas e padrões
- Parsing de with expressions para tratamento de erros
- Implementação de for loops e comprehensions

## [2024-07-13] [Parser] Funções e cláusulas
- Parsing de funções simples e multi-cláusula
- Reconhecimento de fun expressions anônimas
- Implementação de guards em funções e cláusulas
- Suporte a funções de alta ordem e closures

## [2024-07-15] [Parser] Records e maps
- Parsing de definições de records com tipos
- Reconhecimento de criação, acesso e atualização de records
- Implementação de maps com atom keys e string keys
- Suporte a pattern matching em records e maps

## [2024-07-17] [Parser] Componentes OTP
- Parsing de workers com funções obrigatórias
- Reconhecimento de supervisors com estratégias e children
- Implementação de validação de estruturas OTP
- Suporte a especificações e contratos

## [2024-07-19] [Parser] Recuperação de erros e validação
- Sistema robusto de recuperação de erros de parsing
- Mensagens de erro detalhadas com sugestões de correção
- Validação de AST gerado para consistência
- Testes de regressão para casos edge
```

**Regras:**
- Cada tarefa deve gerar uma entrada no changelog
- Incluir data, fase e descrição detalhada do que foi entregue
- Mencionar funcionalidades principais e impacto no projeto
- Manter histórico de dependências e mudanças arquiteturais