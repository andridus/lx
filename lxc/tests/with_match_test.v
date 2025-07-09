module tests

import frontend.lexer
import frontend.parser1
import ast

fn tokenize_code(code string) []lexer.Token {
	mut lexer_instance := lexer.new_lexer(code, 'test.lx')
	mut tokens := []lexer.Token{}

	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			panic('Lexical error: ${token.message}')
		}
		tokens << token
	}

	return tokens
}

fn test_with_expression_parsing() {
	// Test case: with :ok <- :ok do :ok end
	code := '
def a1() do
  with :ok <- :ok do
    :ok
  end
end
'

	tokens := tokenize_code(code)

	mut p := parser1.new_parser(tokens)
	result := p.parse_program() or {
		println('Parse errors:')
		for error in p.get_errors() {
			println('  ${error}')
		}
		assert false, 'Failed to parse with expression'
		return
	}

	// Check that we have a function with a with expression
	assert result.statements.len == 1

	if result.statements[0] is ast.FunctionStmt {
		func_stmt := result.statements[0] as ast.FunctionStmt
		assert func_stmt.name == 'a1'
		assert func_stmt.clauses.len == 1

		clause := func_stmt.clauses[0]
		assert clause.body.body.len == 1

		if clause.body.body[0] is ast.ExprStmt {
			expr_stmt := clause.body.body[0] as ast.ExprStmt
			assert expr_stmt.expr is ast.WithExpr

			with_expr := expr_stmt.expr as ast.WithExpr
			assert with_expr.bindings.len == 1

			binding := with_expr.bindings[0]
			assert binding.pattern is ast.AtomPattern
			assert binding.value is ast.LiteralExpr
		}
	}

	println('✓ With expression parsing test passed')
}

fn test_simple_match_expression_parsing() {
	// Test case: match :ok <- :ok
	code := '
def a2() do
  match :ok <- :ok
    :ok
end
'

	tokens := tokenize_code(code)

	mut p := parser1.new_parser(tokens)
	result := p.parse_program() or {
		println('Parse errors:')
		for error in p.get_errors() {
			println('  ${error}')
		}
		assert false, 'Failed to parse simple match expression'
		return
	}

	// Check that we have a function with a simple match expression
	assert result.statements.len == 1

	if result.statements[0] is ast.FunctionStmt {
		func_stmt := result.statements[0] as ast.FunctionStmt
		assert func_stmt.name == 'a2'
		assert func_stmt.clauses.len == 1

		clause := func_stmt.clauses[0]
		assert clause.body.body.len >= 1

		// The match expression should be in the function body
		mut found_match := false
		for stmt in clause.body.body {
			if stmt is ast.ExprStmt {
				expr_stmt := stmt as ast.ExprStmt
				if expr_stmt.expr is ast.SimpleMatchExpr {
					found_match = true
					match_expr := expr_stmt.expr as ast.SimpleMatchExpr
					assert match_expr.pattern is ast.AtomPattern
					assert match_expr.value is ast.LiteralExpr
					break
				}
			}
		}
		assert found_match, 'Should have found a SimpleMatchExpr'
	}

	println('✓ Simple match expression parsing test passed')
}

fn test_match_rescue_expression_parsing() {
	// Test case: match pattern <- value rescue error do error_handling end
	code := '
def a3() do
  match :ok <- :error rescue err do
    :error_handled
  end
end
'

	tokens := tokenize_code(code)

	mut p := parser1.new_parser(tokens)
	result := p.parse_program() or {
		println('Parse errors:')
		for error in p.get_errors() {
			println('  ${error}')
		}
		assert false, 'Failed to parse match rescue expression'
		return
	}

	// Check that we have a function with a match rescue expression
	assert result.statements.len == 1

	if result.statements[0] is ast.FunctionStmt {
		func_stmt := result.statements[0] as ast.FunctionStmt
		assert func_stmt.name == 'a3'
		assert func_stmt.clauses.len == 1

		clause := func_stmt.clauses[0]
		assert clause.body.body.len >= 1

		// The match rescue expression should be in the function body
		mut found_match_rescue := false
		for stmt in clause.body.body {
			if stmt is ast.ExprStmt {
				expr_stmt := stmt as ast.ExprStmt
				if expr_stmt.expr is ast.MatchRescueExpr {
					found_match_rescue = true
					match_rescue_expr := expr_stmt.expr as ast.MatchRescueExpr
					assert match_rescue_expr.pattern is ast.AtomPattern
					assert match_rescue_expr.value is ast.LiteralExpr
					assert match_rescue_expr.rescue_var == 'err'
					break
				}
			}
		}
		assert found_match_rescue, 'Should have found a MatchRescueExpr'
	}

	println('✓ Match rescue expression parsing test passed')
}
