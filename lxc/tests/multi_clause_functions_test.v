module main

import analysis.linter
import frontend.parser
import frontend.lexer
import ast
import errors

fn test_multi_clause_function_parsing() {
	// Test parsing of multi-clause function with correct arity order
	tokens := [
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'test_module'
		}),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'factorial'
		}),
		lexer.Token(lexer.KeywordToken.do_),
		// First clause: factorial(0) -> 1
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IntToken{
			value: 0
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		// Second clause: factorial(N) when N > 0 -> N * factorial(N - 1)
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'N'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.when),
		lexer.Token(lexer.IdentToken{
			value: 'N'
		}),
		lexer.Token(lexer.OperatorToken.gt),
		lexer.Token(lexer.IntToken{
			value: 0
		}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.IdentToken{
			value: 'N'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IdentToken{
			value: 'factorial'
		}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'N'
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or {
		panic('Failed to parse multi-clause function')
	}

	// Find the function statement
	mut func_stmt := ast.FunctionStmt{}
	mut found := false
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt = stmt as ast.FunctionStmt
				found = true
				break
			}
			else {
				// Skip non-function statements
			}
		}
	}

	assert found == true
	assert func_stmt.name == 'factorial'
	assert func_stmt.clauses.len == 2

	// Check first clause (arity 1)
	clause1 := func_stmt.clauses[0]
	assert clause1.parameters.len == 1

	// Check second clause (arity 1 with guard)
	clause2 := func_stmt.clauses[1]
	assert clause2.parameters.len == 1
}

fn test_multi_clause_function_with_different_arities() {
	// Test function with clauses of different arities in correct order
	tokens := [
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'test_module'
		}),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'process'
		}),
		lexer.Token(lexer.KeywordToken.do_),
		// First clause: process() -> :ok
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.AtomToken{
			value: 'ok'
		}),
		// Second clause: process(x) -> {:ok, x}
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.AtomToken{
			value: 'ok'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		// Third clause: process(x, y) -> {:ok, x, y}
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.AtomToken{
			value: 'ok'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or {
		panic('Failed to parse multi-clause function with different arities')
	}

	// Find the function statement
	mut func_stmt := ast.FunctionStmt{}
	mut found := false
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt = stmt as ast.FunctionStmt
				found = true
				break
			}
			else {
				// Skip non-function statements
			}
		}
	}

	assert found == true
	assert func_stmt.name == 'process'
	assert func_stmt.clauses.len == 3

	// Check arities are in ascending order
	assert func_stmt.clauses[0].parameters.len == 0 // arity 0
	assert func_stmt.clauses[1].parameters.len == 1 // arity 1
	assert func_stmt.clauses[2].parameters.len == 2 // arity 2
}

fn test_multi_clause_function_with_blocks() {
	// Test function with clauses containing blocks
	tokens := [
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'test_module'
		}),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'calculate'
		}),
		lexer.Token(lexer.KeywordToken.do_),
		// First clause: calculate(x) do x * 2 end
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.do_),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.KeywordToken.end_),
		// Second clause: calculate(x, y) do result = x + y; result * 2 end
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.do_),
		lexer.Token(lexer.IdentToken{
			value: 'result'
		}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.semicolon),
		lexer.Token(lexer.IdentToken{
			value: 'result'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or {
		panic('Failed to parse multi-clause function with blocks')
	}

	// Find the function statement
	mut func_stmt := ast.FunctionStmt{}
	mut found := false
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt = stmt as ast.FunctionStmt
				found = true
				break
			}
			else {
				// Skip non-function statements
			}
		}
	}

	assert found == true
	assert func_stmt.name == 'calculate'
	assert func_stmt.clauses.len == 2

	// Check first clause has simple expression
	clause1 := func_stmt.clauses[0]
	assert clause1.parameters.len == 1
	assert clause1.body.len == 1

	// Check second clause has block with multiple statements
	clause2 := func_stmt.clauses[1]
	assert clause2.parameters.len == 2
	assert clause2.body.len == 2 // assignment + expression
}

fn test_multi_clause_function_with_tuples() {
	// Test function with clauses returning tuples
	tokens := [
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'test_module'
		}),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'pair'
		}),
		lexer.Token(lexer.KeywordToken.do_),
		// First clause: pair(x) -> {x, nil}
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.KeywordToken.nil_),
		lexer.Token(lexer.PunctuationToken.rbrace),
		// Second clause: pair(x, y) -> {x, y}
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or {
		panic('Failed to parse multi-clause function with tuples')
	}

	// Find the function statement
	mut func_stmt := ast.FunctionStmt{}
	mut found := false
	for stmt in module_stmt.statements {
		match stmt {
			ast.FunctionStmt {
				func_stmt = stmt as ast.FunctionStmt
				found = true
				break
			}
			else {
				// Skip non-function statements
			}
		}
	}

	assert found == true
	assert func_stmt.name == 'pair'
	assert func_stmt.clauses.len == 2

	// Check arities
	assert func_stmt.clauses[0].parameters.len == 1
	assert func_stmt.clauses[1].parameters.len == 2
}

fn test_linter_arity_order_validation_correct() {
	// Test that linter accepts correctly ordered function clauses
	module_stmt := ast.ModuleStmt{
		name:       'test_module'
		exports:    []
		statements: [
			ast.Stmt(ast.FunctionStmt{
				name:    'test'
				clauses: [
					ast.FunctionClause{
						parameters: []ast.Pattern{} // arity 0
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.LiteralExpr{
									value: ast.Literal(ast.AtomLiteral{
										value: 'ok'
									})
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
						] // arity 1
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.VariableExpr{
									name: 'x'
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
							ast.Pattern(ast.VarPattern{
								name: 'y'
							}),
						] // arity 2
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.BinaryExpr{
									op:    ast.BinaryOp.add
									left:  ast.Expr(ast.VariableExpr{
										name: 'x'
									})
									right: ast.Expr(ast.VariableExpr{
										name: 'y'
									})
								})
							}),
						]
					},
				]
			}),
		]
	}

	mut linter_instance := linter.new_linter()
	result := linter_instance.lint_module(module_stmt)

	// Should have no errors
	assert result.success == true
	assert result.errors.len == 0
}

fn test_linter_arity_order_validation_incorrect() {
	// Test that linter rejects incorrectly ordered function clauses
	module_stmt := ast.ModuleStmt{
		name:       'test_module'
		exports:    []
		statements: [
			ast.Stmt(ast.FunctionStmt{
				name:    'test'
				clauses: [
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
							ast.Pattern(ast.VarPattern{
								name: 'y'
							}),
						] // arity 2 - WRONG ORDER
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.BinaryExpr{
									op:    ast.BinaryOp.add
									left:  ast.Expr(ast.VariableExpr{
										name: 'x'
									})
									right: ast.Expr(ast.VariableExpr{
										name: 'y'
									})
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
						] // arity 1 - WRONG ORDER
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.VariableExpr{
									name: 'x'
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: []ast.Pattern{} // arity 0 - WRONG ORDER
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.LiteralExpr{
									value: ast.Literal(ast.AtomLiteral{
										value: 'ok'
									})
								})
							}),
						]
					},
				]
			}),
		]
	}

	mut linter_instance := linter.new_linter()
	result := linter_instance.lint_module(module_stmt)

	// Should have errors for incorrect arity order
	assert result.success == false
	assert result.errors.len > 0

	// Check that the error message mentions arity ordering
	error_found := result.errors.any(fn (err errors.CompilationError) bool {
		return err.message.contains('arity') || err.message.contains('order')
	})
	assert error_found == true
}

fn test_linter_arity_order_validation_mixed() {
	// Test that linter correctly identifies mixed arity ordering issues
	module_stmt := ast.ModuleStmt{
		name:       'test_module'
		exports:    []
		statements: [
			ast.Stmt(ast.FunctionStmt{
				name:    'test'
				clauses: [
					ast.FunctionClause{
						parameters: []ast.Pattern{} // arity 0 - correct
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.LiteralExpr{
									value: ast.Literal(ast.AtomLiteral{
										value: 'ok'
									})
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
							ast.Pattern(ast.VarPattern{
								name: 'y'
							}),
						] // arity 2 - WRONG ORDER (should be after arity 1)
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.BinaryExpr{
									op:    ast.BinaryOp.add
									left:  ast.Expr(ast.VariableExpr{
										name: 'x'
									})
									right: ast.Expr(ast.VariableExpr{
										name: 'y'
									})
								})
							}),
						]
					},
					ast.FunctionClause{
						parameters: [
							ast.Pattern(ast.VarPattern{
								name: 'x'
							}),
						] // arity 1 - WRONG ORDER (should be before arity 2)
						guard:      ast.Expr(ast.LiteralExpr{
							value: ast.Literal(ast.BooleanLiteral{
								value: true
							})
						})
						body:       [
							ast.Stmt(ast.ExprStmt{
								expr: ast.Expr(ast.VariableExpr{
									name: 'x'
								})
							}),
						]
					},
				]
			}),
		]
	}

	mut linter_instance := linter.new_linter()
	result := linter_instance.lint_module(module_stmt)

	// Should have errors for incorrect arity order
	assert result.success == false
	assert result.errors.len > 0

	// Check that the error message mentions arity ordering
	error_found := result.errors.any(fn (err errors.CompilationError) bool {
		return err.message.contains('arity') || err.message.contains('order')
	})
	assert error_found == true
}

fn main() {
	println('Running multi-clause function tests...')

	test_multi_clause_function_parsing()
	test_multi_clause_function_with_different_arities()
	test_multi_clause_function_with_blocks()
	test_multi_clause_function_with_tuples()
	test_linter_arity_order_validation_correct()
	test_linter_arity_order_validation_incorrect()
	test_linter_arity_order_validation_mixed()

	println('All multi-clause function tests passed!')
}
