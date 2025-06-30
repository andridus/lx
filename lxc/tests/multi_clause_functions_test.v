module main

import analysis.linter
import frontend.parser
import frontend.lexer
import ast
import errors

fn test_multi_clause_function_parsing() {
	// Test parsing of multi-clause function with correct arity order
	tokens := [
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.module)),
		lexer.Token(lexer.new_ident_token('test_module')),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('f')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('f')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_eof_token()),
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
	assert func_stmt.name == 'f'
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
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.module)),
		lexer.Token(lexer.new_ident_token('test_module')),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('process')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		// First clause: process() -> :ok
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_atom_token('ok')),
		// Second clause: process(x) -> {:ok, x}
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_atom_token('ok')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		// Third clause: process(x, y) -> {:ok, x, y}
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_atom_token('ok')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
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
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.module)),
		lexer.Token(lexer.new_ident_token('test_module')),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('calculate')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		// First clause: calculate(x) do x * 2 end
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.mult)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
		// Second clause: calculate(x, y) do result = x + y; result * 2 end
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		lexer.Token(lexer.new_ident_token('result')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.plus)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.semicolon)),
		lexer.Token(lexer.new_ident_token('result')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.mult)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
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
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.module)),
		lexer.Token(lexer.new_ident_token('test_module')),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('pair')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		// First clause: pair(x) -> {x, nil}
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.nil_)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		// Second clause: pair(x, y) -> {x, y}
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
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
