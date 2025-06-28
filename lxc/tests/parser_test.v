module tests

import parser
import lexer
import ast
import v.test

// Test basic literal parsing
fn test_literal_parsing() {
	mut t := test.new_test()

	// Test integer literals
	tokens := [lexer.IntToken{ value: 42 }]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse integer') }

	match expr {
		ast.LiteralExpr {
			match expr.value {
				ast.IntegerLiteral { assert expr.value.value == 42 }
				else { assert false, 'Expected IntegerLiteral' }
			}
		}
		else { assert false, 'Expected LiteralExpr' }
	}

	// Test string literals
	tokens2 := [lexer.StringToken{ value: 'hello' }]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse string') }

	match expr2 {
		ast.LiteralExpr {
			match expr2.value {
				ast.StringLiteral { assert expr2.value.value == 'hello' }
				else { assert false, 'Expected StringLiteral' }
			}
		}
		else { assert false, 'Expected LiteralExpr' }
	}

	// Test boolean literals
	tokens3 := [lexer.BoolToken{ value: true }]
	mut parser3 := parser.new_expression_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse boolean') }

	match expr3 {
		ast.LiteralExpr {
			match expr3.value {
				ast.BooleanLiteral { assert expr3.value.value == true }
				else { assert false, 'Expected BooleanLiteral' }
			}
		}
		else { assert false, 'Expected LiteralExpr' }
	}

	// Test atom literals
	tokens4 := [lexer.AtomToken{ value: 'ok' }]
	mut parser4 := parser.new_expression_parser(tokens4)
	expr4 := parser4.parse_expression() or { panic('Failed to parse atom') }

	match expr4 {
		ast.LiteralExpr {
			match expr4.value {
				ast.AtomLiteral { assert expr4.value.value == 'ok' }
				else { assert false, 'Expected AtomLiteral' }
			}
		}
		else { assert false, 'Expected LiteralExpr' }
	}

	t.assert(true)
}

// Test binary expression parsing
fn test_binary_expression_parsing() {
	mut t := test.new_test()

	// Test addition
	tokens := [
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.plus,
		lexer.IntToken{ value: 2 }
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse binary expression') }

	match expr {
		ast.BinaryExpr {
			assert expr.op == .add
			match expr.left {
				ast.LiteralExpr {
					match expr.left.value {
						ast.IntegerLiteral { assert expr.left.value.value == 1 }
						else { assert false, 'Expected IntegerLiteral' }
					}
				}
				else { assert false, 'Expected LiteralExpr' }
			}
			match expr.right {
				ast.LiteralExpr {
					match expr.right.value {
						ast.IntegerLiteral { assert expr.right.value.value == 2 }
						else { assert false, 'Expected IntegerLiteral' }
					}
				}
				else { assert false, 'Expected LiteralExpr' }
			}
		}
		else { assert false, 'Expected BinaryExpr' }
	}

	// Test comparison
	tokens2 := [
		lexer.IntToken{ value: 5 },
		lexer.OperatorToken.gt,
		lexer.IntToken{ value: 3 }
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse comparison') }

	match expr2 {
		ast.BinaryExpr {
			assert expr2.op == .greater_than
		}
		else { assert false, 'Expected BinaryExpr' }
	}

	t.assert(true)
}

// Test list and tuple parsing
fn test_data_structure_parsing() {
	mut t := test.new_test()

	// Test list parsing
	tokens := [
		lexer.PunctuationToken.lbracket,
		lexer.IntToken{ value: 1 },
		lexer.PunctuationToken.comma,
		lexer.IntToken{ value: 2 },
		lexer.PunctuationToken.comma,
		lexer.IntToken{ value: 3 },
		lexer.PunctuationToken.rbracket
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse list') }

	match expr {
		ast.ListLiteralExpr {
			assert expr.elements.len == 3
		}
		else { assert false, 'Expected ListLiteralExpr' }
	}

	// Test tuple parsing
	tokens2 := [
		lexer.PunctuationToken.lbrace,
		lexer.StringToken{ value: 'hello' },
		lexer.PunctuationToken.comma,
		lexer.IntToken{ value: 42 },
		lexer.PunctuationToken.rbrace
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse tuple') }

	match expr2 {
		ast.TupleExpr {
			assert expr2.elements.len == 2
		}
		else { assert false, 'Expected TupleExpr' }
	}

	t.assert(true)
}

// Test map parsing
fn test_map_parsing() {
	mut t := test.new_test()

	// Test map with atom keys
	tokens := [
		lexer.OperatorToken.record_update,
		lexer.PunctuationToken.lbrace,
		lexer.AtomToken{ value: 'name' },
		lexer.PunctuationToken.colon,
		lexer.StringToken{ value: 'Alice' },
		lexer.PunctuationToken.comma,
		lexer.AtomToken{ value: 'age' },
		lexer.PunctuationToken.colon,
		lexer.IntToken{ value: 30 },
		lexer.PunctuationToken.rbrace
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse map') }

	match expr {
		ast.MapLiteralExpr {
			assert expr.entries.len == 2
		}
		else { assert false, 'Expected MapLiteralExpr' }
	}

	t.assert(true)
}

// Test function parsing
fn test_function_parsing() {
	mut t := test.new_test()

	// Test function definition
	tokens := [
		lexer.KeywordToken.def,
		lexer.IdentToken{ value: 'add' },
		lexer.PunctuationToken.lparen,
		lexer.IdentToken{ value: 'x' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'y' },
		lexer.PunctuationToken.rparen,
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.plus,
		lexer.IdentToken{ value: 'y' },
		lexer.KeywordToken.end_
	]
	mut parser := parser.new_statement_parser(tokens)
	stmt := parser.parse_statement() or { panic('Failed to parse function') }

	match stmt {
		ast.FunctionStmt {
			assert stmt.name == 'add'
			assert stmt.clauses.len == 1
		}
		else { assert false, 'Expected FunctionStmt' }
	}

	t.assert(true)
}

// Test record parsing
fn test_record_parsing() {
	mut t := test.new_test()

	// Test record definition
	tokens := [
		lexer.KeywordToken.record,
		lexer.IdentToken{ value: 'Person' },
		lexer.PunctuationToken.lbrace,
		lexer.IdentToken{ value: 'name' },
		lexer.OperatorToken.type_cons,
		lexer.IdentToken{ value: 'string' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'age' },
		lexer.OperatorToken.type_cons,
		lexer.IdentToken{ value: 'integer' },
		lexer.PunctuationToken.rbrace
	]
	mut parser := parser.new_statement_parser(tokens)
	stmt := parser.parse_statement() or { panic('Failed to parse record') }

	match stmt {
		ast.RecordDefStmt {
			assert stmt.name == 'Person'
			assert stmt.fields.len == 2
		}
		else { assert false, 'Expected RecordDefStmt' }
	}

	t.assert(true)
}

// Test pattern matching
fn test_pattern_matching() {
	mut t := test.new_test()

	// Test case expression
	tokens := [
		lexer.KeywordToken.case_,
		lexer.IdentToken{ value: 'value' },
		lexer.KeywordToken.do_,
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'one' },
		lexer.KeywordToken.end_
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse case') }

	match expr {
		ast.CaseExpr {
			assert expr.cases.len == 1
		}
		else { assert false, 'Expected CaseExpr' }
	}

	t.assert(true)
}

// Test control flow
fn test_control_flow() {
	mut t := test.new_test()

	// Test if expression
	tokens := [
		lexer.KeywordToken.if_,
		lexer.BoolToken{ value: true },
		lexer.KeywordToken.do_,
		lexer.StringToken{ value: 'yes' },
		lexer.KeywordToken.end_
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse if') }

	match expr {
		ast.IfExpr {
			assert expr.then_body.len == 1
		}
		else { assert false, 'Expected IfExpr' }
	}

	// Test for expression (list comprehension)
	tokens2 := [
		lexer.KeywordToken.for_,
		lexer.IdentToken{ value: 'x' },
		lexer.KeywordToken.in,
		lexer.IdentToken{ value: 'list' },
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.mult,
		lexer.IntToken{ value: 2 },
		lexer.KeywordToken.end_
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse for') }

	match expr2 {
		ast.ForExpr {
			assert expr2.body.len == 1
		}
		else { assert false, 'Expected ForExpr' }
	}

	t.assert(true)
}

// Test message passing
fn test_message_passing() {
	mut t := test.new_test()

	// Test send expression
	tokens := [
		lexer.IdentToken{ value: 'pid' },
		lexer.OperatorToken.send,
		lexer.StringToken{ value: 'hello' }
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse send') }

	match expr {
		ast.SendExpr {
			match expr.message {
				ast.LiteralExpr {
					match expr.message.value {
						ast.StringLiteral { assert expr.message.value.value == 'hello' }
						else { assert false, 'Expected StringLiteral' }
					}
				}
				else { assert false, 'Expected LiteralExpr' }
			}
		}
		else { assert false, 'Expected SendExpr' }
	}

	// Test receive expression
	tokens2 := [
		lexer.KeywordToken.receive,
		lexer.KeywordToken.do_,
		lexer.StringToken{ value: 'hello' },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'received' },
		lexer.KeywordToken.end_
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse receive') }

	match expr2 {
		ast.ReceiveExpr {
			assert expr2.cases.len == 1
		}
		else { assert false, 'Expected ReceiveExpr' }
	}

	t.assert(true)
}

// Test module parsing
fn test_module_parsing() {
	mut t := test.new_test()

	// Test module with imports and exports
	tokens := [
		lexer.KeywordToken.module,
		lexer.IdentToken{ value: 'math' },
		lexer.PunctuationToken.lbracket,
		lexer.IdentToken{ value: 'add' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'subtract' },
		lexer.PunctuationToken.rbracket,
		lexer.KeywordToken.import,
		lexer.IdentToken{ value: 'stdlib' },
		lexer.PunctuationToken.lbrace,
		lexer.KeywordToken.def,
		lexer.IdentToken{ value: 'add' },
		lexer.PunctuationToken.lparen,
		lexer.IdentToken{ value: 'x' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'y' },
		lexer.PunctuationToken.rparen,
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.plus,
		lexer.IdentToken{ value: 'y' },
		lexer.KeywordToken.end_,
		lexer.PunctuationToken.rbrace
	]
	mut parser := parser.new_main_parser(tokens)
	module := parser.parse_module() or { panic('Failed to parse module') }

	assert module.name == 'math'
	assert module.exports.len == 2
	assert module.exports[0] == 'add'
	assert module.exports[1] == 'subtract'
	assert module.statements.len == 1

	t.assert(true)
}

// Test error handling
fn test_error_handling() {
	mut t := test.new_test()

	// Test invalid syntax
	tokens := [
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.plus,
		// Missing right operand
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression()

	// Should return none due to error
	assert expr == none
	assert parser.has_errors()

	t.assert(true)
}

// Test complex expressions
fn test_complex_expressions() {
	mut t := test.new_test()

	// Test nested expressions
	tokens := [
		lexer.PunctuationToken.lparen,
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.plus,
		lexer.IntToken{ value: 2 },
		lexer.PunctuationToken.rparen,
		lexer.OperatorToken.mult,
		lexer.IntToken{ value: 3 }
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse complex expression') }

	match expr {
		ast.BinaryExpr {
			assert expr.op == .multiply
			match expr.left {
				ast.BinaryExpr {
					assert expr.left.op == .add
				}
				else { assert false, 'Expected nested BinaryExpr' }
			}
		}
		else { assert false, 'Expected BinaryExpr' }
	}

	t.assert(true)
}

// Test precedence
fn test_operator_precedence() {
	mut t := test.new_test()

	// Test that multiplication has higher precedence than addition
	tokens := [
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.plus,
		lexer.IntToken{ value: 2 },
		lexer.OperatorToken.mult,
		lexer.IntToken{ value: 3 }
	]
	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or { panic('Failed to parse precedence test') }

	match expr {
		ast.BinaryExpr {
			assert expr.op == .add
			match expr.right {
				ast.BinaryExpr {
					assert expr.right.op == .multiply
				}
				else { assert false, 'Expected multiplication to be parsed first' }
			}
		}
		else { assert false, 'Expected BinaryExpr' }
	}

	t.assert(true)
}