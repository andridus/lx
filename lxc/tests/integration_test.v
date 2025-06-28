module tests

import parser
import lexer
import ast
import v.test

// Test the complete parsing pipeline from tokens to AST
fn test_complete_parsing_pipeline() {
	mut t := test.new_test()

	// Test a complete LX program
	program := '
module math_utils [add, multiply] {
  def add(x, y) do
    x + y
  end

  def multiply(x, y) do
    x * y
  end

  record Point {
    x :: integer,
    y :: integer
  }

  def distance(p1, p2) do
    dx = p1.x - p2.x
    dy = p1.y - p2.y
    math.sqrt(dx * dx + dy * dy)
  end
}'

	// For now, we'll create tokens manually since we don't have a lexer yet
	// In a real implementation, this would come from the lexer
	tokens := create_tokens_for_program()

	mut parser := parser.new_main_parser(tokens)
	module := parser.parse_module() or {
		t.fail('Failed to parse module')
		return
	}

	// Verify the module structure
	assert module.name == 'math_utils'
	assert module.exports.len == 2
	assert module.exports[0] == 'add'
	assert module.exports[1] == 'multiply'
	assert module.statements.len == 4 // 2 functions + 1 record + 1 function

	// Verify the first function
	match module.statements[0] {
		ast.FunctionStmt {
			assert it.name == 'add'
			assert it.clauses.len == 1

			clause := it.clauses[0]
			assert clause.parameters.len == 2
			assert clause.body.len == 1
		}
		else {
			t.fail('Expected FunctionStmt for add function')
		}
	}

	// Verify the second function
	match module.statements[1] {
		ast.FunctionStmt {
			assert it.name == 'multiply'
			assert it.clauses.len == 1
		}
		else {
			t.fail('Expected FunctionStmt for multiply function')
		}
	}

	// Verify the record definition
	match module.statements[2] {
		ast.RecordDefStmt {
			assert it.name == 'Point'
			assert it.fields.len == 2
			assert it.fields[0].name == 'x'
			assert it.fields[0].field_type == .integer
			assert it.fields[1].name == 'y'
			assert it.fields[1].field_type == .integer
		}
		else {
			t.fail('Expected RecordDefStmt for Point record')
		}
	}

	// Verify the third function
	match module.statements[3] {
		ast.FunctionStmt {
			assert it.name == 'distance'
			assert it.clauses.len == 1

			clause := it.clauses[0]
			assert clause.parameters.len == 2
			assert clause.body.len == 4 // 2 assignments + 1 expression
		}
		else {
			t.fail('Expected FunctionStmt for distance function')
		}
	}

	t.assert(true)
}

// Test pattern matching expressions
fn test_pattern_matching_expressions() {
	mut t := test.new_test()

	// Test case expression with multiple patterns
	tokens := [
		lexer.KeywordToken.case_,
		lexer.IdentToken{ value: 'value' },
		lexer.KeywordToken.do_,
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'one' },
		lexer.IntToken{ value: 2 },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'two' },
		lexer.IdentToken{ value: '_' },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'other' },
		lexer.KeywordToken.end_
	]

	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or {
		t.fail('Failed to parse case expression')
		return
	}

	match expr {
		ast.CaseExpr {
			assert it.cases.len == 3

			// Check first case
			case1 := it.cases[0]
			match case1.pattern {
				ast.LiteralPattern {
					match case1.pattern.value {
						ast.IntegerLiteral { assert case1.pattern.value.value == 1 }
						else { t.fail('Expected IntegerLiteral in first case') }
					}
				}
				else { t.fail('Expected LiteralPattern in first case') }
			}

			// Check second case
			case2 := it.cases[1]
			match case2.pattern {
				ast.LiteralPattern {
					match case2.pattern.value {
						ast.IntegerLiteral { assert case2.pattern.value.value == 2 }
						else { t.fail('Expected IntegerLiteral in second case') }
					}
				}
				else { t.fail('Expected LiteralPattern in second case') }
			}

			// Check third case (wildcard)
			case3 := it.cases[2]
			match case3.pattern {
				ast.WildcardPattern { /* OK */ }
				else { t.fail('Expected WildcardPattern in third case') }
			}
		}
		else {
			t.fail('Expected CaseExpr')
		}
	}

	t.assert(true)
}

// Test data structure expressions
fn test_data_structure_expressions() {
	mut t := test.new_test()

	// Test map with different key types
	tokens := [
		lexer.OperatorToken.record_update,
		lexer.PunctuationToken.lbrace,
		lexer.AtomToken{ value: 'name' },
		lexer.PunctuationToken.colon,
		lexer.StringToken{ value: 'Alice' },
		lexer.PunctuationToken.comma,
		lexer.StringToken{ value: 'age' },
		lexer.OperatorToken.arrow,
		lexer.IntToken{ value: 30 },
		lexer.PunctuationToken.comma,
		lexer.IntToken{ value: 1 },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'first' },
		lexer.PunctuationToken.rbrace
	]

	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or {
		t.fail('Failed to parse map expression')
		return
	}

	match expr {
		ast.MapLiteralExpr {
			assert it.entries.len == 3

			// Check first entry (atom key)
			entry1 := it.entries[0]
			match entry1.key {
				ast.LiteralExpr {
					match entry1.key.value {
						ast.AtomLiteral { assert entry1.key.value.value == 'name' }
						else { t.fail('Expected AtomLiteral key') }
					}
				}
				else { t.fail('Expected LiteralExpr key') }
			}

			// Check second entry (string key)
			entry2 := it.entries[1]
			match entry2.key {
				ast.LiteralExpr {
					match entry2.key.value {
						ast.StringLiteral { assert entry2.key.value.value == 'age' }
						else { t.fail('Expected StringLiteral key') }
					}
				}
				else { t.fail('Expected LiteralExpr key') }
			}

			// Check third entry (integer key)
			entry3 := it.entries[2]
			match entry3.key {
				ast.LiteralExpr {
					match entry3.key.value {
						ast.IntegerLiteral { assert entry3.key.value.value == 1 }
						else { t.fail('Expected IntegerLiteral key') }
					}
				}
				else { t.fail('Expected LiteralExpr key') }
			}
		}
		else {
			t.fail('Expected MapLiteralExpr')
		}
	}

	t.assert(true)
}

// Test control flow expressions
fn test_control_flow_expressions() {
	mut t := test.new_test()

	// Test if-else expression
	tokens := [
		lexer.KeywordToken.if_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.gt,
		lexer.IntToken{ value: 0 },
		lexer.KeywordToken.do_,
		lexer.StringToken{ value: 'positive' },
		lexer.KeywordToken.else_,
		lexer.StringToken{ value: 'negative' },
		lexer.KeywordToken.end_
	]

	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or {
		t.fail('Failed to parse if-else expression')
		return
	}

	match expr {
		ast.IfExpr {
			assert it.then_body.len == 1
			assert it.else_body.len == 1

			// Check condition
			match it.condition {
				ast.BinaryExpr {
					assert it.condition.op == .greater_than
				}
				else { t.fail('Expected BinaryExpr condition') }
			}
		}
		else {
			t.fail('Expected IfExpr')
		}
	}

	// Test for expression (list comprehension)
	tokens2 := [
		lexer.KeywordToken.for_,
		lexer.IdentToken{ value: 'x' },
		lexer.KeywordToken.in,
		lexer.IdentToken{ value: 'list' },
		lexer.KeywordToken.when,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.gt,
		lexer.IntToken{ value: 0 },
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.mult,
		lexer.IntToken{ value: 2 },
		lexer.KeywordToken.end_
	]

	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or {
		t.fail('Failed to parse for expression')
		return
	}

	match expr2 {
		ast.ForExpr {
			assert it.body.len == 1

			// Check pattern
			match it.pattern {
				ast.VarPattern {
					assert it.pattern.name == 'x'
				}
				else { t.fail('Expected VarPattern') }
			}

			// Check guard
			match it.guard {
				ast.BinaryExpr {
					assert it.guard.op == .greater_than
				}
				else { t.fail('Expected BinaryExpr guard') }
			}
		}
		else {
			t.fail('Expected ForExpr')
		}
	}

	t.assert(true)
}

// Test message passing expressions
fn test_message_passing_expressions() {
	mut t := test.new_test()

	// Test send expression
	tokens := [
		lexer.IdentToken{ value: 'pid' },
		lexer.OperatorToken.send,
		lexer.PunctuationToken.lbrace,
		lexer.AtomToken{ value: 'hello' },
		lexer.PunctuationToken.comma,
		lexer.StringToken{ value: 'world' },
		lexer.PunctuationToken.rbrace
	]

	mut parser := parser.new_expression_parser(tokens)
	expr := parser.parse_expression() or {
		t.fail('Failed to parse send expression')
		return
	}

	match expr {
		ast.SendExpr {
			// Check message is a tuple
			match expr.message {
				ast.TupleExpr {
					assert expr.message.elements.len == 2
				}
				else { t.fail('Expected TupleExpr message') }
			}
		}
		else {
			t.fail('Expected SendExpr')
		}
	}

	// Test receive expression with timeout
	tokens2 := [
		lexer.KeywordToken.receive,
		lexer.KeywordToken.do_,
		lexer.AtomToken{ value: 'hello' },
		lexer.OperatorToken.arrow,
		lexer.StringToken{ value: 'received hello' },
		lexer.KeywordToken.after,
		lexer.IntToken{ value: 5000 },
		lexer.KeywordToken.do_,
		lexer.AtomToken{ value: 'timeout' },
		lexer.KeywordToken.end_
	]

	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or {
		t.fail('Failed to parse receive expression')
		return
	}

	match expr2 {
		ast.ReceiveExpr {
			assert it.cases.len == 1

			// Check timeout
			match it.timeout {
				ast.LiteralExpr {
					match it.timeout.value {
						ast.IntegerLiteral { assert it.timeout.value.value == 5000 }
						else { t.fail('Expected IntegerLiteral timeout') }
					}
				}
				else { t.fail('Expected LiteralExpr timeout') }
			}
		}
		else {
			t.fail('Expected ReceiveExpr')
		}
	}

	t.assert(true)
}

// Helper function to create tokens for the test program
fn create_tokens_for_program() []lexer.Token {
	return [
		lexer.KeywordToken.module,
		lexer.IdentToken{ value: 'math_utils' },
		lexer.PunctuationToken.lbracket,
		lexer.IdentToken{ value: 'add' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'multiply' },
		lexer.PunctuationToken.rbracket,
		lexer.PunctuationToken.lbrace,

		// First function: add
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

		// Second function: multiply
		lexer.KeywordToken.def,
		lexer.IdentToken{ value: 'multiply' },
		lexer.PunctuationToken.lparen,
		lexer.IdentToken{ value: 'x' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'y' },
		lexer.PunctuationToken.rparen,
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.mult,
		lexer.IdentToken{ value: 'y' },
		lexer.KeywordToken.end_,

		// Record definition: Point
		lexer.KeywordToken.record,
		lexer.IdentToken{ value: 'Point' },
		lexer.PunctuationToken.lbrace,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.type_cons,
		lexer.IdentToken{ value: 'integer' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'y' },
		lexer.OperatorToken.type_cons,
		lexer.IdentToken{ value: 'integer' },
		lexer.PunctuationToken.rbrace,

		// Third function: distance
		lexer.KeywordToken.def,
		lexer.IdentToken{ value: 'distance' },
		lexer.PunctuationToken.lparen,
		lexer.IdentToken{ value: 'p1' },
		lexer.PunctuationToken.comma,
		lexer.IdentToken{ value: 'p2' },
		lexer.PunctuationToken.rparen,
		lexer.KeywordToken.do_,
		lexer.IdentToken{ value: 'dx' },
		lexer.OperatorToken.assign,
		lexer.IdentToken{ value: 'p1' },
		lexer.OperatorToken.dot,
		lexer.IdentToken{ value: 'x' },
		lexer.OperatorToken.minus,
		lexer.IdentToken{ value: 'p2' },
		lexer.OperatorToken.dot,
		lexer.IdentToken{ value: 'x' },
		lexer.IdentToken{ value: 'dy' },
		lexer.OperatorToken.assign,
		lexer.IdentToken{ value: 'p1' },
		lexer.OperatorToken.dot,
		lexer.IdentToken{ value: 'y' },
		lexer.OperatorToken.minus,
		lexer.IdentToken{ value: 'p2' },
		lexer.OperatorToken.dot,
		lexer.IdentToken{ value: 'y' },
		lexer.IdentToken{ value: 'math' },
		lexer.OperatorToken.dot,
		lexer.IdentToken{ value: 'sqrt' },
		lexer.PunctuationToken.lparen,
		lexer.IdentToken{ value: 'dx' },
		lexer.OperatorToken.mult,
		lexer.IdentToken{ value: 'dx' },
		lexer.OperatorToken.plus,
		lexer.IdentToken{ value: 'dy' },
		lexer.OperatorToken.mult,
		lexer.IdentToken{ value: 'dy' },
		lexer.PunctuationToken.rparen,
		lexer.KeywordToken.end_,

		lexer.PunctuationToken.rbrace
	]
}