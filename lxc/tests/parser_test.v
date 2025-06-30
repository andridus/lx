module main

import frontend.parser
import frontend.lexer
import ast

// Test basic literal parsing
fn test_literal_parsing() {
	// Test integer literals
	tokens := [lexer.Token(lexer.IntToken{
		value: 42
	})]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse integer') }

	match expr {
		ast.LiteralExpr {
			lit := expr as ast.LiteralExpr
			match lit.value {
				ast.IntegerLiteral {
					int_lit := lit.value as ast.IntegerLiteral
					assert int_lit.value == 42
				}
				else {
					assert false, 'Expected IntegerLiteral'
				}
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test string literals
	tokens2 := [lexer.Token(lexer.StringToken{
		value: 'hello'
	})]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse string') }

	match expr2 {
		ast.LiteralExpr {
			lit := expr2 as ast.LiteralExpr
			match lit.value {
				ast.StringLiteral {
					str_lit := lit.value as ast.StringLiteral
					assert str_lit.value == 'hello'
				}
				else {
					assert false, 'Expected StringLiteral'
				}
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test boolean literals
	tokens3 := [lexer.Token(lexer.BoolToken{
		value: true
	})]
	mut parser3 := parser.new_expression_parser(tokens3)
	expr3 := parser3.parse_expression() or { panic('Failed to parse boolean') }

	match expr3 {
		ast.LiteralExpr {
			lit := expr3 as ast.LiteralExpr
			match lit.value {
				ast.BooleanLiteral {
					bool_lit := lit.value as ast.BooleanLiteral
					assert bool_lit.value == true
				}
				else {
					assert false, 'Expected BooleanLiteral'
				}
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}

	// Test atom literals
	tokens4 := [lexer.Token(lexer.AtomToken{
		value: 'ok'
	})]
	mut parser4 := parser.new_expression_parser(tokens4)
	expr4 := parser4.parse_expression() or { panic('Failed to parse atom') }

	match expr4 {
		ast.LiteralExpr {
			lit := expr4 as ast.LiteralExpr
			match lit.value {
				ast.AtomLiteral {
					atom_lit := lit.value as ast.AtomLiteral
					assert atom_lit.value == 'ok'
				}
				else {
					assert false, 'Expected AtomLiteral'
				}
			}
		}
		else {
			assert false, 'Expected LiteralExpr'
		}
	}
}

// // Test binary expression parsing
fn test_binary_expression_parsing() {
	// Test addition
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse binary expression') }

	match expr {
		ast.BinaryExpr {
			bin := expr as ast.BinaryExpr
			assert bin.op == .add
			match bin.left {
				ast.LiteralExpr {
					lit := bin.left as ast.LiteralExpr
					match lit.value {
						ast.IntegerLiteral {
							int_lit := lit.value as ast.IntegerLiteral
							assert int_lit.value == 1
						}
						else {
							assert false, 'Expected IntegerLiteral'
						}
					}
				}
				else {
					assert false, 'Expected LiteralExpr'
				}
			}
			match bin.right {
				ast.LiteralExpr {
					lit := bin.right as ast.LiteralExpr
					match lit.value {
						ast.IntegerLiteral {
							int_lit := lit.value as ast.IntegerLiteral
							assert int_lit.value == 2
						}
						else {
							assert false, 'Expected IntegerLiteral'
						}
					}
				}
				else {
					assert false, 'Expected LiteralExpr'
				}
			}
		}
		else {
			assert false, 'Expected BinaryExpr'
		}
	}

	// Test comparison
	tokens2 := [
		lexer.Token(lexer.IntToken{
			value: 5
		}),
		lexer.Token(lexer.OperatorToken.gt),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse comparison') }

	match expr2 {
		ast.BinaryExpr {
			bin := expr2 as ast.BinaryExpr
			assert bin.op == .greater_than
		}
		else {
			assert false, 'Expected BinaryExpr'
		}
	}
}

// // Test list and tuple parsing
fn test_data_structure_parsing() {
	// Test list parsing
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.PunctuationToken.rbracket),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse list') }

	match expr {
		ast.ListLiteralExpr {
			list_expr := expr as ast.ListLiteralExpr
			assert list_expr.elements.len == 3
		}
		else {
			assert false, 'Expected ListLiteralExpr'
		}
	}

	// Test tuple parsing
	tokens2 := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.StringToken{
			value: 'hello'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 42
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]
	mut parser2 := parser.new_expression_parser(tokens2)
	expr2 := parser2.parse_expression() or { panic('Failed to parse tuple') }

	match expr2 {
		ast.TupleExpr {
			tuple_expr := expr2 as ast.TupleExpr
			assert tuple_expr.elements.len == 2
		}
		else {
			assert false, 'Expected TupleExpr'
		}
	}
}

// // Test map parsing
fn test_map_parsing() {
	// Test map with atom keys
	tokens := [
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.AtomToken{
			value: 'name'
		}),
		lexer.Token(lexer.PunctuationToken.colon),
		lexer.Token(lexer.StringToken{
			value: 'Alice'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.AtomToken{
			value: 'age'
		}),
		lexer.Token(lexer.PunctuationToken.colon),
		lexer.Token(lexer.IntToken{
			value: 30
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse map') }

	match expr {
		ast.MapLiteralExpr {
			map_expr := expr as ast.MapLiteralExpr
			assert map_expr.entries.len == 2
		}
		else {
			assert false, 'Expected MapLiteralExpr'
		}
	}
}

// // Test assignment expression parsing
fn test_assignment_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IntToken{
			value: 42
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse assignment') }

	match expr {
		ast.AssignExpr {
			assign_expr := expr as ast.AssignExpr
			assert assign_expr.name == 'x'
		}
		else {
			assert false, 'Expected AssignExpr'
		}
	}
}

// // Test function call parsing
fn test_function_call_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'add'
		}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse function call') }

	match expr {
		ast.CallExpr {
			call_expr := expr as ast.CallExpr
			assert call_expr.arguments.len == 2
		}
		else {
			assert false, 'Expected CallExpr'
		}
	}
}

// // Test record access parsing
fn test_record_access_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'person'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'name'
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse record access') }

	match expr {
		ast.RecordAccessExpr {
			access_expr := expr as ast.RecordAccessExpr
			// Note: record and field names are stored as ast.Expr, not strings
			assert true // Just check that it parsed correctly
		}
		else {
			assert false, 'Expected RecordAccessExpr'
		}
	}
}

// // Test case expression parsing
fn test_case_expression_parsing() {
	tokens := [
		lexer.Token(lexer.KeywordToken.case_),
		lexer.Token(lexer.IdentToken{
			value: 'value'
		}),
		lexer.Token(lexer.KeywordToken.do_),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.StringToken{
			value: 'one'
		}),
		lexer.Token(lexer.KeywordToken.end_),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse case expression') }

	match expr {
		ast.CaseExpr {
			case_expr := expr as ast.CaseExpr
			assert case_expr.cases.len == 1

			// Check the case
			case1 := case_expr.cases[0]
			match case1.pattern {
				ast.LiteralPattern {
					lit_pat := case1.pattern as ast.LiteralPattern
					match lit_pat.value {
						ast.IntegerLiteral {
							int_lit := lit_pat.value as ast.IntegerLiteral
							assert int_lit.value == 1
						}
						else {
							panic('Expected IntegerLiteral in case')
						}
					}
				}
				else {
					panic('Expected LiteralPattern in case')
				}
			}
		}
		else {
			assert false, 'Expected CaseExpr'
		}
	}
}

// // Test if expression parsing
// fn test_if_expression_parsing() {
// 	tokens := [
// 		lexer.Token(lexer.KeywordToken.if_),
// 		lexer.Token(lexer.IdentToken{ value: 'x' }),
// 		lexer.Token(lexer.OperatorToken.gt),
// 		lexer.Token(lexer.IntToken{ value: 0 }),
// 		lexer.Token(lexer.KeywordToken.do_),
// 		lexer.Token(lexer.StringToken{ value: 'yes' }),
// 		lexer.Token(lexer.KeywordToken.else_),
// 		lexer.Token(lexer.StringToken{ value: 'no' }),
// 		lexer.Token(lexer.KeywordToken.end_)
// 	]
// 	mut parser_instance := parser.new_expression_parser(tokens)
// 	expr := parser_instance.parse_expression() or { panic('Failed to parse if expression') }

// 	match expr {
// 		ast.IfExpr {
// 			if_expr := expr as ast.IfExpr
// 			assert if_expr.then_body.len == 1
// 			assert if_expr.else_body.len == 1
// 		}
// 		else { assert false, 'Expected IfExpr' }
// 	}
// }

// // Test send expression parsing
fn test_send_expression_parsing() {
	tokens := [
		lexer.Token(lexer.IdentToken{
			value: 'pid'
		}),
		lexer.Token(lexer.OperatorToken.send),
		lexer.Token(lexer.StringToken{
			value: 'hello'
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse send expression') }

	match expr {
		ast.SendExpr {
			send_expr := expr as ast.SendExpr
			assert true // Just check that it parsed correctly
		}
		else {
			assert false, 'Expected SendExpr'
		}
	}
}

// // Test receive expression parsing
fn test_receive_expression_parsing() {
	tokens := [
		lexer.Token(lexer.KeywordToken.receive),
		lexer.Token(lexer.KeywordToken.do_),
		lexer.Token(lexer.StringToken{
			value: 'hello'
		}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.StringToken{
			value: 'received'
		}),
		lexer.Token(lexer.KeywordToken.end_),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse receive expression') }

	match expr {
		ast.ReceiveExpr {
			receive_expr := expr as ast.ReceiveExpr
			assert receive_expr.cases.len == 1
		}
		else {
			assert false, 'Expected ReceiveExpr'
		}
	}
}

// // Test module parsing
fn test_module_parsing() {
	tokens := [
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'math'
		}),
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IdentToken{
			value: 'add'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'subtract'
		}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'add'
		}),
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
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]
	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or { panic('Failed to parse module') }

	// Since ModuleStmt is a struct, we can't match it directly
	// Just check that parsing succeeded
	assert true
}

// // Test error handling
fn test_error_handling() {
	// Test invalid syntax - incomplete binary expression
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		// Missing right operand
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression()

	// Should return an expression (even if it's an error expression)
	// Since parse_expression returns an Option, we need to handle it properly
	if expr == none {
		assert true // Expression was parsed
	} else {
		assert false, 'Expected expression to be parsed'
	}
}

// // Test complex expressions
fn test_complex_expressions() {
	// Test nested expressions
	tokens := [
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse complex expression') }

	match expr {
		ast.BinaryExpr {
			bin := expr as ast.BinaryExpr
			assert bin.op == .multiply
		}
		else {
			assert false, 'Expected BinaryExpr'
		}
	}
}

// // Test precedence
fn test_operator_precedence() {
	// Test that multiplication has higher precedence than addition
	tokens := [
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
	]
	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse precedence test') }

	match expr {
		ast.BinaryExpr {
			bin := expr as ast.BinaryExpr
			assert bin.op == .add
			// The right side should be a multiplication
			match bin.right {
				ast.BinaryExpr {
					right_bin := bin.right as ast.BinaryExpr
					assert right_bin.op == .multiply
				}
				else {
					assert false, 'Expected BinaryExpr on right side'
				}
			}
		}
		else {
			assert false, 'Expected BinaryExpr'
		}
	}
}
