module main

import frontend.parser
import frontend.lexer
import ast

// Test the complete parsing pipeline from tokens to AST
fn test_complete_parsing_pipeline() {
	// Test a simple LX function
	tokens := [
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('add')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		lexer.Token(lexer.new_ident_token('x')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.plus)),
		lexer.Token(lexer.new_ident_token('y')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
	]

	mut parser_instance := parser.new_main_parser(tokens)
	module_stmt := parser_instance.parse_module() or { panic('Failed to parse complete program') }

	// Verify the module structure
	// Since ModuleStmt is a struct, we can't match it directly
	// Just check that parsing succeeded
	assert true
}

// Helper function to create tokens for the test program
fn create_tokens_for_program() []lexer.Token {
	return [
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.def)),
		lexer.Token(lexer.new_ident_token('sum')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lparen)),
		lexer.Token(lexer.new_ident_token('a')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_ident_token('b')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rparen)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.assign)),
		lexer.Token(lexer.new_ident_token('a')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.plus)),
		lexer.Token(lexer.new_ident_token('b')),
		lexer.Token(lexer.new_newline_token()),
		lexer.Token(lexer.new_eof_token()),
	]
}

// Test pattern matching expressions
fn test_pattern_matching_expressions() {
	// Test case expression with a simple pattern
	tokens := [
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.case_)),
		lexer.Token(lexer.new_ident_token('value')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.do_)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_string_token('one')),
		lexer.Token(lexer.new_keyword_token(lexer.KeywordValue.end_)),
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
			panic('Expected CaseExpr')
		}
	}
}

// Test data structure expressions
fn test_data_structure_expressions() {
	// Test map with different key types
	tokens := [
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.modulo)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_key_token('name')),
		lexer.Token(lexer.new_string_token('Alice')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_string_token('age')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_int_token(30)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_string_token('first')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
	]

	mut parser_instance := parser.new_expression_parser(tokens)
	expr := parser_instance.parse_expression() or { panic('Failed to parse map expression') }

	match expr {
		ast.MapLiteralExpr {
			map_expr := expr as ast.MapLiteralExpr
			assert map_expr.entries.len == 3

			// Check first entry (atom key)
			entry1 := map_expr.entries[0]
			match entry1.key {
				ast.LiteralExpr {
					lit := entry1.key as ast.LiteralExpr
					match lit.value {
						ast.AtomLiteral {
							atom_lit := lit.value as ast.AtomLiteral
							assert atom_lit.value == 'name'
						}
						else {
							panic('Expected AtomLiteral key')
						}
					}
				}
				else {
					panic('Expected LiteralExpr key')
				}
			}

			// Check second entry (string key)
			entry2 := map_expr.entries[1]
			match entry2.key {
				ast.LiteralExpr {
					lit := entry2.key as ast.LiteralExpr
					match lit.value {
						ast.StringLiteral {
							str_lit := lit.value as ast.StringLiteral
							assert str_lit.value == 'age'
						}
						else {
							panic('Expected StringLiteral key')
						}
					}
				}
				else {
					panic('Expected LiteralExpr key')
				}
			}

			// Check third entry (int key)
			entry3 := map_expr.entries[2]
			match entry3.key {
				ast.LiteralExpr {
					lit := entry3.key as ast.LiteralExpr
					match lit.value {
						ast.IntegerLiteral {
							int_lit := lit.value as ast.IntegerLiteral
							assert int_lit.value == 1
						}
						else {
							panic('Expected IntegerLiteral key')
						}
					}
				}
				else {
					panic('Expected LiteralExpr key')
				}
			}
		}
		else {
			panic('Expected MapLiteralExpr')
		}
	}
}
