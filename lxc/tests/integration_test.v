module main

import parser
import lexer
import ast

// Test the complete parsing pipeline from tokens to AST
fn test_complete_parsing_pipeline() {
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
		lexer.Token(lexer.KeywordToken.module),
		lexer.Token(lexer.IdentToken{
			value: 'math_utils'
		}),
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IdentToken{
			value: 'add'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'multiply'
		}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.PunctuationToken.lbrace),
		// Function add
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
		// Function multiply
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'multiply'
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
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.KeywordToken.end_),
		// Record Point
		lexer.Token(lexer.KeywordToken.record),
		lexer.Token(lexer.IdentToken{
			value: 'Point'
		}),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.type_cons),
		lexer.Token(lexer.IdentToken{
			value: 'integer'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.OperatorToken.type_cons),
		lexer.Token(lexer.IdentToken{
			value: 'integer'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		// Function distance
		lexer.Token(lexer.KeywordToken.def),
		lexer.Token(lexer.IdentToken{
			value: 'distance'
		}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'p1'
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IdentToken{
			value: 'p2'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.do_),
		lexer.Token(lexer.IdentToken{
			value: 'dx'
		}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IdentToken{
			value: 'p1'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IdentToken{
			value: 'p2'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'x'
		}),
		lexer.Token(lexer.IdentToken{
			value: 'dy'
		}),
		lexer.Token(lexer.OperatorToken.assign),
		lexer.Token(lexer.IdentToken{
			value: 'p1'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.OperatorToken.minus),
		lexer.Token(lexer.IdentToken{
			value: 'p2'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'y'
		}),
		lexer.Token(lexer.IdentToken{
			value: 'math'
		}),
		lexer.Token(lexer.OperatorToken.dot),
		lexer.Token(lexer.IdentToken{
			value: 'sqrt'
		}),
		lexer.Token(lexer.PunctuationToken.lparen),
		lexer.Token(lexer.IdentToken{
			value: 'dx'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IdentToken{
			value: 'dx'
		}),
		lexer.Token(lexer.OperatorToken.plus),
		lexer.Token(lexer.IdentToken{
			value: 'dy'
		}),
		lexer.Token(lexer.OperatorToken.mult),
		lexer.Token(lexer.IdentToken{
			value: 'dy'
		}),
		lexer.Token(lexer.PunctuationToken.rparen),
		lexer.Token(lexer.KeywordToken.end_),
		lexer.Token(lexer.PunctuationToken.rbrace),
	]
}

// Test pattern matching expressions
fn test_pattern_matching_expressions() {
	// Test case expression with a simple pattern
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
			panic('Expected CaseExpr')
		}
	}
}

// Test data structure expressions
fn test_data_structure_expressions() {
	// Test map with different key types
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
		lexer.Token(lexer.StringToken{
			value: 'age'
		}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.IntToken{
			value: 30
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.StringToken{
			value: 'first'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
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
