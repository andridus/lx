module main

import frontend.parser
import frontend.lexer
import ast

fn test_tuple_parsing() {
	tokens := [
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(3)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse tuple') }
	match expr {
		ast.TupleExpr {
			tuple_expr := expr as ast.TupleExpr
			assert tuple_expr.elements.len == 3
		}
		else {
			panic('Expected TupleExpr')
		}
	}
}

fn test_empty_tuple_parsing() {
	tokens := [
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse empty tuple') }
	match expr {
		ast.TupleExpr {
			tuple_expr := expr as ast.TupleExpr
			assert tuple_expr.elements.len == 0
		}
		else {
			panic('Expected TupleExpr')
		}
	}
}

fn test_list_parsing() {
	tokens := [
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbracket)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(3)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbracket)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse list') }
	match expr {
		ast.ListLiteralExpr {
			list_expr := expr as ast.ListLiteralExpr
			assert list_expr.elements.len == 3
		}
		else {
			panic('Expected ListLiteralExpr')
		}
	}
}

fn test_empty_list_parsing() {
	tokens := [
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbracket)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbracket)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse empty list') }
	match expr {
		ast.ListLiteralExpr {
			list_expr := expr as ast.ListLiteralExpr
			assert list_expr.elements.len == 0
		}
		else {
			panic('Expected ListLiteralExpr')
		}
	}
}

fn test_list_cons_parsing() {
	tokens := [
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.type_cons)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbracket)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(3)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbracket)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse list cons') }
	match expr {
		ast.ListConsExpr {
			cons_expr := expr as ast.ListConsExpr
			// Verificar head
			match cons_expr.head {
				ast.LiteralExpr {
					lit := cons_expr.head as ast.LiteralExpr
					match lit.value {
						ast.IntegerLiteral {
							int_lit := lit.value as ast.IntegerLiteral
							assert int_lit.value == 1
						}
						else {
							panic('Expected IntegerLiteral head')
						}
					}
				}
				else {
					panic('Expected LiteralExpr head')
				}
			}

			// Verificar tail
			match cons_expr.tail {
				ast.ListLiteralExpr {
					tail_list := cons_expr.tail as ast.ListLiteralExpr
					assert tail_list.elements.len == 2
				}
				else {
					panic('Expected ListLiteralExpr tail')
				}
			}
		}
		else {
			panic('Expected ListConsExpr')
		}
	}
}

fn test_map_parsing() {
	tokens := [
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.record_update)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_atom_token('name')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.colon)),
		lexer.Token(lexer.new_string_token('Alice')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_atom_token('age')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.colon)),
		lexer.Token(lexer.new_int_token(30)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse map') }
	match expr {
		ast.MapLiteralExpr {
			map_expr := expr as ast.MapLiteralExpr
			assert map_expr.entries.len == 2
		}
		else {
			panic('Expected MapLiteralExpr')
		}
	}
}

fn test_map_with_string_keys_parsing() {
	tokens := [
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.record_update)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_string_token('name')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_string_token('Alice')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_string_token('age')),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.arrow)),
		lexer.Token(lexer.new_int_token(30)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse map with string keys') }
	match expr {
		ast.MapLiteralExpr {
			map_expr := expr as ast.MapLiteralExpr
			assert map_expr.entries.len == 2
		}
		else {
			panic('Expected MapLiteralExpr')
		}
	}
}

fn test_nested_data_structures() {
	tokens := [
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbracket)),
		lexer.Token(lexer.new_int_token(1)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(2)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbracket)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_operator_token(lexer.OperatorValue.record_update)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.lbrace)),
		lexer.Token(lexer.new_atom_token('key')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.colon)),
		lexer.Token(lexer.new_string_token('value')),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.comma)),
		lexer.Token(lexer.new_int_token(3)),
		lexer.Token(lexer.new_punctuation_token(lexer.PunctuationValue.rbrace)),
		lexer.Token(lexer.new_eof_token()),
	]

	mut parser0 := parser.new_main_parser(tokens)

	expr := parser0.parse_expression() or { panic('Failed to parse nested data structures') }
	match expr {
		ast.TupleExpr {
			tuple_expr := expr as ast.TupleExpr
			assert tuple_expr.elements.len == 3
		}
		else {
			panic('Expected TupleExpr')
		}
	}
}
