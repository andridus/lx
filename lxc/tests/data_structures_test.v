module main

import frontend.parser
import frontend.lexer
import ast

fn test_tuple_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
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
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.OperatorToken.type_cons),
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.StringToken{
			value: 'name'
		}),
		lexer.Token(lexer.OperatorToken.arrow),
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
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{}),
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
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IntToken{
			value: 1
		}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 2
		}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.AtomToken{
			value: 'key'
		}),
		lexer.Token(lexer.PunctuationToken.colon),
		lexer.Token(lexer.StringToken{
			value: 'value'
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{
			value: 3
		}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{}),
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
