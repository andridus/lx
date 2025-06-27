module main

import parser
import lexer
import ast

fn test_tuple_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IntToken{value: 1}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 2}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 3}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.TupleExpr
	tuple_expr := expr as ast.TupleExpr
	assert tuple_expr.elements.len == 3

	// Verificar elementos
	assert tuple_expr.elements[0] is ast.LiteralExpr
	first := tuple_expr.elements[0] as ast.LiteralExpr
	assert first.value is ast.IntegerLiteral
	first_int := first.value as ast.IntegerLiteral
	assert first_int.value == 1
}

fn test_empty_tuple_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.TupleExpr
	tuple_expr := expr as ast.TupleExpr
	assert tuple_expr.elements.len == 0
}

fn test_list_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IntToken{value: 1}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 2}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 3}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.ListLiteralExpr
	list_expr := expr as ast.ListLiteralExpr
	assert list_expr.elements.len == 3

	// Verificar elementos
	assert list_expr.elements[0] is ast.LiteralExpr
	first := list_expr.elements[0] as ast.LiteralExpr
	assert first.value is ast.IntegerLiteral
	first_int := first.value as ast.IntegerLiteral
	assert first_int.value == 1
}

fn test_empty_list_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.ListLiteralExpr
	list_expr := expr as ast.ListLiteralExpr
	assert list_expr.elements.len == 0
}

fn test_cons_parsing() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IdentToken{value: 'head'}),
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.IdentToken{value: 'tail'}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.ListConsExpr
	cons_expr := expr as ast.ListConsExpr

	// Verificar head
	assert cons_expr.head is ast.VariableExpr
	head := cons_expr.head as ast.VariableExpr
	assert head.name == 'head'

	// Verificar tail
	assert cons_expr.tail is ast.VariableExpr
	tail := cons_expr.tail as ast.VariableExpr
	assert tail.name == 'tail'
}

fn test_map_parsing_with_atom_keys() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.AtomToken{value: 'name'}),
		lexer.Token(lexer.PunctuationToken.colon),
		lexer.Token(lexer.StringToken{value: 'Alice'}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.AtomToken{value: 'age'}),
		lexer.Token(lexer.PunctuationToken.colon),
		lexer.Token(lexer.IntToken{value: 30}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.MapLiteralExpr
	map_expr := expr as ast.MapLiteralExpr
	assert map_expr.entries.len == 2

	// Verificar primeira entrada
	first_entry := map_expr.entries[0]
	assert first_entry.key is ast.LiteralExpr
	key1 := first_entry.key as ast.LiteralExpr
	assert key1.value is ast.AtomLiteral
	key1_atom := key1.value as ast.AtomLiteral
	assert key1_atom.value == 'name'

	assert first_entry.value is ast.LiteralExpr
	value1 := first_entry.value as ast.LiteralExpr
	assert value1.value is ast.StringLiteral
	value1_str := value1.value as ast.StringLiteral
	assert value1_str.value == 'Alice'
}

fn test_map_parsing_with_string_keys() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.OperatorToken.record_update),
		lexer.Token(lexer.StringToken{value: 'database_url'}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.StringToken{value: 'localhost'}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.StringToken{value: 'port'}),
		lexer.Token(lexer.OperatorToken.arrow),
		lexer.Token(lexer.IntToken{value: 5432}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.MapLiteralExpr
	map_expr := expr as ast.MapLiteralExpr
	assert map_expr.entries.len == 2

	// Verificar primeira entrada
	first_entry := map_expr.entries[0]
	assert first_entry.key is ast.LiteralExpr
	key1 := first_entry.key as ast.LiteralExpr
	assert key1.value is ast.StringLiteral
	key1_str := key1.value as ast.StringLiteral
	assert key1_str.value == 'database_url'

	assert first_entry.value is ast.LiteralExpr
	value1 := first_entry.value as ast.LiteralExpr
	assert value1.value is ast.StringLiteral
	value1_str := value1.value as ast.StringLiteral
	assert value1_str.value == 'localhost'
}

fn test_nested_structures() {
	tokens := [
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IntToken{value: 1}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.PunctuationToken.lbracket),
		lexer.Token(lexer.IntToken{value: 2}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 3}),
		lexer.Token(lexer.PunctuationToken.rbracket),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.PunctuationToken.lbrace),
		lexer.Token(lexer.IntToken{value: 4}),
		lexer.Token(lexer.PunctuationToken.comma),
		lexer.Token(lexer.IntToken{value: 5}),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.PunctuationToken.rbrace),
		lexer.Token(lexer.EOFToken{})
	]
	mut parser0 := parser.new_parser(tokens)

	expr := parser0.parse_expression()
	assert expr is ast.TupleExpr
	tuple_expr := expr as ast.TupleExpr
	assert tuple_expr.elements.len == 3

	// Primeiro elemento: 1
	assert tuple_expr.elements[0] is ast.LiteralExpr
	first := tuple_expr.elements[0] as ast.LiteralExpr
	assert first.value is ast.IntegerLiteral
	first_int := first.value as ast.IntegerLiteral
	assert first_int.value == 1

	// Segundo elemento: [2, 3]
	assert tuple_expr.elements[1] is ast.ListLiteralExpr
	second := tuple_expr.elements[1] as ast.ListLiteralExpr
	assert second.elements.len == 2

	// Terceiro elemento: {4, 5}
	assert tuple_expr.elements[2] is ast.TupleExpr
	third := tuple_expr.elements[2] as ast.TupleExpr
	assert third.elements.len == 2
}