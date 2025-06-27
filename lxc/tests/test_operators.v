module main

import lexer

fn test_assignment_operators() {
	input := '= <-'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.assign

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.pattern_match
}

fn test_arithmetic_operators() {
	input := '+ - * /'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.plus

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.minus

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.mult

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.div
}

fn test_comparison_operators() {
	input := '== != < > <= >='
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.eq

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.neq

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.lt

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.gt

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token5 := token5 as lexer.OperatorToken
	assert operator_token5 == lexer.OperatorToken.leq

	token6 := lexer0.next_token()
	assert token6 is lexer.OperatorToken
	operator_token6 := token6 as lexer.OperatorToken
	assert operator_token6 == lexer.OperatorToken.geq
}

fn test_logical_operators() {
	input := 'and or not andalso orelse'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.and_

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.or_

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.not_

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.andalso

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token5 := token5 as lexer.OperatorToken
	assert operator_token5 == lexer.OperatorToken.orelse
}

fn test_special_operators() {
	input := '! :: . ++ |'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.send

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.type_cons

	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token3 := token3 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.dot

	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token4 := token4 as lexer.OperatorToken
	assert operator_token4 == lexer.OperatorToken.concat

	token5 := lexer0.next_token()
	assert token5 is lexer.OperatorToken
	operator_token5 := token5 as lexer.OperatorToken
	assert operator_token5 == lexer.OperatorToken.record_update
}

fn test_arrow_operators() {
	input := '-> <='
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	token1 := lexer0.next_token()
	assert token1 is lexer.OperatorToken
	operator_token1 := token1 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.arrow

	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token2 := token2 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.with_bind
}

fn test_operator_precedence() {
	// Test operator precedence
	assert lexer.get_operator_precedence(lexer.OperatorToken.send) == 1
	assert lexer.get_operator_precedence(lexer.OperatorToken.type_cons) == 2
	assert lexer.get_operator_precedence(lexer.OperatorToken.concat) == 3
	assert lexer.get_operator_precedence(lexer.OperatorToken.plus) == 4
	assert lexer.get_operator_precedence(lexer.OperatorToken.minus) == 4
	assert lexer.get_operator_precedence(lexer.OperatorToken.mult) == 5
	assert lexer.get_operator_precedence(lexer.OperatorToken.div) == 5
	assert lexer.get_operator_precedence(lexer.OperatorToken.eq) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.neq) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.lt) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.gt) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.leq) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.geq) == 6
	assert lexer.get_operator_precedence(lexer.OperatorToken.and_) == 7
	assert lexer.get_operator_precedence(lexer.OperatorToken.or_) == 7
	assert lexer.get_operator_precedence(lexer.OperatorToken.andalso) == 7
	assert lexer.get_operator_precedence(lexer.OperatorToken.orelse) == 7
	assert lexer.get_operator_precedence(lexer.OperatorToken.not_) == 8
	assert lexer.get_operator_precedence(lexer.OperatorToken.assign) == 9
	assert lexer.get_operator_precedence(lexer.OperatorToken.pattern_match) == 9
	assert lexer.get_operator_precedence(lexer.OperatorToken.with_bind) == 9
}

fn test_operator_associativity() {
	// Test left associative operators
	assert lexer.is_left_associative(lexer.OperatorToken.plus) == true
	assert lexer.is_left_associative(lexer.OperatorToken.minus) == true
	assert lexer.is_left_associative(lexer.OperatorToken.mult) == true
	assert lexer.is_left_associative(lexer.OperatorToken.div) == true
	assert lexer.is_left_associative(lexer.OperatorToken.concat) == true
	assert lexer.is_left_associative(lexer.OperatorToken.and_) == true
	assert lexer.is_left_associative(lexer.OperatorToken.or_) == true
	assert lexer.is_left_associative(lexer.OperatorToken.andalso) == true
	assert lexer.is_left_associative(lexer.OperatorToken.orelse) == true

	// Test right associative operators
	assert lexer.is_right_associative(lexer.OperatorToken.assign) == true
	assert lexer.is_right_associative(lexer.OperatorToken.pattern_match) == true
	assert lexer.is_right_associative(lexer.OperatorToken.with_bind) == true
	assert lexer.is_right_associative(lexer.OperatorToken.send) == true
	assert lexer.is_right_associative(lexer.OperatorToken.type_cons) == true

	// Test non-associative operators
	assert lexer.is_left_associative(lexer.OperatorToken.eq) == false
	assert lexer.is_right_associative(lexer.OperatorToken.eq) == false
}

fn test_operator_recognition() {
	// Test all operators
	operators := [
		'=',
		'<-',
		'<=',
		'->',
		'!',
		'::',
		'.',
		'++',
		'|',
		'+',
		'-',
		'*',
		'/',
		'==',
		'!=',
		'<',
		'>',
		'<=',
		'>=',
		'and',
		'or',
		'not',
		'andalso',
		'orelse',
	]

	for op in operators {
		assert lexer.is_operator(op) == true
		token := lexer.get_operator_token(op) or { return }
		assert token != lexer.OperatorToken.assign // Just check it's not empty
	}

	// Test non-operators
	non_operators := ['x', 'count', 'hello', 'world', 'test123']
	for non_operator in non_operators {
		assert lexer.is_operator(non_operator) == false
	}
}

fn test_operator_with_operands() {
	input := 'x = y + z * 2'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// =
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token1 := token2 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.assign

	// y
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'y'

	// +
	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token2 := token4 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.plus

	// z
	token5 := lexer0.next_token()
	assert token5 is lexer.IdentToken
	ident_token3 := token5 as lexer.IdentToken
	assert ident_token3.value == 'z'

	// *
	token6 := lexer0.next_token()
	assert token6 is lexer.OperatorToken
	operator_token3 := token6 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.mult

	// 2
	token7 := lexer0.next_token()
	assert token7 is lexer.IntToken
	int_token := token7 as lexer.IntToken
	assert int_token.value == 2
}

fn test_comparison_expression() {
	input := 'x == y and z != 0'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// ==
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token1 := token2 as lexer.OperatorToken
	assert operator_token1 == lexer.OperatorToken.eq

	// y
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'y'

	// and
	token4 := lexer0.next_token()
	assert token4 is lexer.OperatorToken
	operator_token2 := token4 as lexer.OperatorToken
	assert operator_token2 == lexer.OperatorToken.and_

	// z
	token5 := lexer0.next_token()
	assert token5 is lexer.IdentToken
	ident_token3 := token5 as lexer.IdentToken
	assert ident_token3.value == 'z'

	// !=
	token6 := lexer0.next_token()
	assert token6 is lexer.OperatorToken
	operator_token3 := token6 as lexer.OperatorToken
	assert operator_token3 == lexer.OperatorToken.neq

	// 0
	token7 := lexer0.next_token()
	assert token7 is lexer.IntToken
	int_token := token7 as lexer.IntToken
	assert int_token.value == 0
}

fn test_pattern_matching_expression() {
	input := 'x <- y'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// <-
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.pattern_match

	// y
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'y'
}

fn test_with_expression() {
	input := 'x <= y'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// <=
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.with_bind

	// y
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'y'
}

fn test_message_sending() {
	input := 'pid ! message'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// pid
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'pid'

	// !
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.send

	// message
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'message'
}

fn test_type_annotation() {
	input := 'x :: integer'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// x
	token1 := lexer0.next_token()
	assert token1 is lexer.IdentToken
	ident_token1 := token1 as lexer.IdentToken
	assert ident_token1.value == 'x'

	// ::
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.type_cons

	// integer
	token3 := lexer0.next_token()
	assert token3 is lexer.IdentToken
	ident_token2 := token3 as lexer.IdentToken
	assert ident_token2.value == 'integer'
}

fn test_string_concatenation() {
	input := '"hello" ++ "world"'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// "hello"
	token1 := lexer0.next_token()
	assert token1 is lexer.StringToken
	string_token1 := token1 as lexer.StringToken
	assert string_token1.value == 'hello'

	// ++
	token2 := lexer0.next_token()
	assert token2 is lexer.OperatorToken
	operator_token := token2 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.concat

	// "world"
	token3 := lexer0.next_token()
	assert token3 is lexer.StringToken
	string_token2 := token3 as lexer.StringToken
	assert string_token2.value == 'world'
}

fn test_record_update() {
	input := '{record | field: value}'
	mut lexer0 := lexer.new_lexer(input, 'test.lx')

	// {
	token1 := lexer0.next_token()
	assert token1 is lexer.PunctuationToken
	punct_token1 := token1 as lexer.PunctuationToken
	assert punct_token1 == lexer.PunctuationToken.lbrace

	// record
	token2 := lexer0.next_token()
	assert token2 is lexer.IdentToken
	ident_token := token2 as lexer.IdentToken
	assert ident_token.value == 'record'

	// |
	token3 := lexer0.next_token()
	assert token3 is lexer.OperatorToken
	operator_token := token3 as lexer.OperatorToken
	assert operator_token == lexer.OperatorToken.record_update

	// field
	token4 := lexer0.next_token()
	assert token4 is lexer.IdentToken
	ident_token2 := token4 as lexer.IdentToken
	assert ident_token2.value == 'field'

	// :
	token5 := lexer0.next_token()
	assert token5 is lexer.PunctuationToken
	punct_token2 := token5 as lexer.PunctuationToken
	assert punct_token2 == lexer.PunctuationToken.colon

	// value
	token6 := lexer0.next_token()
	assert token6 is lexer.IdentToken
	ident_token3 := token6 as lexer.IdentToken
	assert ident_token3.value == 'value'

	// }
	token7 := lexer0.next_token()
	assert token7 is lexer.PunctuationToken
	punct_token3 := token7 as lexer.PunctuationToken
	assert punct_token3 == lexer.PunctuationToken.rbrace
}
