module lexer

import token

fn test_expect_fail_invalid_token() {
	source := 'INVALID'
	expected := 'ERROR: Invalid Token `I` on source[1:1]'
	mut received := ''
	run(source) or { received = err.msg() }
	assert expected == received
}

fn test_expect_fail_empty_file() {
	source0 := ''
	expected0 := 'ERROR: Empty file'
	mut received0 := ''
	run(source0) or { received0 = err.msg() }
	assert expected0 == received0

	source1 := '   '
	expected1 := 'ERROR: Empty file'
	mut received1 := ''
	run(source1) or { received1 = err.msg() }
	assert expected1 == received1
}

fn test_expect_parse_integer() {
	source := '1'
	expected := Lexer{
		source: source
		total: source.len
		pos: 1
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '1'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_parse_bigger_integer() {
	source := '145127'
	expected := Lexer{
		source: source
		total: source.len
		pos: 6
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '145127'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_parse_bigger_integer_with_underscore() {
	source := '14_5127'
	expected := Lexer{
		source: source
		total: source.len
		pos: 7
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '145127'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_parse_bigger_integer_with_underscore_and_with_space_after() {
	source := '14_5127 '
	expected := Lexer{
		source: source
		total: source.len
		pos: 7
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '145127'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_parse_two_integers() {
	source := '14\n5_12_7'
	expected := Lexer{
		source: source
		total: source.len
		pos: 9
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '14'
			},
			token.Token{
				kind: .lit_int
				value: '5127'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_error_when_parse_bigger_integer_with_underscore() {
	source := '14_5127_'
	expected := 'ERROR: Invalid Token `_` on source[1:8]'
	mut received0 := ''
	run(source) or { received0 = err.msg() }
	assert expected == received0
}

fn test_expect_parse_float() {
	source := '1.0'
	expected := Lexer{
		source: source
		total: source.len
		pos: 3
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_float
				value: '1.0'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_parse_float_with_underscore() {
	source := '1.0_0'
	expected := Lexer{
		source: source
		total: source.len
		pos: 5
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_float
				value: '1.00'
			},
		]
	}
	assert expected == run(source)!
}

fn test_expect_float_and_integer() {
	source := '1\n1.0'
	expected := Lexer{
		source: source
		total: source.len
		pos: 5
		next_pos: -1
		tokens: [
			token.Token{
				kind: .lit_int
				value: '1'
			},
			token.Token{
				kind: .lit_float
				value: '1.0'
			},
		]
	}
	assert expected == run(source)!
}
