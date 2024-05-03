module lexer

import token
import ast

@[heap]
pub struct Lexer {
	source string
	total  int
mut:
	line     int = 1
	pos      int
	next_pos int
	tokens   []token.Token
}

const invalid_token_message = 'Invalid Token'

fn run(source string) !Lexer {
	mut lexer0 := new(source)!
	for lexer0.next_pos != -1 {
		token0 := lexer0.parse_token() or {
			return error(lexer0.show_error_with_position(err.msg()))
		}
		lexer0.tokens << token0
	}

	return lexer0
}

pub fn new(source string) !Lexer {
	lexer0 := Lexer{
		source: source
		total: source.len
	}
	lexer0.check_has_empty_file()!
	if lexer0.source.len == 0 {
		return error(lexer0.show_error_custom_error('Empty file'))
	}
	return lexer0
}

fn (mut lexer0 Lexer) next_position() {
	pos := lexer0.pos + 1
	next_pos := pos + 1

	if next_pos < lexer0.total {
		lexer0.next_pos = next_pos
	} else {
		lexer0.next_pos = -1
	}
	lexer0.pos = pos
}
pub fn (mut lexer0 Lexer) read_next_token() !token.Token {
	return lexer0.parse_token()!
}
fn (mut lexer0 Lexer) parse_token() !token.Token {
	if lexer0.next_pos == -1 {
		return token.generate_eof()
	}
	current := lexer0.source[lexer0.pos]

	return match current {
		` `, `\n` {
			lexer0.next_position()
			lexer0.parse_token()!
		}
		`0`...`9` {
			lexer0.parse_number()!
		}
		`+` {
			lexer0.next_position()
			lexer0.new_token(.op_plus, '+')
		}
		`-` {
			lexer0.next_position()
			lexer0.new_token(.op_minus, '-')
		}
		`*` {
			lexer0.next_position()
			lexer0.new_token(.op_mult, '*')
		}
		`/` {
			lexer0.next_position()
			lexer0.new_token(.op_div, '/')
		}
		else {
			error(lexer.invalid_token_message)
		}
	}
}

fn (mut lexer0 Lexer) is_breaking_term() bool {
	if lexer0.pos < lexer0.total {
		a := lexer0.source[lexer0.pos]
		if a == ` ` || a == `\n` || a == `\t` {
			return true
		} else {
			return false
		}
	}
	return true
}

fn (lexer0 Lexer) show_error_with_position(msg string) string {
	code := [lexer0.source[lexer0.pos]].bytestr()
	return utils_show('ERROR: ${msg} `${code}` on source[${lexer0.line}:${lexer0.pos + 1}]')
}

fn (lexer0 Lexer) show_error_custom_error(str string) string {
	return utils_show('ERROR: ${str}')
}

fn (lexer0 Lexer) check_has_empty_file() !bool {
	source := lexer0.source
	mut show_error := false
	if source.len == 0 {
		show_error = true
	} else if source.trim_space().len == 0 {
		show_error = true
	} else {
	}
	if show_error {
		return error(lexer0.show_error_custom_error('Empty file'))
	} else {
		return false
	}
}

fn (mut lexer0 Lexer) parse_number() !token.Token {
	mut dg := lexer0.source[lexer0.pos]
	mut kind := token.Kind.lit_int
	mut num := [dg]
	lexer0.next_position()
	mut next := true
	for lexer0.pos < lexer0.total && next {
		dg = lexer0.source[lexer0.pos]
		if utils_is_digit(dg) {
			num << dg
			lexer0.next_position()
		} else if lexer0.verify_char_inside(`_`, dg) { // ignore _ inside number
			dg0 := lexer0.source[lexer0.pos + 1]
			num << dg0
			lexer0.next_position()
			lexer0.next_position()
		} else if lexer0.verify_char_inside(`.`, dg) { // verify if the number is a float
			dg0 := lexer0.source[lexer0.pos + 1]
			kind = token.Kind.lit_float
			num << dg
			num << dg0
			lexer0.next_position()
			lexer0.next_position()
		} else if lexer0.is_breaking_term() { // verify if reach end of number
			break
		} else {
			return error(lexer.invalid_token_message)
		}
	}
	return lexer0.new_token(kind, num.bytestr())
}

fn (lexer0 Lexer) verify_char_inside(c rune, dg rune) bool {
	if lexer0.pos + 1 < lexer0.total {
		dg0 := lexer0.source[lexer0.pos + 1]
		return utils_has_char_before_number(c, dg, dg0)
	}
	return false
}

fn (lexer0 Lexer) new_token(kind token.Kind, value string) token.Token {
	return token.Token{
		kind: kind
		value: value
	}
}
fn (lexer0 Lexer) to_meta() ast.Meta {
	return ast.new_meta(0,0,0)
}
