module lexer

import token
import ast
import utils

@[heap]
pub struct Lexer {
mut:
	source utils.DataBytes
	tokens []token.Token
}

const invalid_token_message = 'Invalid Token'

pub fn Lexer.init(source []u8) !Lexer {
	mut l := Lexer{
		source: utils.DataBytes.init(source)
	}
	if l.source.is_empty_file() {
		return error(l.show_error_custom_error('Empty file'))
	}
	return l
}

pub fn (mut lexer0 Lexer) read_next_token() !token.Token {
	return lexer0.parse_token()!
}

fn (mut lexer0 Lexer) parse_token() !token.Token {
	if lexer0.source.eof() {
		return token.generate_eof()
	}
	current := lexer0.source.get_next_byte()!
	return match current {
		` `, `\n` {
			lexer0.parse_token()!
		}
		`0`...`9` {
			lexer0.parse_number()!
		}
		`+` {
			lexer0.new_token(._add_op, '+')
		}
		`-` {
			lexer0.new_token(._add_op, '-')
		}
		`*` {
			lexer0.new_token(._mult_op, '*')
		}
		`/` {
			lexer0.new_token(._mult_op, '/')
		}
		else {
			error(lexer0.show_error_custom_error(lexer.invalid_token_message))
		}
	}
}

fn (mut lexer0 Lexer) match_keyword() {
}

fn (mut lexer0 Lexer) is_breaking_term() bool {
	if lexer0.source.eof() {
		return true
	}
	a := lexer0.source.current()
	if a == ` ` || a == `\n` || a == `\t` {
		return true
	} else {
		return false
	}
}

pub fn (mut lexer0 Lexer) show_error_with_position(msg string) string {
	code := [lexer0.source.current()].bytestr()
	return utils_show('ERROR: ${msg} `${code}` on source[${lexer0.source.current_line()}:${lexer0.source.current_pos()}]')
}

pub fn (mut lexer0 Lexer) show_error_custom_error(str string) string {
	return utils_show('ERROR: ${str}')
}

fn (mut lexer0 Lexer) parse_number() !token.Token {
	mut term := [lexer0.source.current()]
	term << lexer0.source.get_while_number()
	return lexer0.continue_term(mut term, ._int)!
}

fn (mut lexer0 Lexer) continue_term(mut term []u8, kind token.Kind) !token.Token {
	if lexer0.source.eof() {
		return lexer0.new_token(kind, term.bytestr())
	}
	current := lexer0.source.current()
	return match current {
		`_` {
			lexer0.source.ignore_bytes(1)!
			term << lexer0.source.get_while_number()
			lexer0.continue_term(mut term, kind)!
		}
		`.` {
			if kind == ._float {
				term << current
				term << lexer0.source.get_while_number()
				println(term)
				lexer0.continue_term(mut term, ._string)!
			} else {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, ._float)!
			}
		}
		else {
			lexer0.source.backwards_bytes(1)
			lexer0.new_token(kind, term.bytestr())
		}
	}
}

fn (lexer0 Lexer) new_token(kind token.Kind, value string) token.Token {
	return token.Token{
		kind:  kind
		value: value
	}
}

fn (lexer0 Lexer) to_meta() ast.Meta {
	return ast.new_meta(0, 0, 0)
}
