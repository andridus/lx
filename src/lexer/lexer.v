module lexer

import token
import ast
import utils

const delimiters = [` `, `(`, `{`, `|`, `'`]

@[heap]
pub struct Lexer {
	keywords [][]u8
mut:
	source utils.DataBytes
	tokens []token.Token
}

const invalid_token_message = 'Invalid Token'

pub fn Lexer.init(source []u8) !Lexer {
	mut keywords := token.keywords.clone()
	keywords.sort()
	keywords_binary := keywords.map(it.bytes())
	mut l := Lexer{
		keywords: keywords_binary
		source:   utils.DataBytes.init(source)
	}
	if l.source.is_empty_file() {
		return error(l.show_error_custom_error('Empty file'))
	}
	return l
}

pub fn (mut l Lexer) eof() bool {
	return l.source.eof()
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
		` ` {
			lexer0.parse_token()!
		}
		`\n` {
			lexer0.new_token(.newline, '')
		}
		`#` {
			lexer0.parse_inline_comment()!
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
		`:` {
			lexer0.parse_atom()!
		}
		`\"`, `'` {
			lexer0.parse_delimiter()!
		}
		else {
			if keyword := lexer0.match_keyword() {
				return match keyword {
					'true' {
						lexer0.new_token(._true, '')
					}
					'false' {
						lexer0.new_token(._false, '')
					}
					else {
						error(lexer0.show_error_custom_error('undefined matched keyword `${keyword}`'))
					}
				}
			}
			error(lexer0.show_error_custom_error(lexer.invalid_token_message))
		}
	}
}

fn (mut lexer0 Lexer) parse_inline_comment() !token.Token {
	mut curr := lexer0.source.current()
	mut next := lexer0.source.peek_next() or { return lexer0.new_token(._comment, '') }
	mut data := []u8{}
	if curr != `#` {
		return error(lexer0.show_error_custom_error('is not a comment'))
	}
	for next != 10 {
		curr = lexer0.source.get_next_byte() or { break }
		next = lexer0.source.peek_next() or { break }
		data << curr
	}
	return lexer0.new_token(._comment, data.bytestr())
}

fn (mut lexer0 Lexer) parse_atom() !token.Token {
	mut curr := lexer0.source.current()
	if curr != `:` {
		return error(lexer0.show_error_custom_error('is not an atom'))
	}
	curr = lexer0.source.get_next_byte()!
	mut data := []u8{}
	if curr in [`'`, `\"`] {
		_, data = lexer0.parse_bytes_from_delimiter()!
	} else if is_alpha(curr) {
		data = lexer0.parse_alpha()!
	}
	if data.len == 0 {
		return error(lexer0.show_error_custom_error('not is valid atom'))
	}

	return lexer0.new_token(._atom, data.bytestr())
}

fn (mut lexer0 Lexer) parse_alpha() ![]u8 {
	mut byt := [lexer0.source.current()]
	for is_alpha(lexer0.source.peek_next()!) {
		byt << lexer0.source.get_next_byte()!
	}
	return byt
}

fn (mut lexer0 Lexer) match_keyword() ?string {
	mut word := []u8{}
	mut curr := lexer0.source.current()
	mut tmp := lexer0.keywords.clone()
	for {
		word << curr
		keyword0 := tmp.filter(it.len > word.len && it[word.len - 1] == curr)
		if keyword0.len == 1 {
			mut slice_keyword0 := keyword0[0][(word.len - 1)..]
			if lexer0.source.peek_expect_match(slice_keyword0) {
				slice_keyword0.drop(1)
				word << slice_keyword0
				lexer0.source.ignore_bytes(slice_keyword0.len) or { break }
				return word.bytestr()
			}
		} else if keyword0.len == 0 {
			return none
		} else {
			curr = lexer0.source.get_next_byte() or { break }
			if lexer.delimiters.index(curr) != -1 {
				break
			}
		}
	}
	return none
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

fn (mut lexer0 Lexer) parse_delimiter() !token.Token {
	delimiter, data := lexer0.parse_bytes_from_delimiter()!
	return match delimiter {
		`\"` {
			lexer0.new_token(._string, data.bytestr())
		}
		`'` {
			lexer0.new_token(._charlist, data.bytestr())
		}
		else {
			error(lexer0.show_error_custom_error('invalid delimiter'))
		}
	}
}

fn (mut lexer0 Lexer) parse_bytes_from_delimiter() !(rune, []u8) {
	delimiter := lexer0.source.current()
	mut data := []u8{}
	mut byt := lexer0.source.get_next_byte()!
	for byt != delimiter {
		data << byt
		byt = lexer0.source.get_next_byte()!
		if byt == `\\` && lexer0.source.peek_next()! == delimiter {
			data << `\\`
			data << delimiter
			lexer0.source.ignore_bytes(1)!
			byt = lexer0.source.get_next_byte()!
		}
	}
	return delimiter, data
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
		`e` {
			if kind == ._float {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, ._float_e)!
			} else {
				error(lexer0.show_error_custom_error(lexer.invalid_token_message))
			}
		}
		`.` {
			if kind == ._float {
				term << current
				term << lexer0.source.get_while_number()
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
