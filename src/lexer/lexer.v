module lexer

import token
import utils

const delimiters = [` `, `(`, `)`, `{`, `|`, `'`, `,`, `+`, `\n`]

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
		`A`...`Z` {
			lexer0.parse_aliases()!
		}
		`=` {
			lexer0.new_token(._attrb_op, '=')
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
		`(` {
			lexer0.new_token(._lpar, '(')
		}
		`)` {
			lexer0.new_token(._rpar, ')')
		}
		`[` {
			lexer0.new_token(._lsbr, '[')
		}
		`]` {
			lexer0.new_token(._rsbr, ']')
		}
		`{` {
			lexer0.new_token(._lcbr, '{')
		}
		`}` {
			lexer0.new_token(._rcbr, '}')
		}
		`,` {
			lexer0.new_token(._comma, ',')
		}
		`.` {
			lexer0.new_token(._dot, '.')
		}
		`:` {
			if lexer0.source.peek_next()! == `:` {
				lexer0.source.ignore_bytes(1)!
				lexer0.new_token(._type, '::')
			} else {
				lexer0.parse_atom()!
			}
		}
		`@` {
			lexer0.parse_attribute()!
		}
		`\"`, `'` {
			lexer0.parse_delimiter()!
		}
		else {
			if keyword, is_atom := lexer0.match_keyword() {
				if is_atom {
					return lexer0.new_token(._keyword_atom, keyword)
				} else {
					return match keyword {
						'nil' {
							lexer0.new_token(._nil, '')
						}
						'true' {
							lexer0.new_token(._true, '')
						}
						'false' {
							lexer0.new_token(._false, '')
						}
						'defmodule' {
							lexer0.new_token(._defmodule, '')
						}
						'do' {
							lexer0.new_token(._do, '')
						}
						'def' {
							lexer0.new_token(._def, '')
						}
						'end' {
							lexer0.new_token(._end, '')
						}
						else {
							if is_atom {
								lexer0.new_token(._atom, keyword)
							} else {
								lexer0.new_token(._ident, keyword)
							}
						}
					}
				}
			}
			error(lexer0.show_error_custom_error(invalid_token_message))
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
		next = lexer0.source.peek_next() or {
			data << curr
			break
		}
		data << curr
	}
	return lexer0.new_token(._comment, data.bytestr())
}

fn (mut lexer0 Lexer) parse_aliases() !token.Token {
	data := lexer0.parse_alpha()
	if data.len == 0 {
		return error(lexer0.show_error_custom_error('not is valid module'))
	}

	return lexer0.new_token(._aliases, data.bytestr())
}

fn (mut lexer0 Lexer) parse_attribute() !token.Token {
	mut curr := lexer0.source.current()

	if curr != `@` {
		return error(lexer0.show_error_custom_error('is not an attribute'))
	}
	curr = lexer0.source.get_next_byte()!
	mut data := []u8{}
	if is_alpha(curr) {
		data = lexer0.parse_alpha()
	}
	if data.len == 0 {
		return error(lexer0.show_error_custom_error('not is valid atom'))
	}
	return lexer0.new_token(._mod_attr, data.bytestr())
}

fn (mut lexer0 Lexer) parse_atom() !token.Token {
	mut curr := lexer0.source.current()
	mut data := []u8{}
	if curr in [`'`, `\"`] {
		_, data = lexer0.parse_bytes_from_delimiter()!
	} else if is_capital(curr) {
		return lexer0.parse_aliases()!
	} else if is_alpha(curr) {
		data = lexer0.parse_alpha()
	}
	if data.len == 0 {
		return error(lexer0.show_error_custom_error('not is valid atom'))
	}

	return lexer0.new_token(._atom, data.bytestr())
}

fn (mut lexer0 Lexer) parse_alpha() []u8 {
	mut byt := [lexer0.source.current()]
	for {
		c := lexer0.source.peek_next() or { break }
		if !is_alpha(c) {
			break
		}
		byt << lexer0.source.get_next_byte() or { break }
	}
	return byt
}

fn (mut lexer0 Lexer) match_keyword() ?(string, bool) {
	mut word := []u8{}
	mut curr := lexer0.source.current()
	for {
		word << curr
		pk := lexer0.source.peek_next() or { break }
		if pk == `:` {
			if lexer0.source.peek_next_length(2)! == [u8(58), 58] {
				break
			}
		}
		if delimiters.index(pk) != -1 {
			break
		}
		curr = lexer0.source.get_next_byte() or { break }
	}
	if word.len > 0 {
		if word.last() == `:` {
			_ := word.pop()

			return word.bytestr(), true
		}
	}
	return word.bytestr(), false
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
		if byt == `\\` {
			d := lexer0.source.peek_next() or { break }
			if d != delimiter {
				break
			}
			data << `\\`
			data << delimiter
			lexer0.source.ignore_bytes(1)!
			byt = lexer0.source.get_next_byte() or { break }
		}
	}
	return delimiter, data
}

fn (mut lexer0 Lexer) parse_number() !token.Token {
	mut opts := map[string]bool{}
	mut term := [lexer0.source.current()]
	term << lexer0.source.get_while_number()
	return lexer0.continue_term(mut term, ._int, mut opts)!
}

fn (mut lexer0 Lexer) continue_term(mut term []u8, kind token.Kind, mut opts map[string]bool) !token.Token {
	current := lexer0.source.current()
	return match current {
		`_` {
			lexer0.source.ignore_bytes(1)!
			term << lexer0.source.get_while_number()
			lexer0.continue_term(mut term, kind, mut opts)!
		}
		`e` {
			if kind == ._float {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, ._float_e, mut opts)!
			} else {
				error(lexer0.show_error_custom_error(invalid_token_message))
			}
		}
		`x` {
			if kind == ._int {
				term << current
				term << lexer0.source.get_while_number()
				opts['hexadecimal'] = true
				lexer0.continue_term(mut term, ._int, mut opts)!
			} else {
				error(lexer0.show_error_custom_error(invalid_token_message))
			}
		}
		`A`...`F`, `a`...`f` {
			if opts['hexadecimal'] == true && kind == ._int {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, kind, mut opts)!
			} else {
				error(lexer0.show_error_custom_error(invalid_token_message))
			}
		}
		`.` {
			if kind == ._float {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, ._string, mut opts)!
			} else {
				term << current
				term << lexer0.source.get_while_number()
				lexer0.continue_term(mut term, ._float, mut opts)!
			}
		}
		`0`...`9` {
			lexer0.new_token(kind, term.bytestr())
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

pub fn (mut lexer0 Lexer) current_position() (int, int) {
	return lexer0.source.current_pos_inline(), lexer0.source.current_line()
}

pub fn (mut lexer0 Lexer) source() []u8 {
	return lexer0.source.data()
}
