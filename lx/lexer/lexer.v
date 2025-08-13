module lexer

import ast

pub struct Lexer {
mut:
	input    string
	position int
	line     int
	column   int
	file     string
}

pub fn new_lexer(input string, file string) Lexer {
	return Lexer{
		input:  input
		file:   file
		line:   1
		column: 1
	}
}

pub fn (mut l Lexer) next_token() Token {
	l.skip_whitespace()

	if l.position >= l.input.len {
		return l.make_token(.eof, '')
	}

	ch := l.input[l.position]

	return match ch {
		`(` { l.advance_and_return(.lparen, '(') }
		`)` { l.advance_and_return(.rparen, ')') }
		`;` { l.advance_and_return(.semicolon, ';') }
		`,` { l.advance_and_return(.comma, ',') }
		`[` { l.advance_and_return(.lbracket, '[') }
		`]` { l.advance_and_return(.rbracket, ']') }
		`{` { l.advance_and_return(.lbrace, '{') }
		`}` { l.advance_and_return(.rbrace, '}') }
		`%` { l.advance_and_return(.percent, '%') }
		`.` { l.advance_and_return(.dot, '.') }
		`:` { l.read_colon_or_double_colon() }
		`-` { l.read_arrow_or_minus() }
		`<` { l.read_left_angle_operators() }
		`>` { l.read_right_angle_operators() }
		`!` { l.read_exclamation_operators() }
		`#` { l.advance_and_return(.hash, '#') }
		`@` { l.advance_and_return(.at_sign, '@') }
		`/` { l.advance_and_return(.slash, '/') }
		`"` { l.read_string() }
		`'` { l.read_charlist() }
		`0`...`9` { l.read_number() }
		`=` { l.read_equals_operators() }
		`+`, `*`, `&`, `^`, `|`, `$`, `a`...`z`, `A`...`Z`, `_` { l.read_identifier() }
		`\n` { l.advance_and_return(.newline, '\n') }
		else { l.make_error('Unexpected character: ${ch.ascii_str()}') }
	}
}

fn (mut l Lexer) skip_whitespace() {
	for l.position < l.input.len {
		ch := l.input[l.position]
		if ch == ` ` || ch == `\t` || ch == `\r` {
			l.advance()
		} else if ch == `#` {
			for l.position < l.input.len && l.input[l.position] != `\n` {
				l.advance()
			}
		} else {
			break
		}
	}
}

fn (mut l Lexer) advance() {
	if l.position < l.input.len && l.input[l.position] == `\n` {
		l.line++
		l.column = 1
	} else {
		l.column++
	}
	l.position++
}

fn (mut l Lexer) advance_and_return(token_type TokenType, value string) Token {
	pos := l.current_position()
	l.advance()
	return l.make_token_at(token_type, value, pos)
}

fn (mut l Lexer) current_position() ast.Position {
	return ast.new_position(l.line, l.column, l.file)
}

fn (mut l Lexer) make_token(token_type TokenType, value string) Token {
	return new_token(token_type, value, l.current_position())
}

fn (mut l Lexer) make_token_at(token_type TokenType, value string, pos ast.Position) Token {
	return new_token(token_type, value, pos)
}

fn (mut l Lexer) make_error(msg string) Token {
	return l.make_token(.error, msg)
}

fn (mut l Lexer) read_string() Token {
	start_pos := l.current_position()
	l.advance()

	mut value := ''

	for l.position < l.input.len && l.input[l.position] != `"` {
		ch := l.input[l.position]
		if ch == `\\` {
			l.advance()
			if l.position >= l.input.len {
				return l.make_error('Unterminated string literal')
			}

			escape_ch := l.input[l.position]
			value += match escape_ch {
				`n` { '\n' }
				`t` { '\t' }
				`r` { '\r' }
				`\\` { '\\' }
				`"` { '"' }
				else { escape_ch.ascii_str() }
			}
		} else {
			value += ch.ascii_str()
		}
		l.advance()
	}

	if l.position >= l.input.len {
		return l.make_error('Unterminated string literal')
	}

	l.advance()
	return l.make_token_at(.string, value, start_pos)
}

fn (mut l Lexer) read_colon_or_double_colon() Token {
	start_pos := l.current_position()
	// Capture previous character before advancing (character before ':')
	prev_ch := if l.position > 0 { l.input[l.position - 1] } else { ` ` }
	l.advance() // Skip ':'

	// Check if next character is also ':' (double colon)
	if l.position < l.input.len && l.input[l.position] == `:` {
		l.advance() // Skip second ':'
		return l.make_token_at(.double_colon, '::', start_pos)
	}

	// Helper: determine if previous character indicates we should NOT parse an atom
	// If previous char is an identifier character or a closing delimiter, this ':' is a separator
	prev_is_ident := (prev_ch >= `a` && prev_ch <= `z`) || (prev_ch >= `A` && prev_ch <= `Z`) || (prev_ch >= `0` && prev_ch <= `9`) || prev_ch == `_`
	prev_is_closer := prev_ch == `]` || prev_ch == `)` || prev_ch == `}`

	// If previous suggests separator context, emit ':' token
	if prev_is_ident || prev_is_closer {
		return l.make_token_at(.colon, ':', start_pos)
	}

	// Otherwise, decide between atom or ':' based on next character
	if l.position < l.input.len && l.input[l.position].is_letter() {
		// This is an atom, read the identifier part
		mut value := ''
		for l.position < l.input.len {
			ch := l.input[l.position]
			if ch.is_alnum() || ch == `_` {
				value += ch.ascii_str()
				l.advance()
			} else {
				break
			}
		}
		return l.make_token_at(.atom, value, start_pos)
	}

	// Fallback: just a ':' token
	return l.make_token_at(.colon, ':', start_pos)
}

fn (mut l Lexer) read_atom() Token {
	start_pos := l.current_position()
	l.advance()

	if l.position >= l.input.len || !l.input[l.position].is_letter() {
		return l.make_error('Invalid atom: must start with letter')
	}

	mut value := ''

	for l.position < l.input.len {
		ch := l.input[l.position]
		if ch.is_alnum() || ch == `_` {
			value += ch.ascii_str()
			l.advance()
		} else {
			break
		}
	}

	return l.make_token_at(.atom, value, start_pos)
}

fn (mut l Lexer) read_number() Token {
	start_pos := l.current_position()
	start := l.position

	// Check for hexadecimal literal (0x)
	if l.position + 1 < l.input.len && l.input[l.position] == `0` && l.input[l.position + 1] == `x` {
		l.advance() // Skip '0'
		l.advance() // Skip 'x'

		// Read hexadecimal digits
		for l.position < l.input.len {
			ch := l.input[l.position]
			if ch.is_digit() || (ch >= `a` && ch <= `f`) || (ch >= `A` && ch <= `F`) {
				l.advance()
			} else {
				break
			}
		}

		value := l.input[start..l.position]
		return l.make_token_at(.integer, value, start_pos)
	}

	// Check for octal literal (0o)
	if l.position + 1 < l.input.len && l.input[l.position] == `0` && l.input[l.position + 1] == `o` {
		l.advance() // Skip '0'
		l.advance() // Skip 'o'

		// Read octal digits (0-7)
		for l.position < l.input.len {
			ch := l.input[l.position]
			if ch >= `0` && ch <= `7` {
				l.advance()
			} else {
				break
			}
		}

		value := l.input[start..l.position]
		return l.make_token_at(.integer, value, start_pos)
	}

	// Check for binary literal (0b)
	if l.position + 1 < l.input.len && l.input[l.position] == `0` && l.input[l.position + 1] == `b` {
		l.advance() // Skip '0'
		l.advance() // Skip 'b'

		// Read binary digits (0-1)
		for l.position < l.input.len {
			ch := l.input[l.position]
			if ch == `0` || ch == `1` {
				l.advance()
			} else {
				break
			}
		}

		value := l.input[start..l.position]
		return l.make_token_at(.integer, value, start_pos)
	}

	// Read decimal digits (and check for base generic format)
	for l.position < l.input.len && l.input[l.position].is_digit() {
		l.advance()
	}

	// Check for base generic format (BaseB)
	if l.position < l.input.len && l.input[l.position] == `B` {
		l.advance() // Skip 'B'

		// Read digits/letters for the specified base
		for l.position < l.input.len {
			ch := l.input[l.position]
			if ch.is_digit() || (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) {
				l.advance()
			} else {
				break
			}
		}

		value := l.input[start..l.position]
		return l.make_token_at(.integer, value, start_pos)
	}

	// Check for float
	if l.position < l.input.len && l.input[l.position] == `.` {
		if l.position + 1 < l.input.len && l.input[l.position + 1].is_digit() {
			l.advance()

			for l.position < l.input.len && l.input[l.position].is_digit() {
				l.advance()
			}

			value := l.input[start..l.position]
			return l.make_token_at(.float, value, start_pos)
		}
	}

	value := l.input[start..l.position]
	return l.make_token_at(.integer, value, start_pos)
}

fn (mut l Lexer) read_identifier() Token {
	start_pos := l.current_position()
	start := l.position
	mut before_has_operator := false
	for l.position < l.input.len {
		ch := l.input[l.position]
		if ch == `$` && l.position == start {
			l.advance() // Skip $
		} else if l.is_operator_char(ch) && l.position > start && before_has_operator {
			l.advance()
		} else if l.is_operator_char(ch) && l.position == start {
			l.advance()
			before_has_operator = true
		} else if ch.is_alnum() || ch == `_` {
			l.advance()
		} else {
			break
		}
	}

	value := l.input[start..l.position]
	token_type := match value {
		'=' { TokenType.bind }
		'==' { TokenType.identifier }
		'!=' { TokenType.identifier }
		'<=' { TokenType.identifier }
		'>=' { TokenType.identifier }
		'<' { TokenType.identifier }
		'>' { TokenType.identifier }
		'+' { TokenType.identifier }
		'-' { TokenType.identifier }
		'*' { TokenType.identifier }
		'/' { TokenType.identifier }
		'!' { TokenType.identifier }
		'|' { TokenType.pipe }
		'|>' { TokenType.pipe_forward }
		'def' { TokenType.def }
		'defp' { TokenType.defp }
		'do' { TokenType.do }
		'end' { TokenType.end }
		'true' { TokenType.true_ }
		'false' { TokenType.false_ }
		'nil' { TokenType.nil_ }
		'record' { TokenType.record }
		'case' { TokenType.case }
		'when' { TokenType.when }
		'fn' { TokenType.fn }
		'type' { TokenType.type }
		// Task 11: Control Flow Keywords
		'if' { TokenType.if_ }
		'else' { TokenType.else_ }
		'with' { TokenType.with }
		'match' { TokenType.match }
		'rescue' { TokenType.rescue }
		// Task 11: Concurrency Keywords
		'spawn' { TokenType.spawn }
		'receive' { TokenType.receive }
		'supervisor' { TokenType.supervisor }
		'worker' { TokenType.worker }
		// Task 11: Module System Keywords
		'deps' { TokenType.deps }
		'application' { TokenType.application }
		'import' { TokenType.import }
		// Task 11: Advanced Keywords
		'describe' { TokenType.describe }
		'test' { TokenType.test }
		'assert' { TokenType.assert }
		'for' { TokenType.for_ }
		'in' { TokenType.in }
		else { TokenType.identifier }
	}

	return l.make_token_at(token_type, value, start_pos)
}

fn (l Lexer) is_operator_char(ch u8) bool {
	return ch == `+` || ch == `-` || ch == `*` || ch == `/` || ch == `=` || ch == `!` || ch == `<`
		|| ch == `>` || ch == `&` || ch == `|` || ch == `^` || ch == `#` || ch == `@`
}

fn (mut l Lexer) read_arrow_or_minus() Token {
	start_pos := l.current_position()
	l.advance() // Skip -

	if l.position < l.input.len && l.input[l.position] == `>` {
		l.advance() // Skip >
		return l.make_token_at(.arrow, '->', start_pos)
	}

	return l.make_token_at(.identifier, '-', start_pos)
}

// Task 11: Read left angle operators: <, <<, <-
fn (mut l Lexer) read_left_angle_operators() Token {
	start_pos := l.current_position()
	l.advance() // Skip <

	if l.position < l.input.len {
		next_ch := l.input[l.position]
		if next_ch == `<` {
			l.advance() // Skip second <
			// Check for third <
			if l.position < l.input.len && l.input[l.position] == `<` {
				l.advance() // Skip third <
				// Shift-left operator
				return l.make_token_at(.identifier, '<<<', start_pos)
			}
			// Binary literal opener
			return l.make_token_at(.double_lt, '<<', start_pos)
		} else if next_ch == `-` {
			l.advance() // Skip -
			return l.make_token_at(.left_arrow, '<-', start_pos)
		} else if next_ch == `=` {
			l.advance() // Skip '='
			return l.make_token_at(.identifier, '<=', start_pos)
		}
	}

	return l.make_token_at(.identifier, '<', start_pos)
}

// Task 11: Read right angle operators: >, >>, >>>
fn (mut l Lexer) read_right_angle_operators() Token {
	start_pos := l.current_position()
	l.advance() // Skip >

	if l.position < l.input.len {
		if l.input[l.position] == `>` {
			l.advance() // Skip second >
			// Check for third >
			if l.position < l.input.len && l.input[l.position] == `>` {
				l.advance() // Skip third >
				// Shift-right operator
				return l.make_token_at(.identifier, '>>>', start_pos)
			}
			// Binary literal closer
			return l.make_token_at(.double_gt, '>>', start_pos)
		} else if l.input[l.position] == `=` {
			l.advance() // Skip '='
			return l.make_token_at(.identifier, '>=', start_pos)
		}
	}

	return l.make_token_at(.identifier, '>', start_pos)
}

// Read exclamation operators: !, !=
fn (mut l Lexer) read_exclamation_operators() Token {
	start_pos := l.current_position()
	l.advance() // Skip !

	if l.position < l.input.len && l.input[l.position] == `=` {
		l.advance() // Skip =
		return l.make_token_at(.identifier, '!=', start_pos)
	}

	return l.make_token_at(.exclamation, '!', start_pos)
}

// Read equals operators: =, ==
fn (mut l Lexer) read_equals_operators() Token {
	start_pos := l.current_position()
	l.advance() // Skip =

	if l.position < l.input.len && l.input[l.position] == `=` {
		l.advance() // Skip second =
		return l.make_token_at(.identifier, '==', start_pos)
	}

	return l.make_token_at(.bind, '=', start_pos)
}

// Read single-quoted string -> .charlist
fn (mut l Lexer) read_charlist() Token {
	start_pos := l.current_position()
	l.advance() // Skip opening '

	mut value := ''
	for l.position < l.input.len && l.input[l.position] != `'` {
		ch := l.input[l.position]
		if ch == `\\` {
			l.advance()
			if l.position >= l.input.len {
				return l.make_error('Unterminated charlist literal')
			}
			escape_ch := l.input[l.position]
			value += match escape_ch {
				`n` { '\n' }
				`t` { '\t' }
				`r` { '\r' }
				`\\` { '\\' }
				`'` { '\'' }
				else { escape_ch.ascii_str() }
			}
		} else {
			value += ch.ascii_str()
		}
		l.advance()
	}
	if l.position >= l.input.len {
		return l.make_error('Unterminated charlist literal')
	}
	l.advance() // Skip closing '
	return l.make_token_at(.charlist, value, start_pos)
}
