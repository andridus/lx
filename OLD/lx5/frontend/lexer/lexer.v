module lexer

import ast
import errors

// Lexer represents the lexical analyzer for LX language
@[heap]
pub struct Lexer {
mut:
	input     []u8
	position  int
	line      int
	column    int
	filename  string
	state     LexerState
	buffer    string
	start_pos TokenPosition
	errors    []errors.CompilationError
	had_error bool
}

// new_lexer creates a new lexer instance
pub fn new_lexer(input string, filename string) Lexer {
	return Lexer{
		input:     input.bytes()
		position:  0
		line:      1
		column:    1
		filename:  filename
		state:     .initial
		buffer:    ''
		start_pos: new_token_position(1, 1, filename)
		errors:    []
		had_error: false
	}
}

pub fn (mut l Lexer) next_token() Token {
	if l.had_error {
		return EOFToken{
			position: l.get_current_position()
		}
	}

	if l.has_errors() {
		l.had_error = true
		msg := l.errors[0].message
		l.errors.delete(0)
		l.errors.clear()
		return ErrorToken{
			message:  msg
			position: l.get_current_position()
		}
	}

	for l.position < l.input.len {
		ch := l.input[l.position]

		if l.state == .initial && ch == `0` && l.position + 1 < l.input.len
			&& l.input[l.position + 1] == `x` {
			l.state = .hex_number
			l.start_pos = l.get_current_position()
			l.buffer = '0x'
			l.position += 2
			l.column += 2
			continue
		}

		if l.state == .hex_number {
			if ch.is_digit() || (ch >= `a` && ch <= `f`) || (ch >= `A` && ch <= `F`) {
				l.buffer += ch.ascii_str()
				l.position++
				l.column++
				continue
			} else {
				token := l.create_token_from_buffer()
				l.buffer = ''
				l.state = .initial
				return token
			}
		}

		// Find transition for current state and character
		transition := find_transition(l.state, ch, l.buffer) or {
			if l.state == .operator && l.buffer.len > 0 {
				token := l.create_token_from_buffer()
				l.buffer = ''
				l.state = .initial
				return token
			}
			if l.state.is_final_state() {
				token := l.create_token_from_buffer()
				l.buffer = ''
				l.state = .initial
				return token
			} else {
				l.add_error('Lexical error: Unexpected character', l.get_current_position())
				l.state = .initial
				l.buffer = ''
				msg := l.errors[0].message
				l.errors.delete(0)
				l.errors.clear()
				l.had_error = false
				return ErrorToken{
					message:  msg
					position: l.get_current_position()
				}
			}
		}

		match transition.action or { TransitionAction.consume_character } {
			.consume_character {
				if l.state == .initial
					&& (ch != ` ` && ch != `\t` && ch != `\n` && ch != `\r` && ch != `#`) {
					l.start_pos = l.get_current_position()
				}
				l.buffer += ch.ascii_str()
				l.position++
				if ch == `\n` {
					l.line++
					l.column = 1
					// Emit NewlineToken immediately when we encounter \n
					l.state = transition.to_state
					return NewlineToken{
						position: l.get_current_position()
					}
				} else {
					l.column++
				}
				l.state = transition.to_state

				// Verificar se é aspas de fechamento não escapada
				if l.state == .string && ch == `"` && l.buffer.len > 1 {
					mut escaped := false
					mut j := l.buffer.len - 2 // posição antes da aspas atual
					mut consecutive_backslashes := 0
					// Contar barras invertidas consecutivas antes da aspas
					for j >= 0 && l.buffer[j] == `\\` {
						consecutive_backslashes++
						j--
					}
					// Se o número de barras invertidas consecutivas for ímpar, a aspas está escapada
					escaped = consecutive_backslashes % 2 == 1
					if !escaped {
						// Aspas não escapada, emitir token
						token := l.create_token_from_buffer()
						l.state = .initial
						l.buffer = ''
						l.start_pos = l.get_current_position()
						return token
					}
				}

				// Special handling for @: only allow as directive at start of line (ignoring spaces/tabs) and only in .initial
				if ch == `@` && l.state == .initial {
					mut i := l.position - 1
					mut only_spaces := true
					for i >= 0 && l.input[i] != `\n` && l.input[i] != `\r` {
						if l.input[i] != ` ` && l.input[i] != `\t` {
							only_spaces = false
							break
						}
						i--
					}
					if !only_spaces {
						l.add_error('Lexical error: Unexpected character', l.get_current_position())
						l.state = .initial
						l.buffer = ''
						msg := l.errors[0].message
						l.errors.delete(0)
						l.errors.clear()
						l.had_error = false
						return ErrorToken{
							message:  msg
							position: l.get_current_position()
						}
					}
				}
			}
			.skip_character {
				l.position++
				if ch == `\n` {
					l.line++
					l.column = 1
					// Após pular linha, o próximo token deve atualizar l.start_pos
					l.state = transition.to_state
					l.buffer = ''
					continue
				} else {
					l.column++
				}
				if l.state == .comment && transition.to_state == .initial {
					l.buffer = ''
				}
				l.state = transition.to_state
			}
			.emit_token {
				token := l.create_token_from_buffer()
				l.state = transition.to_state
				l.buffer = ''
				l.start_pos = l.get_current_position()
				return token
			}
			.emit_error {
				// Após erro, parar e não consumir mais caracteres
				mut error_message := 'Lexical error: Unexpected character'
				if l.position < l.input.len && l.input[l.position] == `.` {
					error_message = 'Lexical error: Float literals must start with a digit'
				}
				l.add_error(error_message, l.get_current_position())
				l.state = transition.to_state
				l.buffer = ''
				return ErrorToken{
					message:  error_message
					position: l.get_current_position()
				}
			}
			.backtrack {
				l.state = transition.to_state
				continue
			}
		}
	}

	// Check for unterminated string at end of input
	if l.state == .string {
		l.add_error('Lexical error: Unterminated string literal', l.get_current_position())
		msg := l.errors[0].message
		l.errors.delete(0)
		return ErrorToken{
			message:  msg
			position: l.get_current_position()
		}
	}

	// Check for invalid character after number
	if l.state == .number && l.position < l.input.len {
		next_ch := l.input[l.position]
		if next_ch.is_letter() {
			// Consome todos os caracteres válidos para identificador
			for l.position < l.input.len
				&& (l.input[l.position].is_letter() || l.input[l.position].is_digit()
				|| l.input[l.position] == `_`) {
				l.position++
				l.column++
			}
			l.add_error('Lexical error: Invalid identifier after number', l.get_current_position())
			msg := l.errors[0].message
			l.errors.delete(0)
			l.errors.clear()
			l.had_error = false
			l.buffer = ''
			l.state = .initial
			return ErrorToken{
				message:  msg
				position: l.get_current_position()
			}
		}
	}

	// Check for invalid float at end of input
	if l.state == .float {
		if l.buffer.ends_with('.') {
			l.add_error('Lexical error: Float literal cannot end with dot', l.get_current_position())
			msg := l.errors[0].message
			l.errors.delete(0)
			return ErrorToken{
				message:  msg
				position: l.get_current_position()
			}
		}
	}

	// Handle operator state specifically
	if l.buffer.len > 0 && l.state == .operator {
		token := l.create_token_from_buffer()
		l.buffer = ''
		l.state = .initial
		return token
	}

	// Handle atom_start state specifically
	if l.buffer.len > 0 && l.state == .atom_start {
		token := l.create_token_from_buffer()
		l.buffer = ''
		l.state = .initial
		return token
	}

	// Emit pending token if buffer is not empty and state is final
	if l.buffer.len > 0 && l.state.is_final_state() {
		// Só emite token se buffer não foi limpo por erro
		if l.state != .initial {
			token := l.create_token_from_buffer()
			l.buffer = ''
			l.state = .initial
			return token
		}
	}

	return EOFToken{
		position: l.get_current_position()
	}
}

// create_token_from_buffer creates a token from the current buffer
fn (mut l Lexer) create_token_from_buffer() Token {
	if l.buffer.len == 0 {
		return EOFToken{
			position: l.get_current_position()
		}
	}

	return match l.state {
		.identifier {
			if is_keyword(l.buffer) {
				buffer := l.buffer
				keyword_token := get_keyword_token(buffer, l.start_pos) or {
					panic('Internal error: keyword not found')
				}
				// Convert boolean keywords to BoolToken
				match keyword_token.value {
					.true_ {
						BoolToken{
							value:    true
							position: l.start_pos
						}
					}
					.false_ {
						BoolToken{
							value:    false
							position: l.start_pos
						}
					}
					else {
						KeywordToken{
							value:    keyword_token.value
							position: l.start_pos
						}
					}
				}
			} else if is_operator(l.buffer) {
				operator_token := get_operator_token(l.buffer, l.start_pos) or {
					panic('Internal error: operator not found')
				}
				OperatorToken{
					value:    operator_token.value
					position: l.start_pos
				}
			} else {
				if l.buffer[0].is_capital() {
					UpperIdentToken{
						value:    l.buffer
						position: l.start_pos
					}
				} else {
					IdentToken{
						value:    l.buffer
						position: l.start_pos
					}
				}
			}
		}
		.key {
			// Remove the colon from the end to get the key name
			key_name := l.buffer[..l.buffer.len - 1]
			KeyToken{
				value:    key_name
				position: l.start_pos
			}
		}
		.number {
			mut value := 0
			if l.buffer.starts_with('0x') {
				hex_str := l.buffer[2..]
				value = parse_hex_int(hex_str) or { 0 }
			} else {
				value = l.buffer.int()
			}
			IntToken{
				value:    value
				position: l.start_pos
			}
		}
		.float {
			// Check if float ends with dot
			if l.buffer.ends_with('.') {
				return ErrorToken{
					message:  'Lexical error: Float literal cannot end with dot'
					position: l.start_pos
				}
			}
			value := l.buffer.f64()
			FloatToken{
				value:    value
				position: l.start_pos
			}
		}
		.string {
			value := l.parse_string_literal() or {
				return ErrorToken{
					message:  'Lexical error: Invalid escape sequence in string literal'
					position: l.start_pos
				}
			}
			StringToken{
				value:    value
				position: l.start_pos
			}
		}
		.atom {
			if l.buffer.len == 1 {
				if l.state != .binary {
					return ErrorToken{
						message:  'Lexical error: Isolated colon is not allowed'
						position: l.start_pos
					}
				}
			}
			value := l.buffer[1..]
			AtomToken{
				value:    value
				position: l.start_pos
			}
		}
		.atom_start {
			// If we have only ':', it's an error
			if l.buffer == ':' {
				return ErrorToken{
					message:  'Lexical error: Isolated colon is not allowed'
					position: l.start_pos
				}
			}
			// This should not happen in normal flow, but just in case
			ErrorToken{
				message:  'Lexical error: Unexpected state in atom_start'
				position: l.start_pos
			}
		}
		.comment {
			EOFToken{
				position: l.get_current_position()
			}
		}
		.directive {
			// Extract directive name (remove @ prefix)
			directive_text := l.buffer.trim_left('@')
			DirectiveToken{
				directive: directive_text
				position:  l.start_pos
			}
		}
		.operator {
			if is_punctuation(l.buffer) {
				if ptok := get_punctuation_token(l.buffer, l.start_pos) {
					PunctuationToken{
						value:    ptok.value
						position: l.start_pos
					}
				} else {
					ErrorToken{
						message:  'Lexical error: Unknown punctuation'
						position: l.start_pos
					}
				}
			} else if is_operator(l.buffer) {
				operator_token := get_operator_token(l.buffer, l.start_pos) or {
					panic('Internal error: operator not found')
				}
				OperatorToken{
					value:    operator_token.value
					position: l.start_pos
				}
			} else {
				// Special case: if buffer ends with ::, it might be a key followed by ::
				if l.buffer.ends_with('::') {
					// Extract the key part (remove the ::)
					key_part := l.buffer[..l.buffer.len - 2]
					if key_part.len > 0 {
						// This is a key followed by ::, but we can't return two tokens
						// So we'll return an error for now
						ErrorToken{
							message:  'Lexical error: Unexpected :: after key'
							position: l.start_pos
						}
					} else {
						ErrorToken{
							message:  'Lexical error: Unknown operator'
							position: l.start_pos
						}
					}
				} else {
					ErrorToken{
						message:  'Lexical error: Unknown operator'
						position: l.start_pos
					}
				}
			}
		}
		.punctuation {
			if is_punctuation(l.buffer) {
				if ptok := get_punctuation_token(l.buffer, l.start_pos) {
					PunctuationToken{
						value:    ptok.value
						position: l.start_pos
					}
				} else {
					ErrorToken{
						message:  'Lexical error: Unknown punctuation'
						position: l.start_pos
					}
				}
			} else {
				ErrorToken{
					message:  'Lexical error: Unknown punctuation'
					position: l.start_pos
				}
			}
		}
		.hex_number {
			mut value := 0
			if l.buffer.starts_with('0x') {
				hex_str := l.buffer[2..]
				value = parse_hex_int(hex_str) or { 0 }
			}
			IntToken{
				value:    value
				position: l.start_pos
			}
		}
		else {
			ErrorToken{
				message:  'Lexical error: Unexpected state'
				position: l.start_pos
			}
		}
	}
}

// parse_string_literal parses a string literal and handles escape sequences
fn (mut l Lexer) parse_string_literal() ?string {
	if l.buffer.len < 2 {
		return ''
	}

	mut result := ''
	mut i := 1 // começa após a aspas de abertura
	for i < l.buffer.len - 1 {
		ch := l.buffer[i]
		if ch == `\\` && i + 1 < l.buffer.len - 1 {
			next := l.buffer[i + 1]
			match next {
				`n` {
					result += '\n'
				}
				`t` {
					result += '\t'
				}
				`r` {
					result += '\r'
				}
				`"` {
					result += '"'
				}
				`\\` {
					result += '\\'
				}
				else {
					l.add_error('Lexical error: Invalid escape sequence', l.get_current_position())
					return none
				}
			}
			i += 2
		} else {
			result += l.buffer[i].ascii_str()
			i++
		}
	}
	// Se a última aspas for escapada, ela já foi processada acima
	return result
}

// get_current_position returns the current position
fn (l Lexer) get_current_position() TokenPosition {
	return new_token_position(l.line, l.column, l.filename)
}

// add_error adds an error to the lexer's error collection
fn (mut l Lexer) add_error(message string, position TokenPosition) {
	err := errors.new_compilation_error(errors.LexicalError{ message: message }, ast.new_position(position.line,
		position.column, position.filename), message)
	l.errors << err
}

// has_errors checks if the lexer has encountered any errors
pub fn (l Lexer) has_errors() bool {
	return l.errors.len > 0
}

// get_errors returns all errors encountered by the lexer
pub fn (l Lexer) get_errors() []errors.CompilationError {
	return l.errors
}

// reset resets the lexer to its initial state
pub fn (mut l Lexer) reset() {
	l.position = 0
	l.line = 1
	l.column = 1
	l.state = .initial
	l.buffer = ''
	l.start_pos = new_token_position(1, 1, l.filename)
	l.errors.clear()
	l.had_error = false
}

// peek looks at the next character without consuming it
pub fn (l Lexer) peek() ?u8 {
	if l.position >= l.input.len {
		return none
	}
	return l.input[l.position]
}

// peek_next looks at the next two characters without consuming them
pub fn (l Lexer) peek_next() ?u8 {
	if l.position + 1 >= l.input.len {
		return none
	}
	return l.input[l.position + 1]
}

// advance advances the position by one character
pub fn (mut l Lexer) advance() {
	if l.position < l.input.len {
		ch := l.input[l.position]
		l.position++

		if ch == `\n` {
			l.line++
			l.column = 1
		} else {
			l.column++
		}
	}
}

// is_at_end checks if the lexer has reached the end of input
pub fn (l Lexer) is_at_end() bool {
	return l.position >= l.input.len
}

// get_position returns the current position information
pub fn (l Lexer) get_position() TokenPosition {
	return l.get_current_position()
}

// get_span returns a span from start to current position
pub fn (l Lexer) get_span(start TokenPosition) ast.Span {
	return ast.new_span(ast.new_position(start.line, start.column, start.filename), ast.new_position(l.line,
		l.column, l.filename))
}

// get_input returns the input string
pub fn (l Lexer) get_input() string {
	return l.input.bytestr()
}

// get_position_info returns position information
pub fn (l Lexer) get_position_info() (int, int, int) {
	return l.position, l.line, l.column
}

// get_filename returns the filename
pub fn (l Lexer) get_filename() string {
	return l.filename
}

// get_state returns the current state of the lexer
pub fn (l Lexer) get_state() LexerState {
	return l.state
}

// get_position returns the current position in the input
pub fn (l Lexer) get_position_index() int {
	return l.position
}

// get_buffer returns the current buffer
pub fn (l Lexer) get_buffer() string {
	return l.buffer
}

// parse_hex_int converts a hexadecimal string to an integer
fn parse_hex_int(hex_str string) ?int {
	if hex_str.len == 0 {
		return none
	}

	mut result := 0
	for ch in hex_str {
		digit := match ch {
			`0`...`9` { int(ch - `0`) }
			`a`...`f` { int(ch - `a` + 10) }
			`A`...`F` { int(ch - `A` + 10) }
			else { return none }
		}
		result = result * 16 + digit
	}
	return result
}
