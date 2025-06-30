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
	start_pos ast.Position
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
		start_pos: ast.new_position(1, 1, filename)
		errors:    []
		had_error: false
	}
}

// next_token returns the next token from the input
pub fn (mut l Lexer) next_token() Token {
	// Se houve erro fatal, só retorna EOF
	if l.had_error {
		return EOFToken{}
	}
	// If there are errors, always return ErrorToken first
	if l.has_errors() {
		l.had_error = true
		msg := l.errors[0].message
		l.errors.delete(0)
		l.errors.clear()
		return ErrorToken{
			message: msg
		}
	}

	for l.position < l.input.len {
		ch := l.input[l.position]

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
				// Após erro, não consumir o caractere inválido e parar
				l.add_error('Lexical error: Unexpected character', l.get_current_position())
				l.state = .initial
				l.buffer = ''
				msg := l.errors[0].message
				l.errors.delete(0)
				l.errors.clear()
				l.had_error = false
				return ErrorToken{
					message: msg
				}
			}
		}

		match transition.action or { TransitionAction.consume_character } {
			.consume_character {
				l.buffer += ch.ascii_str()
				l.position++
				if ch == `\n` {
					l.line++
					l.column = 1
					// Emit NewlineToken immediately when we encounter \n
					l.state = transition.to_state
					return NewlineToken{}
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
			}
			.skip_character {
				l.position++
				if ch == `\n` {
					l.line++
					l.column = 1
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
					message: error_message
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
			message: msg
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
				message: msg
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
				message: msg
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

	return EOFToken{}
}

// create_token_from_buffer creates a token from the current buffer
fn (mut l Lexer) create_token_from_buffer() Token {
	if l.buffer.len == 0 {
		return EOFToken{}
	}

	return match l.state {
		.identifier {
			if is_keyword(l.buffer) {
				return get_keyword_token(l.buffer) or { panic('Internal error: keyword not found') }
			} else if is_operator(l.buffer) {
				operator_token := get_operator_token(l.buffer) or {
					panic('Internal error: operator not found')
				}
				operator_token
			} else {
				if l.buffer[0].is_capital() {
					UpperIdentToken{
						value: l.buffer
					}
				} else {
					IdentToken{
						value: l.buffer
					}
				}
			}
		}
		.number {
			value := l.buffer.int()
			IntToken{
				value: value
			}
		}
		.float {
			// Check if float ends with dot
			if l.buffer.ends_with('.') {
				return ErrorToken{
					message: 'Lexical error: Float literal cannot end with dot'
				}
			}
			value := l.buffer.f64()
			FloatToken{
				value: value
			}
		}
		.string {
			value := l.parse_string_literal() or {
				return ErrorToken{
					message: 'Lexical error: Invalid escape sequence in string literal'
				}
			}
			StringToken{
				value: value
			}
		}
		.atom {
			if l.buffer.len == 1 {
				return ErrorToken{
					message: 'Lexical error: Isolated colon is not allowed'
				}
			}
			value := l.buffer[1..]
			AtomToken{
				value: value
			}
		}
		.atom_start {
			// If we have only ':', it's an error
			if l.buffer == ':' {
				return ErrorToken{
					message: 'Lexical error: Isolated colon is not allowed'
				}
			}
			// This should not happen in normal flow, but just in case
			ErrorToken{
				message: 'Lexical error: Unexpected state in atom_start'
			}
		}
		.comment {
			EOFToken{}
		}
		.operator {
			if is_punctuation(l.buffer) {
				if ptok := get_punctuation_token(l.buffer) {
					ptok
				} else {
					ErrorToken{
						message: 'Lexical error: Unknown punctuation'
					}
				}
			} else if is_operator(l.buffer) {
				if otok := get_operator_token(l.buffer) {
					otok
				} else {
					ErrorToken{
						message: 'Lexical error: Unknown operator'
					}
				}
			} else {
				// Only emit error for single colon if we're at the end of input
				// or if the next character can't form a valid operator
				if l.buffer == ':' && l.position >= l.input.len {
					return ErrorToken{
						message: 'Lexical error: Isolated colon is not allowed'
					}
				}
				ErrorToken{
					message: 'Lexical error: Unknown operator'
				}
			}
		}
		else {
			ErrorToken{
				message: 'Lexical error: Unexpected state'
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
fn (l Lexer) get_current_position() ast.Position {
	return ast.new_position(l.line, l.column, l.filename)
}

// add_error adds an error to the lexer's error collection
fn (mut l Lexer) add_error(message string, position ast.Position) {
	err := errors.new_compilation_error(errors.LexicalError{ message: message }, position,
		message)
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
	l.start_pos = ast.new_position(1, 1, l.filename)
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
pub fn (l Lexer) get_position() ast.Position {
	return l.get_current_position()
}

// get_span returns a span from start to current position
pub fn (l Lexer) get_span(start ast.Position) ast.Span {
	return ast.new_span(start, l.get_current_position())
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
