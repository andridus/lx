module lexer

// Transition represents a state transition in the lexer
pub struct Transition {
pub:
	from_state LexerState
	to_state   LexerState
	condition  TransitionCondition
	action     ?TransitionAction
}

// TransitionCondition represents the condition for a transition using sum types
pub type TransitionCondition = CharacterCondition
	| CharacterClassCondition
	| KeywordCondition
	| OperatorCondition
	| PunctuationCondition
	| AlwaysCondition
	| NeverCondition

// CharacterCondition represents a character match condition
pub struct CharacterCondition {
pub:
	value u8
}

// CharacterClassCondition represents a character class match condition
pub struct CharacterClassCondition {
pub:
	class CharacterClass
}

// KeywordCondition represents a keyword match condition
pub struct KeywordCondition {
pub:
	keyword string
}

// OperatorCondition represents an operator match condition
pub struct OperatorCondition {
pub:
	operator string
}

// PunctuationCondition represents a punctuation match condition
pub struct PunctuationCondition {
pub:
	punctuation string
}

// AlwaysCondition represents a condition that always matches
pub struct AlwaysCondition {
}

// NeverCondition represents a condition that never matches
pub struct NeverCondition {
}

// CharacterClass represents a class of characters
pub enum CharacterClass {
	letter
	digit
	whitespace
	operator_start
	identifier_start
	identifier_part
	escape_sequence
	newline
	quote
	hash
	colon
	dot
	underscore
	punctuation
}

// TransitionAction represents an action to perform during transition
pub enum TransitionAction {
	emit_token
	emit_error
	skip_character
	backtrack
	consume_character
}

// str returns a string representation of TransitionCondition
pub fn (c TransitionCondition) str() string {
	return match c {
		CharacterCondition { 'char(${c.value})' }
		CharacterClassCondition { 'class(${c.class})' }
		KeywordCondition { 'keyword(${c.keyword})' }
		OperatorCondition { 'operator(${c.operator})' }
		PunctuationCondition { 'punctuation(${c.punctuation})' }
		AlwaysCondition { 'always' }
		NeverCondition { 'never' }
	}
}

// str returns a string representation of CharacterClass
pub fn (c CharacterClass) str() string {
	return match c {
		.letter { 'letter' }
		.digit { 'digit' }
		.whitespace { 'whitespace' }
		.operator_start { 'operator_start' }
		.identifier_start { 'identifier_start' }
		.identifier_part { 'identifier_part' }
		.escape_sequence { 'escape_sequence' }
		.newline { 'newline' }
		.quote { 'quote' }
		.hash { 'hash' }
		.colon { 'colon' }
		.dot { 'dot' }
		.underscore { 'underscore' }
		.punctuation { 'punctuation' }
	}
}

// str returns a string representation of TransitionAction
pub fn (a TransitionAction) str() string {
	return match a {
		.emit_token { 'emit_token' }
		.emit_error { 'emit_error' }
		.skip_character { 'skip_character' }
		.backtrack { 'backtrack' }
		.consume_character { 'consume_character' }
	}
}

// matches checks if a character matches a transition condition
pub fn (c TransitionCondition) matches(ch u8) bool {
	return match c {
		CharacterCondition { ch == c.value }
		CharacterClassCondition { c.class.matches(ch) }
		KeywordCondition { false } // Keywords are handled separately
		OperatorCondition { false } // Operators are handled separately
		PunctuationCondition { false } // Punctuation is handled separately
		AlwaysCondition { true }
		NeverCondition { false }
	}
}

// matches checks if a character matches a character class
pub fn (c CharacterClass) matches(ch u8) bool {
	return match c {
		.letter { ch.is_letter() }
		.digit { ch.is_digit() }
		.whitespace { ch.is_space() }
		.operator_start { is_operator_start(ch) }
		.identifier_start { ch.is_letter() || ch == `_` }
		.identifier_part { ch.is_letter() || ch.is_digit() || ch == `_` }
		.escape_sequence { ch == `\\` }
		.newline { ch == `\n` || ch == `\r` }
		.quote { ch == `"` }
		.hash { ch == `#` }
		.colon { ch == `:` }
		.dot { ch == `.` }
		.underscore { ch == `_` }
		.punctuation { is_punctuation_char(ch) }
	}
}

// get_transitions returns all possible transitions for the lexer
pub fn get_transitions() []Transition {
	return [
		Transition{
			from_state: .initial
			to_state:   .identifier
			condition:  CharacterClassCondition{
				class: .identifier_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .number
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .string
			condition:  CharacterClassCondition{
				class: .quote
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .comment
			condition:  CharacterClassCondition{
				class: .hash
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .atom_start
			condition:  CharacterCondition{
				value: `:`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .operator
			condition:  CharacterClassCondition{
				class: .operator_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .whitespace
			condition:  CharacterClassCondition{
				class: .whitespace
			}
			action:     .skip_character
		},
		Transition{
			from_state: .initial
			to_state:   .punctuation
			condition:  CharacterClassCondition{
				class: .punctuation
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .directive
			condition:  CharacterCondition{
				value: `@`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .initial
			to_state:   .binary
			condition:  CharacterCondition{
				value: `<`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .whitespace
			to_state:   .whitespace
			condition:  CharacterClassCondition{
				class: .whitespace
			}
			action:     .skip_character
		},
		Transition{
			from_state: .whitespace
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .backtrack
		},
		Transition{
			from_state: .identifier
			to_state:   .identifier
			condition:  CharacterClassCondition{
				class: .identifier_part
			}
			action:     .consume_character
		},
		Transition{
			from_state: .identifier
			to_state:   .key
			condition:  CharacterCondition{
				value: `:`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .identifier
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .key
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .number
			to_state:   .number
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .number
			to_state:   .float
			condition:  CharacterCondition{
				value: `.`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .number
			to_state:   .error
			condition:  CharacterClassCondition{
				class: .letter
			}
			action:     .emit_error
		},
		Transition{
			from_state: .number
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .float
			to_state:   .float
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .float
			to_state:   .error
			condition:  CharacterCondition{
				value: `.`
			}
			action:     .emit_error
		},
		Transition{
			from_state: .float
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .string
			to_state:   .string
			condition:  CharacterClassCondition{
				class: .escape_sequence
			}
			action:     .consume_character
		},
		Transition{
			from_state: .string
			to_state:   .string
			condition:  CharacterClassCondition{
				class: .quote
			}
			action:     .consume_character
		},
		Transition{
			from_state: .string
			to_state:   .error
			condition:  CharacterClassCondition{
				class: .newline
			}
			action:     .emit_error
		},
		Transition{
			from_state: .string
			to_state:   .string
			condition:  AlwaysCondition{}
			action:     .consume_character
		},
		Transition{
			from_state: .atom
			to_state:   .atom
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .atom
			to_state:   .atom
			condition:  CharacterClassCondition{
				class: .identifier_part
			}
			action:     .consume_character
		},
		Transition{
			from_state: .atom
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .atom_start
			to_state:   .operator
			condition:  CharacterCondition{
				value: `:`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .atom_start
			to_state:   .atom
			condition:  CharacterClassCondition{
				class: .identifier_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .atom_start
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .comment
			to_state:   .initial
			condition:  CharacterClassCondition{
				class: .newline
			}
			action:     .skip_character
		},
		Transition{
			from_state: .comment
			to_state:   .comment
			condition:  AlwaysCondition{}
			action:     .consume_character
		},
		Transition{
			from_state: .directive
			to_state:   .initial
			condition:  CharacterClassCondition{
				class: .newline
			}
			action:     .emit_token
		},
		Transition{
			from_state: .directive
			to_state:   .initial
			condition:  CharacterClassCondition{
				class: .whitespace
			}
			action:     .emit_token
		},
		Transition{
			from_state: .directive
			to_state:   .error
			condition:  CharacterClassCondition{
				class: .operator_start
			}
			action:     .emit_error
		},
		Transition{
			from_state: .directive
			to_state:   .error
			condition:  CharacterClassCondition{
				class: .punctuation
			}
			action:     .emit_error
		},
		Transition{
			from_state: .directive
			to_state:   .error
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .emit_error
		},
		Transition{
			from_state: .directive
			to_state:   .directive
			condition:  CharacterClassCondition{
				class: .identifier_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .directive
			to_state:   .directive
			condition:  CharacterClassCondition{
				class: .identifier_part
			}
			action:     .consume_character
		},
		Transition{
			from_state: .directive
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .operator
			to_state:   .operator
			condition:  CharacterClassCondition{
				class: .operator_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .operator
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .punctuation
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .error
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_error
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterCondition{
				value: `<`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterCondition{
				value: `>`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterCondition{
				value: `:`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterClassCondition{
				class: .identifier_start
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterClassCondition{
				class: .identifier_part
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterCondition{
				value: `,`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .binary
			condition:  CharacterClassCondition{
				class: .whitespace
			}
			action:     .consume_character
		},
		Transition{
			from_state: .binary
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .number
			to_state:   .hex_number
			condition:  CharacterCondition{
				value: `x`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .hex_number
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .hex_number
			condition:  CharacterClassCondition{
				class: .letter
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
		Transition{
			from_state: .initial
			to_state:   .hex_number
			condition:  CharacterCondition{
				value: `0`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .hex_number
			condition:  CharacterCondition{
				value: `x`
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .hex_number
			condition:  CharacterClassCondition{
				class: .digit
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .hex_number
			condition:  CharacterClassCondition{
				class: .letter
			}
			action:     .consume_character
		},
		Transition{
			from_state: .hex_number
			to_state:   .initial
			condition:  AlwaysCondition{}
			action:     .emit_token
		},
	]
}

// find_transition finds a valid transition for the current state and character
pub fn find_transition(current_state LexerState, ch u8, buffer string) ?Transition {
	transitions := get_transitions()

	for transition in transitions {
		if transition.from_state == current_state {
			if transition.condition.matches(ch) {
				return transition
			}
		}
	}

	return none
}

// get_final_transition gets the transition to finalize the current token
pub fn get_final_transition(current_state LexerState) ?Transition {
	transitions := get_transitions()

	for transition in transitions {
		if transition.from_state == current_state {
			if transition.condition is AlwaysCondition {
				if transition.action or { TransitionAction.emit_token } == TransitionAction.emit_token {
					return transition
				}
			}
		}
	}

	return none
}

// is_punctuation_char checks if a character is a punctuation character
fn is_punctuation_char(ch u8) bool {
	return ch == `(` || ch == `)` || ch == `{` || ch == `}` || ch == `[` || ch == `]` || ch == `,`
		|| ch == `;` || ch == `:`
}
