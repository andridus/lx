module lexer

// LexerState represents the current state of the lexer
pub enum LexerState {
	initial
	atom_start
	identifier
	key
	number
	float
	string
	atom
	comment
	directive
	operator
	whitespace
	error
	punctuation
	binary
	hex_number
}

// str returns a string representation of LexerState
pub fn (s LexerState) str() string {
	return match s {
		.initial { 'initial' }
		.atom_start { 'atom_start' }
		.identifier { 'identifier' }
		.key { 'key' }
		.number { 'number' }
		.float { 'float' }
		.string { 'string' }
		.atom { 'atom' }
		.comment { 'comment' }
		.directive { 'directive' }
		.operator { 'operator' }
		.whitespace { 'whitespace' }
		.error { 'error' }
		.punctuation { 'punctuation' }
		.binary { 'binary' }
		.hex_number { 'hex_number' }
	}
}

// is_final_state checks if a state is a final state (can emit a token)
pub fn (s LexerState) is_final_state() bool {
	return match s {
		.identifier, .key, .number, .float, .string, .atom, .directive, .error, .punctuation, .binary, .hex_number { true }
		else { false }
	}
}

// can_transition_to checks if a transition from current state to target state is valid
pub fn (current LexerState) can_transition_to(target LexerState) bool {
	return match current {
		.initial {
			match target {
				.identifier, .number, .string, .atom, .comment, .directive, .operator, .whitespace, .binary, .hex_number {
					true
				}
				else {
					false
				}
			}
		}
		.atom_start {
			match target {
				.identifier, .number, .string, .atom, .comment, .directive, .operator, .whitespace {
					true
				}
				else {
					false
				}
			}
		}
		.identifier {
			match target {
				.initial, .key, .operator, .whitespace, .comment { true }
				else { false }
			}
		}
		.key {
			match target {
				.initial, .operator, .whitespace, .comment { true }
				else { false }
			}
		}
		.number {
			match target {
				.float, .initial, .operator, .whitespace, .comment { true }
				else { false }
			}
		}
		.float {
			match target {
				.initial, .operator, .whitespace, .comment { true }
				else { false }
			}
		}
		.string {
			match target {
				.initial, .operator, .whitespace, .comment, .error { true }
				else { false }
			}
		}
		.atom {
			match target {
				.initial, .operator, .whitespace, .comment { true }
				else { false }
			}
		}
		.comment {
			match target {
				.initial, .whitespace { true }
				else { false }
			}
		}
		.directive {
			match target {
				.initial, .whitespace { true }
				else { false }
			}
		}
		.operator {
			match target {
				.initial, .identifier, .number, .string, .atom, .whitespace, .comment { true }
				else { false }
			}
		}
		.whitespace {
			match target {
				.initial, .identifier, .number, .string, .atom, .operator, .comment, .directive {
					true
				}
				else {
					false
				}
			}
		}
		.error {
			match target {
				.initial { true }
				else { false }
			}
		}
		.punctuation {
			match target {
				.initial { true }
				else { false }
			}
		}
		.binary {
			match target {
				.initial { true }
				else { false }
			}
		}
		.hex_number {
			match target {
				.initial { true }
				else { false }
			}
		}
	}
}

// get_default_transition returns the default transition for a state
pub fn (s LexerState) get_default_transition() LexerState {
	return match s {
		.initial { .initial }
		.atom_start { .atom_start }
		.identifier { .initial }
		.key { .initial }
		.number { .initial }
		.float { .initial }
		.string { .initial }
		.atom { .initial }
		.comment { .initial }
		.directive { .initial }
		.operator { .initial }
		.whitespace { .initial }
		.error { .initial }
		.punctuation { .initial }
		.binary { .initial }
		.hex_number { .initial }
	}
}

// is_accepting_state checks if a state can accept more input
pub fn (s LexerState) is_accepting_state() bool {
	return match s {
		.identifier, .key, .number, .float, .string, .atom, .operator, .comment, .directive,
		.atom_start, .binary, .hex_number {
			true
		}
		else {
			false
		}
	}
}

// requires_lookahead checks if a state requires lookahead to determine the next state
pub fn (s LexerState) requires_lookahead() bool {
	return match s {
		.operator, .atom_start { true }
		else { false }
	}
}

// can_transition_from checks if a state can transition from another state
pub fn (s LexerState) can_transition_from(from LexerState) bool {
	return match s {
		.initial {
			true
		}
		.identifier {
			from == .initial || from == .whitespace
		}
		.key {
			from == .identifier
		}
		.number {
			from == .initial || from == .whitespace
		}
		.float {
			from == .number
		}
		.string {
			from == .initial || from == .whitespace
		}
		.atom {
			from == .atom_start
		}
		.atom_start {
			from == .initial || from == .whitespace
		}
		.operator {
			from == .initial || from == .whitespace || from == .atom_start
		}
		.comment {
			from == .initial || from == .whitespace
		}
		.directive {
			from == .initial || from == .whitespace
		}
		.punctuation {
			from == .initial || from == .whitespace
		}
		.whitespace {
			from == .initial || from == .identifier || from == .number || from == .float
				|| from == .string || from == .atom || from == .operator || from == .comment
				|| from == .directive || from == .punctuation || from == .atom_start || from == .binary
				|| from == .hex_number
		}
		.error {
			true
		}
		.binary {
			from == .initial || from == .whitespace
		}
		.hex_number {
			from == .initial || from == .whitespace
		}
	}
}
