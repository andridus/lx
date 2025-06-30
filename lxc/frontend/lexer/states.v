module lexer

// LexerState represents the current state of the lexer
pub enum LexerState {
	initial
	atom_start
	identifier
	number
	float
	string
	atom
	comment
	operator
	whitespace
	error
	punctuation
}

// str returns a string representation of LexerState
pub fn (s LexerState) str() string {
	return match s {
		.initial { 'initial' }
		.atom_start { 'atom_start' }
		.identifier { 'identifier' }
		.number { 'number' }
		.float { 'float' }
		.string { 'string' }
		.atom { 'atom' }
		.comment { 'comment' }
		.operator { 'operator' }
		.whitespace { 'whitespace' }
		.error { 'error' }
		.punctuation { 'punctuation' }
	}
}

// is_final_state checks if a state is a final state (can emit a token)
pub fn (s LexerState) is_final_state() bool {
	return match s {
		.identifier, .number, .float, .string, .atom, .error, .punctuation { true }
		else { false }
	}
}

// can_transition_to checks if a transition from current state to target state is valid
pub fn (current LexerState) can_transition_to(target LexerState) bool {
	return match current {
		.initial {
			match target {
				.identifier, .number, .string, .atom, .comment, .operator, .whitespace {
					true
				}
				else {
					false
				}
			}
		}
		.atom_start {
			match target {
				.identifier, .number, .string, .atom, .comment, .operator, .whitespace {
					true
				}
				else {
					false
				}
			}
		}
		.identifier {
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
		.operator {
			match target {
				.initial, .identifier, .number, .string, .atom, .whitespace, .comment { true }
				else { false }
			}
		}
		.whitespace {
			match target {
				.initial, .identifier, .number, .string, .atom, .operator, .comment {
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
	}
}

// get_default_transition returns the default transition for a state
pub fn (s LexerState) get_default_transition() LexerState {
	return match s {
		.initial { .initial }
		.atom_start { .atom_start }
		.identifier { .initial }
		.number { .initial }
		.float { .initial }
		.string { .initial }
		.atom { .initial }
		.comment { .initial }
		.operator { .initial }
		.whitespace { .initial }
		.error { .initial }
		.punctuation { .initial }
	}
}

// is_accepting_state checks if a state can accept more input
pub fn (s LexerState) is_accepting_state() bool {
	return match s {
		.identifier, .number, .float, .string, .atom, .operator, .comment, .atom_start { true }
		else { false }
	}
}

// requires_lookahead checks if a state requires lookahead to determine the next state
pub fn (s LexerState) requires_lookahead() bool {
	return match s {
		.operator, .atom_start { true }
		else { false }
	}
}
