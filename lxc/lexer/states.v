module lexer

// LexerState represents the current state of the lexer
pub enum LexerState {
	initial
	identifier
	number
	float
	string
	atom
	comment
	operator
	whitespace
	error
	maybe_negative
}

// str returns a string representation of LexerState
pub fn (s LexerState) str() string {
	return match s {
		.initial { 'initial' }
		.identifier { 'identifier' }
		.number { 'number' }
		.float { 'float' }
		.string { 'string' }
		.atom { 'atom' }
		.comment { 'comment' }
		.operator { 'operator' }
		.whitespace { 'whitespace' }
		.error { 'error' }
		.maybe_negative { 'maybe_negative' }
	}
}

// is_final_state checks if a state is a final state (can emit a token)
pub fn (s LexerState) is_final_state() bool {
	return match s {
		.identifier, .number, .float, .string, .atom, .operator, .error, .maybe_negative { true }
		else { false }
	}
}

// can_transition_to checks if a transition from current state to target state is valid
pub fn (current LexerState) can_transition_to(target LexerState) bool {
	return match current {
		.initial {
			match target {
				.identifier, .number, .string, .atom, .comment, .operator, .whitespace,
				.maybe_negative {
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
				.initial, .identifier, .number, .string, .atom, .operator, .comment,
				.maybe_negative {
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
		.maybe_negative {
			match target {
				.number, .operator, .initial { true }
				else { false }
			}
		}
	}
}

// get_default_transition returns the default transition for a state
pub fn (s LexerState) get_default_transition() LexerState {
	return match s {
		.initial { .initial }
		.identifier { .initial }
		.number { .initial }
		.float { .initial }
		.string { .initial }
		.atom { .initial }
		.comment { .initial }
		.operator { .initial }
		.whitespace { .initial }
		.error { .initial }
		.maybe_negative { .initial }
	}
}

// is_accepting_state checks if a state can accept more input
pub fn (s LexerState) is_accepting_state() bool {
	return match s {
		.identifier, .number, .float, .string, .atom, .operator, .comment { true }
		else { false }
	}
}

// requires_lookahead checks if a state requires lookahead to determine the next state
pub fn (s LexerState) requires_lookahead() bool {
	return match s {
		.operator { true }
		else { false }
	}
}
