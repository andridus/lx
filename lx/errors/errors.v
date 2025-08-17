module errors

import ast

// ErrorKind defines the type of error.
pub enum ErrorKind {
	lexical
	parser
	analysis
	generation
}

// Err struct to hold error information.
pub struct Err {
pub:
	kind       ErrorKind
	message    string
	position   ast.Position
	suggestion string
}

// ErrorReporter collects and reports errors.
pub struct ErrorReporter {
mut:
	errors []Err
}

// new_error_reporter creates a new ErrorReporter.
pub fn new_error_reporter() ErrorReporter {
	return ErrorReporter{
		errors: []
	}
}

// report adds a new error to the reporter.
pub fn (mut er ErrorReporter) report(kind ErrorKind, message string, position ast.Position) {
	err_obj := Err{
		kind:     kind
		message:  message
		position: position
	}
	er.errors << err_obj
}

// report_with_suggestion adds a new error with a suggestion.
pub fn (mut er ErrorReporter) report_with_suggestion(kind ErrorKind, message string, position ast.Position, suggestion string) {
	err_obj := Err{
		kind:       kind
		message:    message
		position:   position
		suggestion: suggestion
	}
	er.errors << err_obj
}

// has_errors checks if there are any errors.
pub fn (er &ErrorReporter) has_errors() bool {
	return er.errors.len > 0
}

// all returns all collected errors.
pub fn (er &ErrorReporter) all() []Err {
	return er.errors
}

// format_all formats all collected errors into a single string.
pub fn (er &ErrorReporter) format_all() string {
	mut out := []string{}
	for err in er.errors {
		out << format_error(err)
	}
	return out.join('\n')
}

// ===================
// Error Formatting
// ===================

// format_error formats a single error object.
pub fn format_error(err Err) string {
	pos := if err.position.line > 0 && err.position.column > 0 {
		'${err.position.file}:${err.position.line}:${err.position.column}'
	} else {
		''
	}
	return '[${err.kind.str()}] ${pos} ${err.message}'
}

// format_error_detailed formats a single error with detailed context.
pub fn format_error_detailed(err Err, source_lines []string) string {
	mut out := ''
	out += red() +
		'[${err.kind.str()} Error] ${err.position.file}:${err.position.line}:${err.position.column}' +
		reset() + '\n'
	out += red() + err.message + reset() + '\n'
	if err.position.line > 0 && source_lines.len > 0 {
		mut start := err.position.line - 3
		if start < 1 {
			start = 1
		}
		mut end_ := err.position.line + 3
		if end_ > source_lines.len {
			end_ = source_lines.len
		}
		out += '\n'
		for i := start; i <= end_; i++ {
			line_content := if i <= source_lines.len { source_lines[i - 1] } else { '' }
			lnum := '${i:4}'
			if i == err.position.line {
				out += red() + '${lnum} | ' + reset() + line_content + '\n'
				out += '     | ' + red() + ' '.repeat(err.position.column - 1) + '^~~' + reset() +
					'\n'
			} else {
				out += '${lnum} | ' + line_content + '\n'
			}
		}
	}
	if err.suggestion.len > 0 {
		out += '\n' + white() + 'Suggestion:\n   ' + err.suggestion + reset() + '\n'
	}
	return out
}

// =====================
// Formatting Colors
// =====================

fn red() string {
	return '\x1b[31m'
}

fn white() string {
	return '\x1b[37m'
}

fn reset() string {
	return '\x1b[0m'
}

// =====================
// Enum String
// =====================

// str returns a string representation of the ErrorKind.
pub fn (k ErrorKind) str() string {
	return match k {
		.lexical { 'Lexical' }
		.parser { 'Parser' }
		.analysis { 'Analysis' }
		.generation { 'Generation' }
	}
}
