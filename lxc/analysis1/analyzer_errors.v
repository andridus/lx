module analysis1

import ast
import errors

pub enum ErrorKind {
	type_error
	variable_error
	lint_error
	directive_error
}

pub struct AnalysisError {
pub:
	kind       ErrorKind
	message    string
	position   ast.Position
	suggestion string
}

pub fn new_analysis_error(kind ErrorKind, message string, position ast.Position, suggestion string) AnalysisError {
	return AnalysisError{
		kind:       kind
		message:    message
		position:   position
		suggestion: suggestion
	}
}

pub fn (ae AnalysisError) to_compilation_error() errors.CompilationError {
	return match ae.kind {
		.type_error {
			errors.new_compilation_error(errors.TypeError{
				message:    ae.message
				suggestion: ae.suggestion
			}, ae.position, ae.message)
		}
		.variable_error {
			errors.new_compilation_error(errors.UnboundVariableError{
				variable:   ''
				similar:    []
				suggestion: ae.suggestion
			}, ae.position, ae.message)
		}
		.lint_error {
			errors.new_compilation_error(errors.SyntaxError{
				message:  ae.message
				expected: ae.suggestion
				found:    ''
			}, ae.position, ae.message)
		}
		.directive_error {
			errors.new_compilation_error(errors.SyntaxError{
				message:  ae.message
				expected: ae.suggestion
				found:    ''
			}, ae.position, ae.message)
		}
	}
}

pub struct ErrorReporter {
pub mut:
	errors []AnalysisError
}

pub fn new_error_reporter() ErrorReporter {
	return ErrorReporter{
		errors: []
	}
}

pub fn (mut er ErrorReporter) report_error(kind ErrorKind, message string, position ast.Position, suggestion string) {
	error := new_analysis_error(kind, message, position, suggestion)
	er.errors << error
}

pub fn (er &ErrorReporter) get_compilation_errors() []errors.CompilationError {
	return er.errors.map(it.to_compilation_error())
}

pub fn (er &ErrorReporter) get_errors_by_kind(kind ErrorKind) []AnalysisError {
	return er.errors.filter(it.kind == kind)
}
