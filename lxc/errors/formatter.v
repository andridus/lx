module errors

import os

// ErrorFormatter handles formatting of compilation errors
pub struct ErrorFormatter {
pub:
	show_source      bool
	show_suggestions bool
	colored_output   bool
	max_line_length  int
}

// new_error_formatter creates a new ErrorFormatter
pub fn new_error_formatter() ErrorFormatter {
	return ErrorFormatter{
		show_source:      true
		show_suggestions: true
		colored_output:   true
		max_line_length:  80
	}
}

// format_error formats a single compilation error
pub fn (ef ErrorFormatter) format_error(err CompilationError, source_lines []string) string {
	mut result := ''

	// Format header
	result += ef.format_header(err)

	// Format message
	result += ef.format_message(err)

	// Format source context
	result += ef.format_source_context(err, source_lines)

	// Format suggestion
	result += ef.format_suggestion(err)

	return result
}

// format_header formats the error header with position and severity
fn (ef ErrorFormatter) format_header(err CompilationError) string {
	severity_color := ef.get_severity_color(err.severity)
	position_str := err.position.str()
	severity_str := err.severity.str()

	if ef.colored_output {
		return '${severity_color}${severity_str}${ef.reset_color()} at ${position_str}:\n'
	} else {
		return '${severity_str} at ${position_str}:\n'
	}
}

// format_message formats the error message
fn (ef ErrorFormatter) format_message(err CompilationError) string {
	message_color := ef.get_message_color(err.severity)
	message := err.message

	if ef.colored_output {
		return '${message_color}${message}${ef.reset_color()}\n'
	} else {
		return '${message}\n'
	}
}

// format_source_context formats the source code context around the error
fn (ef ErrorFormatter) format_source_context(err CompilationError, source_lines []string) string {
	if !err.position.is_valid() || source_lines.len == 0 {
		return ''
	}

	mut result := '\n'
	line_num := err.position.line
	column := err.position.column

	// Show 3 lines before and after the error line
	start_line := if line_num - 3 > 0 { line_num - 3 } else { 1 }
	end_line := if line_num + 3 < source_lines.len { line_num + 3 } else { source_lines.len }

	for i := start_line; i <= end_line; i++ {
		line_content := if i <= source_lines.len { source_lines[i - 1] } else { '' }

		// Line number
		line_num_str := '${i:4}'
		if i == line_num {
			result += ef.get_error_color() + '${line_num_str} | ' + ef.reset_color()
		} else {
			result += '${line_num_str} | '
		}

		// Line content
		result += line_content + '\n'

		// Error indicator
		if i == line_num {
			result += ef.format_error_indicator(column, line_content.len)
		}
	}

	return result
}

// format_error_indicator formats the error indicator line
fn (ef ErrorFormatter) format_error_indicator(column int, line_length int) string {
	mut result := '     | '

	// Add spaces before the error position
	for i := 1; i < column; i++ {
		result += ' '
	}

	// Add the error indicator
	result += ef.get_error_color() + '^' + ef.reset_color()

	// Add more indicators if needed
	for i := column + 1; i <= line_length && i <= column + 2; i++ {
		result += ef.get_error_color() + '~' + ef.reset_color()
	}

	result += '\n'
	return result
}

// format_suggestion formats error suggestions
fn (ef ErrorFormatter) format_suggestion(err CompilationError) string {
	// For now, return empty string since we don't have get_suggestion method
	return ''
}

// format_error_collection formats a collection of errors
pub fn (ef ErrorFormatter) format_error_collection(collection ErrorCollection, source_lines []string) string {
	mut result := ''

	// Format errors
	for err in collection.errors {
		result += ef.format_error(err, source_lines)
		result += '\n'
	}

	// Format warnings
	for warning in collection.warnings {
		result += ef.format_error(warning, source_lines)
		result += '\n'
	}

	// Summary
	result += ef.format_summary(collection)

	return result
}

// format_summary formats a summary of the error collection
fn (ef ErrorFormatter) format_summary(collection ErrorCollection) string {
	error_count := collection.errors.len
	warning_count := collection.warnings.len

	if error_count == 0 && warning_count == 0 {
		return ''
	}

	mut summary := '\nSummary:\n'

	if error_count > 0 {
		summary += '  ${error_count} error(s)\n'
	}

	if warning_count > 0 {
		summary += '  ${warning_count} warning(s)\n'
	}

	return summary
}

// Color methods for terminal output
fn (ef ErrorFormatter) get_severity_color(severity ErrorSeverity) string {
	if !ef.colored_output {
		return ''
	}

	return match severity {
		.info { '\x1b[36m' } // Cyan
		.warning { '\x1b[33m' } // Yellow
		.error { '\x1b[31m' } // Red
		.fatal { '\x1b[35m' } // Magenta
	}
}

fn (ef ErrorFormatter) get_message_color(severity ErrorSeverity) string {
	if !ef.colored_output {
		return ''
	}

	return match severity {
		.info { '\x1b[36m' } // Cyan
		.warning { '\x1b[33m' } // Yellow
		.error { '\x1b[31m' } // Red
		.fatal { '\x1b[35m' } // Magenta
	}
}

fn (ef ErrorFormatter) get_error_color() string {
	if !ef.colored_output {
		return ''
	}
	return '\x1b[31m' // Red
}

fn (ef ErrorFormatter) get_suggestion_color() string {
	if !ef.colored_output {
		return ''
	}
	return '\x1b[32m' // Green
}

fn (ef ErrorFormatter) reset_color() string {
	if !ef.colored_output {
		return ''
	}
	return '\x1b[0m'
}

// load_source_lines loads source lines from a file
pub fn load_source_lines(filename string) []string {
	if !os.exists(filename) {
		return []
	}

	content := os.read_file(filename) or { return [] }
	return content.split('\n')
}

// format_error_simple formats an error without source context
pub fn format_error_simple(err CompilationError) string {
	position_str := err.position.str()
	severity_str := err.severity.str()

	return '${severity_str} at ${position_str}: ${err.message}'
}

// get_color_code returns the ANSI color code for a severity level
fn get_color_code(severity ErrorSeverity) string {
	return match severity {
		.info { '\x1b[36m' } // Cyan
		.warning { '\x1b[33m' } // Yellow
		.error { '\x1b[31m' } // Red
		.fatal { '\x1b[35m' } // Magenta
	}
}
