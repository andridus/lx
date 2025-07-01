module compiler

import frontend.lexer
import frontend.parser
import ast
import os
import errors
import analysis

// CompilerResult represents the result of compilation
pub struct CompilerResult {
pub:
	success     bool
	erlang_code string
	module_name string
	file_path   string
	errors      []string
	warnings    []string
}

// Compiler represents the main compiler for LX language
pub struct Compiler {
pub mut:
	file_path       string
	module_name     string
	lexer_instance  lexer.Lexer
	parser_instance parser.MainParser
	error_formatter errors.ErrorFormatter
	debug_tokens    bool
}

// new_compiler creates a new compiler instance
pub fn new_compiler() Compiler {
	return Compiler{
		module_name:     ''
		file_path:       ''
		error_formatter: errors.new_error_formatter()
		debug_tokens:    false
	}
}

// compile_file compiles a single file
pub fn (mut comp Compiler) compile_file(file_path string) !ast.ModuleStmt {
	comp.file_path = file_path

	// Read the source file
	source := os.read_file(file_path) or {
		return error('Failed to read file: ${file_path}: ${err}')
	}

	// Create lexer and tokenize
	comp.lexer_instance = lexer.new_lexer(source, file_path)
	mut tokens := []lexer.Token{}
	for {
		token := comp.lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			return error('Lexical error: ${token.message}')
		}
		tokens << token
	}

	// Debug: Show tokens if requested
	if comp.debug_tokens {
		println('=== TOKENS ===')
		for i, token in tokens {
			println('${i}: ${token.str()} @ ${token.get_position().str()}')
		}
		println('=== END TOKENS ===')
	}

	if comp.lexer_instance.has_errors() {
		// Format lexer errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error('Lexical errors:\n${formatted_errors.join('\n')}')
	}

	// Create parser and parse the tokens into AST
	comp.parser_instance = parser.new_main_parser(tokens)
	module_stmt := comp.parser_instance.parse_module() or {
		// Format parser errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in comp.parser_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error(formatted_errors.join('\n'))
	}

	if comp.parser_instance.has_errors() {
		// Format parser errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in comp.parser_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error('Parser errors:\n${formatted_errors.join('\n')}')
	}

	// Variable scope checking (after parsing, before typechecking)
	mut var_checker := analysis.new_variable_checker()
	result := var_checker.check_module(module_stmt)
	if result.errors.len > 0 {
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for err in result.errors {
			formatted_errors << comp.error_formatter.format_error(err, source_lines)
		}
		return error('Variable scope errors:\n' + formatted_errors.join('\n'))
	}

	println('Compiled ${file_path} successfully')
	return module_stmt
}

// compile_string compiles a string of source code
pub fn (mut comp Compiler) compile_string(source string) !ast.ModuleStmt {
	// Create lexer and tokenize
	comp.lexer_instance = lexer.new_lexer(source, 'string')
	mut tokens := []lexer.Token{}
	for {
		token := comp.lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			return error('Lexical error: ${token.message}')
		}
		tokens << token
	}

	if comp.lexer_instance.has_errors() {
		// Format lexer errors properly for string compilation
		source_lines := source.split('\n')
		mut formatted_errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error('Lexical errors:\n${formatted_errors.join('\n')}')
	}

	// Create parser and parse the tokens into AST
	comp.parser_instance = parser.new_main_parser(tokens)
	module_stmt := comp.parser_instance.parse_module() or {
		// Format parser errors properly for string compilation
		source_lines := source.split('\n')
		mut formatted_errors := []string{}
		for error in comp.parser_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error(formatted_errors.join('\n'))
	}

	if comp.parser_instance.has_errors() {
		// Format parser errors properly for string compilation
		source_lines := source.split('\n')
		mut formatted_errors := []string{}
		for error in comp.parser_instance.get_errors() {
			formatted_errors << comp.error_formatter.format_error(error, source_lines)
		}
		return error('Parser errors:\n${formatted_errors.join('\n')}')
	}

	println('Compiled string successfully')
	return module_stmt
}

// new_compiler_result creates a new compiler result
fn (comp Compiler) new_compiler_result(success bool, erlang_code string, error_list []string, warnings []string) CompilerResult {
	return CompilerResult{
		success:     success
		erlang_code: erlang_code
		module_name: comp.module_name
		file_path:   comp.file_path
		errors:      error_list
		warnings:    warnings
	}
}

// write_erlang_file writes the generated Erlang code to a file
pub fn (result CompilerResult) write_erlang_file(output_dir string) !string {
	if !result.success {
		return error('Compilation failed')
	}

	// Create output directory if it doesn't exist
	os.mkdir_all(output_dir) or { return error('Failed to create output directory: ${err}') }

	// Generate output filename
	output_file := '${output_dir}/${result.module_name}.erl'

	// Write the Erlang code to file
	os.write_file(output_file, result.erlang_code) or {
		return error('Failed to write output file: ${err}')
	}

	return output_file
}
