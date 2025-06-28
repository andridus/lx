module compiler

import lexer
import parser
import ast
import os

// CompilerResult represents the result of compilation
pub struct CompilerResult {
pub:
	success      bool
	erlang_code  string
	module_name  string
	file_path    string
	errors       []string
	warnings     []string
}

// Compiler represents the main compiler for LX language
pub struct Compiler {
mut:
	file_path   string
	module_name string
	lexer_instance  lexer.Lexer
	parser_instance parser.MainParser
}

// new_compiler creates a new compiler instance
pub fn new_compiler() Compiler {
	return Compiler{
		module_name:     ''
		file_path:       ''
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

	if comp.lexer_instance.has_errors() {
		mut errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			errors << error.message
		}
		return error('Lexical errors: ${errors.join('\n')}')
	}

	// Create parser and parse the tokens into AST
	comp.parser_instance = parser.new_main_parser(tokens)
	module_stmt := comp.parser_instance.parse_module() or {
		return error('Parsing failed: ${err}')
	}

	if comp.parser_instance.has_errors() {
		mut errors := []string{}
		for error in comp.parser_instance.get_errors() {
			errors << error.message
		}
		return error('Parser errors: ${errors.join('\n')}')
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
		mut errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			errors << error.message
		}
		return error('Lexical errors: ${errors.join('\n')}')
	}

	// Create parser and parse the tokens into AST
	comp.parser_instance = parser.new_main_parser(tokens)
	module_stmt := comp.parser_instance.parse_module() or {
		return error('Parsing failed: ${err}')
	}

	if comp.parser_instance.has_errors() {
		mut errors := []string{}
		for error in comp.parser_instance.get_errors() {
			errors << error.message
		}
		return error('Parser errors: ${errors.join('\n')}')
	}

	println('Compiled string successfully')
	return module_stmt
}

// new_compiler_result creates a new compiler result
fn (comp Compiler) new_compiler_result(success bool, erlang_code string, errors []string, warnings []string) CompilerResult {
	return CompilerResult{
		success:     success
		erlang_code: erlang_code
		module_name: comp.module_name
		file_path:   comp.file_path
		errors:      errors
		warnings:    warnings
	}
}

// write_erlang_file writes the generated Erlang code to a file
pub fn (result CompilerResult) write_erlang_file(output_dir string) !string {
	if !result.success {
		return error('Compilation failed')
	}

	// Create output directory if it doesn't exist
	os.mkdir_all(output_dir) or {
		return error('Failed to create output directory: ${err}')
	}

	// Generate output filename
	output_file := '${output_dir}/${result.module_name}.erl'

	// Write the Erlang code to file
	os.write_file(output_file, result.erlang_code) or {
		return error('Failed to write output file: ${err}')
	}

	return output_file
}