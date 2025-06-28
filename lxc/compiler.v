module compiler

import lexer
import parser
import generate.erlang
import typechecker
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
	lexer_instance  &lexer.Lexer = unsafe { nil }
	parser_instance &parser.Parser = unsafe { nil }
	generator       &erlang.ErlangGenerator = unsafe { nil }
}

// new_compiler creates a new compiler instance
pub fn new_compiler() Compiler {
	return Compiler{
		module_name:     ''
		file_path:       ''
		lexer_instance:  unsafe { nil }
		parser_instance: unsafe { nil }
		generator:       &erlang.ErlangGenerator{}
	}
}

// compile_file compiles a single LX file to Erlang
pub fn (mut comp Compiler) compile_file(file_path string) CompilerResult {
	// Read source file
	source := os.read_file(file_path) or {
		return comp.new_compiler_result(false, '', ['Failed to read file: ${err}'], [])
	}
	// Extract module name from filename
	comp.module_name = os.file_name(file_path).replace('.lx', '')
	comp.file_path = file_path
	lexer_instance := lexer.new_lexer(source, file_path)
	comp.lexer_instance = &lexer_instance

	mut tokens := []lexer.Token{}
	for {
		token := comp.lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			return comp.new_compiler_result(false, '', [token.message], [])
		}
		tokens << token
	}

	if comp.lexer_instance.has_errors() {
		mut errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	// Create parser
	comp.parser_instance = parser.new_parser(tokens)
	// Parse the tokens into AST
	statements := comp.parser_instance.parse()
	println('statements: ${statements}')
	if comp.parser_instance.has_errors() {
		mut errors := []string{}
		for error in comp.parser_instance.get_errors() {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	// Create a simple module structure for now
	module_stmt := ast.ModuleStmt{
		name:       comp.module_name
		exports:    []
		imports:    []
		statements: statements
		position:   ast.new_position(1, 1, comp.file_path)
	}

	// Create type context (for now, empty)
	type_ctx := typechecker.new_context()

	// Generate Erlang code
	codegen_result := comp.generator.generate_module(module_stmt, type_ctx)
	if !codegen_result.success {
		mut errors := []string{}
		for error in codegen_result.errors {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	return comp.new_compiler_result(true, codegen_result.code, [], [])
}

// compile_string compiles LX source code from a string
pub fn (mut comp Compiler) compile_string(source string, module_name string) CompilerResult {
	// Create lexer
	comp.lexer_instance = &lexer.Lexer{}
	unsafe {
		*comp.lexer_instance = lexer.new_lexer(source, 'string')
	}

	// Tokenize the source
	mut tokens := []lexer.Token{}
	for {
		token := comp.lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			return comp.new_compiler_result(false, '', [token.message], [])
		}
		tokens << token
	}

	if comp.lexer_instance.has_errors() {
		mut errors := []string{}
		for error in comp.lexer_instance.get_errors() {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	// Create parser
	comp.parser_instance = parser.new_parser(tokens)

	println('parser: ${comp.parser_instance}')
	// Parse the tokens into AST
	statements := comp.parser_instance.parse()
	if comp.parser_instance.has_errors() {
		mut errors := []string{}
		for error in comp.parser_instance.get_errors() {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	// Create a simple module structure for now
	module_stmt := ast.ModuleStmt{
		name:       module_name
		exports:    []
		imports:    []
		statements: statements
		position:   ast.new_position(1, 1, 'string')
	}

	// Create type context (for now, empty)
	type_ctx := typechecker.new_context()

	// Generate Erlang code
	codegen_result := comp.generator.generate_module(module_stmt, type_ctx)
	if !codegen_result.success {
		mut errors := []string{}
		for error in codegen_result.errors {
			errors << error.message
		}
		return comp.new_compiler_result(false, '', errors, [])
	}

	return comp.new_compiler_result(true, codegen_result.code, [], [])
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