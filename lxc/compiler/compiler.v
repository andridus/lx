module compiler

import frontend.lexer
import frontend.parser
import os
import ast
import errors
import analysis1
import backend.codegen
import backend.erlang

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
	file_path    string
	module_name  string
	debug_tokens bool
}

// new_compiler creates a new compiler instance
pub fn new_compiler() Compiler {
	return Compiler{
		module_name:  ''
		file_path:    ''
		debug_tokens: false
	}
}

// compile_file compiles a single file
pub fn (mut comp Compiler) compile_file(file_path string) {
	module_name := os.file_name(file_path).replace('.lx', '')
	comp.file_path = file_path
	// Read the source file
	source := os.read_file(file_path) or {
		println('Failed to read file: ${file_path}: ${err}')
		exit(1)
	}
	result := comp.compile(source, file_path)

	// Create output directory if it doesn't exist
	os.mkdir_all(os.dir(file_path)) or {
		println('Failed to create output directory: ${err}')
		exit(1)
	}

	// Generate output filename in the same directory
	output_file := '${os.dir(file_path)}/${module_name}.erl'

	// Write the Erlang code to file
	os.write_file(output_file, result.code) or {
		println('Failed to write output file: ${err}')
		exit(1)
	}
}

pub fn (mut comp Compiler) compile(source string, file_path string) codegen.CodegenResult {
	mut error_formatter := errors.new_error_formatter()
	// Create lexer and tokenize
	mut lexer_instance := lexer.new_lexer(source, file_path)
	mut tokens := []lexer.Token{}
	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			println('Lexical error: ${token.message}')
			exit(1)
		}
		tokens << token
	}

	if comp.debug_tokens {
		println('=== TOKENS ===')
		for i, token in tokens {
			println('${i}: ${token.str()} @ ${token.get_position().str()}')
		}
		println('=== END TOKENS ===')
	}
	if lexer_instance.has_errors() {
		// Format lexer errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in lexer_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Lexical errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Create parser and parse the tokens into AST
	mut parser1_instance := parser.new_parser(tokens)
	module_stmt0 := parser1_instance.parse_program() or { ast.ModuleStmt{} }

	if parser1_instance.has_errors() {
		// Format parser errors properly
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for error in parser1_instance.get_errors() {
			formatted_errors << error_formatter.format_error(error, source_lines)
		}
		println('Parser errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	// Use the new refactored analysis module
	mut analyzer := analysis1.new_analyzer()
	analysis_result := analyzer.analyze_module(module_stmt0)

	if analysis_result.errors.len > 0 {
		source_lines := errors.load_source_lines(file_path)
		mut formatted_errors := []string{}
		for err in analysis_result.errors {
			formatted_errors << error_formatter.format_error(err, source_lines)
		}
		println('Analysis errors:\n${formatted_errors.join('\n')}')
		exit(1)
	}

	mut final_module_stmt := module_stmt0
	module_name := os.file_name(file_path).replace('.lx', '')
	if final_module_stmt.name == 'main' {
		final_module_stmt.name = module_name
	}

	mut erlang_gen := erlang.new_erlang_generator()
	codegen_result := erlang_gen.generate_module(final_module_stmt, analysis_result.type_context)

	if !codegen_result.success {
		eprintln('Code generation failed')
		for error in codegen_result.errors {
			eprintln('Error: ${error}')
		}
		exit(1)
	}
	println('Compiled ${file_path} successfully')
	return codegen_result
}
