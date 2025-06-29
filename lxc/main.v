module main

import os
import lexer
import parser
import generate.erlang
import typechecker

fn main() {
	args := os.args
	if args.len < 2 {
		eprintln('Usage: lxc <input_file>')
		exit(1)
	}

	input_file := args[1]

	if !os.exists(input_file) {
		eprintln('Input file not found: ${input_file}')
		exit(1)
	}

	// Check if file has .lx extension
	if !input_file.ends_with('.lx') {
		eprintln('Input file must have .lx extension: ${input_file}')
		exit(1)
	}

	// Extract module name from file name (remove .lx extension)
	module_name := os.file_name(input_file).replace('.lx', '')

	// Read and parse the file
	content := os.read_file(input_file) or {
		eprintln('Failed to read file: ${err}')
		exit(1)
	}
	// Create lexer and tokenize
	mut lexer_instance := lexer.new_lexer(content, input_file)
	mut tokens := []lexer.Token{}
	for {
		token := lexer_instance.next_token()
		if token is lexer.EOFToken {
			break
		}
		if token is lexer.ErrorToken {
			eprintln('Lexical error: ${token.message}')
			exit(1)
		}
		tokens << token
	}

	if lexer_instance.has_errors() {
		mut errors := []string{}
		for error in lexer_instance.get_errors() {
			errors << error.message
		}
		eprintln('Lexical errors: ${errors.join('\n')}')
		exit(1)
	}

	// Create parser and parse the tokens into AST
	mut parser_instance := parser.new_main_parser(tokens)
	mut module_stmt := parser_instance.parse_module() or {
		eprintln('Parsing failed: ${err}')
		eprintln('Parser errors:')
		for error in parser_instance.get_errors() {
			eprintln('  ${error}')
		}
		exit(1)
	}

	if parser_instance.has_errors() {
		mut errors := []string{}
		for error in parser_instance.get_errors() {
			errors << error.message
		}
		eprintln('Parser errors: ${errors.join('\n')}')
		exit(1)
	}

	// Override the module name with the file name
	module_stmt.name = module_name

	// Generate Erlang code
	erlang_gen := erlang.new_erlang_generator()
	type_ctx := typechecker.new_type_context()

	codegen_result := erlang_gen.generate_module(module_stmt, type_ctx)

	if !codegen_result.success {
		eprintln('Code generation failed')
		for error in codegen_result.errors {
			eprintln('Error: ${error}')
		}
		exit(1)
	}

	// Get the directory of the input file
	input_dir := os.dir(input_file)

	// Generate output filename in the same directory
	output_file := '${input_dir}/${module_name}.erl'

	// Write the Erlang code to file
	os.write_file(output_file, codegen_result.code) or {
		eprintln('Failed to write output file: ${err}')
		exit(1)
	}

	println('Compilation successful!')
	println('Module: ${module_name}')
	println('Generated: ${output_file}')
	println('Statements: ${module_stmt.statements.len}')
}
