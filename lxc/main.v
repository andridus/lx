module main

import os
import compiler
import backend.erlang
import analysis.linter
import ast

fn main() {
	args := os.args
	if args.len < 2 {
		eprintln('Usage: lxc <input_file> [--debug-tokens]')
		exit(1)
	}

	input_file := args[1]
	debug_tokens := args.len > 2 && args[2] == '--debug-tokens'

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

	// Use the compiler module to compile the file
	mut comp := compiler.new_compiler()
	if debug_tokens {
		comp.debug_tokens = true
	}
	compile_result := comp.compile_file(input_file) or {
		eprintln(err)
		exit(1)
	}
	mut module_stmt := compile_result.module_stmt

	// Override the module name with the file name
	module_stmt = ast.ModuleStmt{
		...module_stmt
		name: module_name
	}

	// Run linter to check code quality
	linter_instance := linter.new_linter()
	linter_result := linter_instance.lint_module(module_stmt)

	if !linter_result.success {
		eprintln('Linter errors:')
		for error in linter_result.errors {
			eprintln('  ${error.message}')
		}
		exit(1)
	}

	// Generate Erlang code
	mut erlang_gen := erlang.new_erlang_generator()
	codegen_result := erlang_gen.generate_module(module_stmt, compile_result.type_context)

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
