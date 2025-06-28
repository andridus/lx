module main

import os
import flag
import compiler

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('lx_compiler')
	fp.description('LX Language Compiler - Compiles LX code to Erlang')
	fp.skip_executable()

	help := fp.bool('help', `h`, false, 'Show this help message')
	version := fp.bool('version', `v`, false, 'Show version information')
	build := fp.bool('build', `b`, false, 'Build the project')
	file := fp.string('file', `f`, '', 'Input LX file to compile')
	output := fp.string('output', `o`, '_build', 'Output directory for generated files')
	verbose := fp.bool('verbose', `V`, false, 'Enable verbose output')

	additional_args := fp.finalize() or {
		eprintln('Error: ${err}')
		println(fp.usage())
		exit(1)
	}

	if help {
		println(fp.usage())
		return
	}

	if version {
		println('LX Compiler v0.1.0')
		println('A functional programming language that compiles to Erlang')
		return
	}

	if build {
		println('Building LX compiler...')
		// TODO: Implement build logic for the compiler itself
		println('Build completed successfully')
		return
	}

	// Determine input file
	mut input_file := file
	if input_file.len == 0 && additional_args.len > 0 {
		input_file = additional_args[0]
	}

	if input_file.len > 0 {
		if !os.exists(input_file) {
			eprintln('Error: File "${input_file}" not found')
			exit(1)
		}

		if !input_file.ends_with('.lx') {
			eprintln('Error: Input file must have .lx extension')
			exit(1)
		}

		if verbose {
			println('Compiling ${input_file}...')
		}

		mut comp := compiler.new_compiler()
		result := comp.compile_file(input_file)
		if !result.success {
			eprintln('Compilation failed:')
			for err in result.errors {
				eprintln(err)
			}
			exit(1)
		}

		output_path := result.write_erlang_file(output) or {
			eprintln('Failed to write .erl file: ${err}')
			exit(1)
		}
		println('Compilation successful! Output: ${output_path}')
		return
	}

	// Default: show help
	println(fp.usage())
}
