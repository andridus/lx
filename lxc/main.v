module main

import os
import compiler

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
	// Use the compiler module to compile the file
	mut comp := compiler.new_compiler()
	if debug_tokens {
		comp.debug_tokens = true
	}
	comp.compile_file(input_file)
}
