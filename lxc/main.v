module main

import os
import compiler

fn main() {
	args := os.args
	if args.len < 2 {
		eprintln('Usage: lxc <input_file> [--debug-tokens] [--debug-types]')
		eprintln('  --debug-tokens: Show token debug information')
		eprintln('  --debug-types: Show type analysis debug information')
		exit(1)
	}

	input_file := args[1]
	mut debug_tokens := false
	mut debug_types := false

	// Parse command line flags
	for i in 2 .. args.len {
		match args[i] {
			'--debug-tokens' {
				debug_tokens = true
			}
			'--debug-types' {
				debug_types = true
			}
			else {
				eprintln('Unknown flag: ${args[i]}')
				exit(1)
			}
		}
	}

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
		comp.enable_debug_tokens()
	}
	if debug_types {
		comp.enable_debug_types()
	}
	comp.compile_file(input_file)
}
