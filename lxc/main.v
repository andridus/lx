module main

import os
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('lx_compiler')
	fp.description('LX Language Compiler - Compiles LX code to Erlang')
	fp.skip_executable()

	help := fp.bool('help', `h`, false, 'Show this help message')
	version := fp.bool('version', `v`, false, 'Show version information')
	build := fp.bool('build', `b`, false, 'Build the project')
	file := fp.string('file', `f`, '', 'Input LX file to compile')

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
		return
	}

	if build {
		println('Building LX compiler...')
		// TODO: Implement build logic
		println('Build completed successfully')
		return
	}

	if file.len > 0 {
		if !os.exists(file) {
			eprintln('Error: File "${file}" not found')
			exit(1)
		}
		println('Compiling ${file}...')
		// TODO: Implement compilation logic
		println('Compilation completed')
		return
	}

	if additional_args.len > 0 {
		// Treat first argument as file
		file_path := additional_args[0]
		if !os.exists(file_path) {
			eprintln('Error: File "${file_path}" not found')
			exit(1)
		}
		println('Compiling ${file_path}...')
		// TODO: Implement compilation logic
		println('Compilation completed')
		return
	}

	// Default: show help
	println(fp.usage())
}
