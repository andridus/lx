module main

import os
import compile

fn main() {
	if os.args.len < 2 {
		eprintln('Usage: lx1 <file.lx>')
		eprintln('       lx1 run <file.lx>')
		eprintln('       lx1 --version')
		eprintln('       lx1 --help')
		exit(1)
	}

	arg := os.args[1]
	show_type_table := os.args.contains('--type-table')
	show_nodes := os.args.contains('--nodes')

	match arg {
		'--version' {
			println('LX1 Compiler v0.1.0 - Task 1: Functions with Literals')
			exit(0)
		}
		'--help' {
			print_help()
			exit(0)
		}
		'run' {
			if os.args.len < 3 {
				eprintln('Usage: lx1 run <file.lx>')
				exit(1)
			}
			file := os.args[2]
			module_name := os.file_name(file).all_before_last('.')
			dir := os.dir(file)
			compile.compile_file(file, false, false)
			original_dir := os.getwd()
			os.chdir(dir) or {
				eprintln('Failed to change directory: ${err}')
				exit(1)
			}
			erl_file := module_name + '.erl'
			res := os.execute('erlc ' + erl_file)
			if res.exit_code != 0 {
				eprintln('erlc failed: ' + res.output)
				os.chdir(original_dir) or {}
				exit(1)
			}
			run_res := os.execute('erl -noshell -eval \'io:format("~p\\n", [' + module_name +
				":main()]).' -s init stop")
			if run_res.exit_code != 0 {
				eprintln('erl failed: ' + run_res.output)
				os.chdir(original_dir) or {}
				exit(1)
			}
			print(run_res.output)
			os.rm(erl_file) or {}
			os.rm(module_name + '.beam') or {}
			os.chdir(original_dir) or {}
			return
		}
		else {
			compile.compile_file(arg, show_type_table, show_nodes)
		}
	}
}

fn print_help() {
	println('LX1 Compiler - Task 1: Functions with Literals')
	println('')
	println('USAGE:')
	println('    lx1 <file.lx>        Compile LX file to Erlang')
	println('    lx1 run <file.lx>    Compile and run LX file with Erlang')
	println('    lx1 --version        Show version information')
	println('    lx1 --help           Show this help message')
	println('')
	println('SUPPORTED SYNTAX (Task 1):')
	println('    def function_name() do')
	println('        literal_value')
	println('    end')
	println('')
	println('SUPPORTED LITERALS:')
	println('    - Integers: 42, -10, 0')
	println('    - Floats: 3.14, -2.5, 0.0')
	println('    - Strings: "Hello, World!"')
	println('    - Booleans: true, false')
	println('    - Atoms: :ok, :error, :success')
	println('    - Nil: nil')
	println('')
	println('EXAMPLES:')
	println('    def answer() do')
	println('        42')
	println('    end')
	println('')
	println('    def greeting() do')
	println('        "Hello, World!"')
	println('    end')
}
