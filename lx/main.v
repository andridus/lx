module main

import os
import lx_cli

fn main() {
	args := os.args
	if args.len < 2 {
		eprintln('Usage: lx <command> [options]')
		eprintln('Commands:')
		eprintln('  compile <input_file_or_project> [--debug-tokens] [--debug-types] [--no-rebar-compile]  - Compile a .lx file or project')
		eprintln('  new <app_name>                                                                        - Create a new Lx application')
		eprintln('  shell [project_path]                                                                  - Start an interactive shell for the project')
		eprintln('  symlink                                                                               - Create a symlink')
		exit(1)
	}

	command := args[1]
	match command {
		'compile' {
			lx_cli.handle_compile_command(args[2..])
		}
		'new' {
			lx_cli.handle_new_command(args[2..])
		}
		'shell' {
			lx_cli.handle_shell_command(args[2..])
		}
		'symlink' {
			lx_cli.handle_symlink_command()
		}
		else {
			eprintln('Unknown command: ${command}')
			eprintln('Available commands: compile, new, shell, symlink')
			exit(1)
		}
	}
}
