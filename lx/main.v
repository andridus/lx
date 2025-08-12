module main

import os
import compile

fn main() {
	if os.args.len < 2 {
		eprintln('Usage: lx <command> [options]')
		eprintln('')
		eprintln('Commands:')
		eprintln('  new <project>           Create new OTP project')
		eprintln('  compile <file|dir>      Compile LX files')
		eprintln('  run <file.lx>          Compile and run single file')
		eprintln('  shell [dir]            Launch rebar3 shell')
		eprintln('  symlink                Create global symlink')
		eprintln('  --version              Show version')
		eprintln('  --help                 Show help')
		exit(1)
	}

	command := os.args[1]

	match command {
		'--version' {
			println('LX Compiler v1.0.0 - OTP Project Support')
			exit(0)
		}
		'--help' {
			print_help()
			exit(0)
		}
		'new' {
			if os.args.len < 3 {
				eprintln('Usage: lx new <project>')
				exit(1)
			}
			project_name := os.args[2]
			create_new_project(project_name)
		}
		'compile' {
			if os.args.len < 3 {
				eprintln('Usage: lx compile <file.lx|dir> [--no-rebar-compile]')
				exit(1)
			}
			target := os.args[2]
			no_rebar := os.args.len > 3 && os.args[3] == '--no-rebar-compile'
			compile_target(target, !no_rebar)
		}
		'run' {
			if os.args.len < 3 {
				eprintln('Usage: lx run <file.lx>')
				exit(1)
			}
			file := os.args[2]
			run_single_file(file)
		}
		'shell' {
			dir := if os.args.len > 2 { os.args[2] } else { '.' }
			launch_shell(dir)
		}
		'symlink' {
			create_symlink()
		}
		else {
			// Legacy: treat as file to compile
			compile.compile_file(command)
		}
	}
}

fn print_help() {
	println('LX Compiler - OTP Project Support')
	println('')
	println('USAGE:')
	println('    lx new <project>           Create new OTP project skeleton')
	println('    lx compile <file|dir>      Compile LX files to Erlang')
	println('    lx run <file.lx>          Compile and run single LX file')
	println('    lx shell [dir]            Launch rebar3 shell in project')
	println('    lx symlink                Create global symlink at /usr/local/bin/lx')
	println('    lx --version              Show version information')
	println('    lx --help                 Show this help message')
	println('')
	println('OTP PROJECT FEATURES:')
	println('    application { ... }       Define application metadata')
	println('    supervisor name do ... end Create OTP supervisor')
	println('    worker name do ... end     Create OTP gen_server worker')
	println('    record name { ... }        Define records (generates .hrl)')
	println('    fun.(args)                Anonymous function calls')
	println('')
	println('EXAMPLES:')
	println('    lx new myapp              # Create new OTP project')
	println('    lx compile src/           # Compile all .lx files in src/')
	println('    lx run examples/demo.lx   # Run single file')
	println('    lx shell                  # Launch rebar3 shell')
}

// Create new OTP project skeleton
fn create_new_project(project_name string) {
	if os.exists(project_name) {
		eprintln('Error: Directory "${project_name}" already exists')
		exit(1)
	}

	println('Creating OTP project: ${project_name}')

	// Create directory structure
	dirs := [
		project_name,
		'${project_name}/apps',
		'${project_name}/apps/${project_name}',
		'${project_name}/apps/${project_name}/src',
		'${project_name}/apps/${project_name}/include',
		'${project_name}/apps/${project_name}/test',
	]

	for dir in dirs {
		os.mkdir_all(dir) or {
			eprintln('Failed to create directory ${dir}: ${err}')
			exit(1)
		}
	}

	// Create rebar.config
	rebar_config := '{erl_opts, [debug_info]}.
{deps, []}.
{shell, [
  {config, "config/sys.config"},
  {apps, [${project_name}]}
]}.
'
	os.write_file('${project_name}/rebar.config', rebar_config) or {
		eprintln('Failed to create rebar.config: ${err}')
		exit(1)
	}

	// Create minimal .app.src
	app_src := '{application, ${project_name},
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []}]}.
'
	os.write_file('${project_name}/apps/${project_name}/src/${project_name}.app.src',
		app_src) or {
		eprintln('Failed to create .app.src: ${err}')
		exit(1)
	}

	// Create minimal LX file
	main_lx := 'application {
  description: "An OTP application",
  vsn: "0.1.0",
  applications: [:kernel, :stdlib]
}

def main() do
  :ok
end
'
	os.write_file('${project_name}/apps/${project_name}/src/${project_name}.lx', main_lx) or {
		eprintln('Failed to create main LX file: ${err}')
		exit(1)
	}

	println('Created project ${project_name}')
	println('To get started:')
	println('  cd ${project_name}')
	println('  lx compile apps/${project_name}/src/')
	println('  lx shell')
}

// Compile target (file or directory)
fn compile_target(target string, run_rebar bool) {
	if !os.exists(target) {
		eprintln('Error: "${target}" not found')
		exit(1)
	}

	if os.is_dir(target) {
		// Compile all .lx files in directory
		lx_files := find_lx_files(target)
		if lx_files.len == 0 {
			println('No .lx files found in ${target}')
			return
		}

		for lx_file in lx_files {
			compile.compile_file(lx_file)
		}

		if run_rebar && os.exists('rebar.config') {
			println('Running rebar3 compile...')
			res := os.execute('rebar3 compile')
			if res.exit_code != 0 {
				eprintln('rebar3 compile failed: ${res.output}')
				exit(1)
			}
			println('rebar3 compile completed successfully')
		}
	} else {
		// Compile single file
		compile.compile_file(target)
	}
}

// Run single file (lightweight mode)
fn run_single_file(file string) {
	if !os.exists(file) {
		eprintln('Error: File "${file}" not found')
		exit(1)
	}

	module_name := os.file_name(file).all_before_last('.')
	dir := os.dir(file)
	compile.compile_file(file)
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
}

// Launch rebar3 shell
fn launch_shell(dir string) {
	if !os.exists('${dir}/rebar.config') {
		eprintln('Error: No rebar.config found in ${dir}')
		eprintln('Make sure you are in an OTP project directory')
		exit(1)
	}

	original_dir := os.getwd()
	os.chdir(dir) or {
		eprintln('Failed to change directory: ${err}')
		exit(1)
	}

	// Check if rebar3 is available
	rebar_check := os.execute('which rebar3')
	if rebar_check.exit_code != 0 {
		eprintln('Error: rebar3 not found in PATH')
		eprintln('Please install rebar3: https://rebar3.readme.io/docs/getting-started')
		os.chdir(original_dir) or {}
		exit(1)
	}

	println('Launching rebar3 shell...')
	os.system('rebar3 shell')
	os.chdir(original_dir) or {}
}

// Create global symlink
fn create_symlink() {
	lx_path := os.executable()
	target := '/usr/local/bin/lx'

	if os.exists(target) {
		println('Symlink already exists at ${target}')
		return
	}

	os.symlink(lx_path, target) or {
		eprintln('Failed to create symlink: ${err}')
		eprintln('You may need to run with sudo: sudo lx symlink')
		exit(1)
	}

	println('Created symlink: ${target} -> ${lx_path}')
	println('You can now use "lx" from anywhere')
}

// Helper function to find .lx files in directory
fn find_lx_files(dir string) []string {
	mut files := []string{}
	entries := os.ls(dir) or { return files }

	for entry in entries {
		full_path := os.join_path(dir, entry)
		if os.is_dir(full_path) {
			// Recursively search subdirectories
			files << find_lx_files(full_path)
		} else if entry.ends_with('.lx') {
			files << full_path
		}
	}

	return files
}
