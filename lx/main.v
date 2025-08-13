module main

import os
import compile

fn main() {
	if os.args.len < 2 {
		eprintln('Usage: lx <command> [options]')
		eprintln('')
		eprintln('Commands:')
		eprintln('  new <project>           Create new project (minimal)')
		eprintln('  add app <name>          Add a new app to current project')
		eprintln('  compile <file|dir>      Compile LX files')
		eprintln('  .                       Compile current project (generate rebar3 structure)')
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
		'add' {
			if os.args.len < 4 || os.args[2] != 'app' {
				eprintln('Usage: lx add app <name>')
				exit(1)
			}
			app_name := os.args[3]
			add_app_to_project('.', app_name)
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
			force := os.args.len > 2 && (os.args[2] == '--force' || os.args[2] == '-f')
			create_symlink(force)
		}
		'.' {
			compile_project('.')
		}
		else {
			// If the argument is a directory (e.g., lx . or lx /path), compile the project there
			if os.exists(command) && os.is_dir(command) {
				compile_project(command)
				return
			}
			// Legacy: treat as file to compile
			compile.compile_file(command)
		}
	}
}

fn print_help() {
	println('LX Compiler - OTP Project Support')
	println('')
	println('USAGE:')
	println('    lx new <project>           Create new project skeleton (apps/ + <project>.config)')
	println('    lx add app <name>          Add app folder with <name>.lx')
	println('    lx compile <file|dir>      Compile LX files to Erlang')
	println('    lx .                       Compile project and generate rebar3 structure')
	println('    lx run <file.lx>          Compile and run single LX file')
	println('    lx shell [dir]            Launch rebar3 shell in project')
	println('    lx symlink [--force]     Create/overwrite global symlink at /usr/local/bin/lx')
	println('    lx --version              Show version information')
	println('    lx --help                 Show this help message')
	println('')
	println('PROJECT LAYOUT:')
	println('    <project>/')
	println('      apps/')
	println('      <project>.config')
}

// Create new minimal project skeleton
fn create_new_project(project_name string) {
	if os.exists(project_name) {
		eprintln('Error: Directory "${project_name}" already exists')
		exit(1)
	}

	println('Creating project: ${project_name}')

	// Create directory structure (minimal)
	dirs := [
		project_name,
		'${project_name}/apps',
	]

	for dir in dirs {
		os.mkdir_all(dir) or {
			eprintln('Failed to create directory ${dir}: ${err}')
			exit(1)
		}
	}

	// Create project config file
	project_config := '# ${project_name}.config - Lx project configuration\n'
	os.write_file('${project_name}/${project_name}.config', project_config) or {
		eprintln('Failed to create ${project_name}.config: ${err}')
		exit(1)
	}

	println('Created project ${project_name}')
	println('To get started:')
	println('  cd ${project_name}')
	println('  lx add app ${project_name}')
	println('  lx .')
	println('  lx shell')
}

// Add a new app to the current project directory
fn add_app_to_project(project_dir string, app_name string) {
	apps_dir := os.join_path(project_dir, 'apps')
	if !os.exists(apps_dir) {
		eprintln('Error: apps/ directory not found. Are you in a project folder?')
		exit(1)
	}

	app_dir := os.join_path(apps_dir, app_name)
	if os.exists(app_dir) {
		eprintln('Error: app "${app_name}" already exists')
		exit(1)
	}

	os.mkdir_all(app_dir) or {
		eprintln('Failed to create app directory: ${err}')
		exit(1)
	}

	// Create initial file <app_name>.lx
	initial_lx := generate_initial_app_template(app_name)
	os.write_file(os.join_path(app_dir, '${app_name}.lx'), initial_lx) or {
		eprintln('Failed to create initial LX file: ${err}')
		exit(1)
	}

	println('Added app ${app_name} at ${app_dir}')
}

// Compile target (file or directory)
fn compile_target(target string, run_rebar bool) {
	if !os.exists(target) {
		eprintln('Error: "${target}" not found')
		exit(1)
	}

	if os.is_dir(target) {
		// If this looks like a project (has apps/), compile as project
		if os.exists(os.join_path(target, 'apps')) {
			compile_project(target)
			return
		}

		// Compile all .lx files in directory (non-project)
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

// Compile an Lx project: generate rebar3 structure and build
fn compile_project(project_dir string) {
	// Ensure rebar files under _build
	ensure_rebar_build_files(project_dir)

	apps_dir := os.join_path(project_dir, 'apps')
	if !os.exists(apps_dir) {
		eprintln('Error: apps/ directory not found in ${project_dir}')
		exit(1)
	}

	// Build output apps dir inside _build
	build_dir := os.join_path(project_dir, '_build')
	build_apps_root := os.join_path(build_dir, 'apps')
	os.mkdir_all(build_apps_root) or {}

	apps := os.ls(apps_dir) or { []string{} }
	for app_name in apps {
		app_path := os.join_path(apps_dir, app_name)
		if !os.is_dir(app_path) {
			continue
		}

		// Target paths inside _build
		app_build_dir := os.join_path(build_apps_root, app_name)
		app_src_dir := os.join_path(app_build_dir, 'src')
		app_include_dir := os.join_path(app_build_dir, 'include')
		os.mkdir_all(app_src_dir) or {}
		os.mkdir_all(app_include_dir) or {}

		// Collect .lx files at the top level of the app directory (ignore src/include)
		entries := os.ls(app_path) or { []string{} }
		mut app_src_written := false
		for entry in entries {
			if entry.ends_with('.lx') {
				lx_path := os.join_path(app_path, entry)
				// Copy source into build src dir
				build_lx_path := os.join_path(app_src_dir, entry)
				os.cp(lx_path, build_lx_path) or {}

				// Compile the .lx inside _build so all artifacts (.erl/.app.src/.hrl) are generated there
				compile.compile_file(build_lx_path)

				app_src_written = app_src_written || os.exists(os.join_path(app_src_dir, '${app_name}.app.src'))
			}
		}

		// If no file produced an app.src, create a default one in _build
		if !app_src_written {
			os.write_file(os.join_path(app_src_dir, '${app_name}.app.src'), default_app_src(app_name)) or {}
		}
	}

	// Run rebar3 compile inside _build
	original_dir := os.getwd()
	os.chdir(build_dir) or {}
	println('Running rebar3 compile in ${build_dir} ...')
	res := os.execute('rebar3 compile')
	os.chdir(original_dir) or {}
	if res.exit_code != 0 {
		eprintln('rebar3 compile failed: ${res.output}')
		exit(1)
	}
	println('rebar3 compile completed successfully')
}

// Ensure umbrella rebar.config under _build and sys.config under _build/config
fn ensure_rebar_build_files(project_dir string) {
	build_dir := os.join_path(project_dir, '_build')
	rebar_path := os.join_path(build_dir, 'rebar.config')
	config_dir := os.join_path(build_dir, 'config')
	sys_config_path := os.join_path(config_dir, 'sys.config')
	os.mkdir_all(build_dir) or {}

	if !os.exists(rebar_path) {
		content := '{erl_opts, [debug_info]}.' + '\n' +
			'{deps, []}.' + '\n' +
			'{apps_dir, ["apps/*"]}.' + '\n' +
			'{shell, [' + '\n' +
			'  {config, "config/sys.config"}' + '\n' +
			']}.' + '\n' + '\n'
		os.write_file(rebar_path, content) or {}
		println('Created ${rebar_path}')
	}
	if !os.exists(sys_config_path) {
		os.mkdir_all(config_dir) or {}
		os.write_file(sys_config_path, '[] .\n') or {}
		println('Created ${sys_config_path}')
	}
}

// Generate a default .app.src content for an app
fn default_app_src(app_name string) string {
	return '{application, ${app_name},\n [\n  {description, "An OTP application"},\n  {vsn, "0.1.0"},\n  {registered, []},\n  {applications, [kernel, stdlib]},\n  {env, []}\n ]}.\n'
}

// Rewrite the application name line in a generated .app.src content
fn rewrite_app_name(content string, app_name string) string {
	mut lines := content.split('\n')
	for i, line in lines {
		if line.starts_with('{application, ') {
			lines[i] = '{application, ${app_name},'
			break
		}
	}
	return lines.join('\n')
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
	run_res := os.execute('erl -noshell -eval \'io:format("~p\\n", [' + module_name + ':main()]).\' -s init stop')
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
	// If rebar project files are missing, create them to allow shell to start
	ensure_rebar_build_files(dir)

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

	// Load kernel modules from ~/.lx/kernel if they exist (via ERL_FLAGS so the same VM sees it)
	kernel_path := os.join_path(os.home_dir(), '.lx', 'kernel')
	if os.exists(kernel_path) {
		current_flags := os.getenv_opt('ERL_FLAGS') or { '' }
		mut new_flags := '-pa ${kernel_path}'
		if current_flags.len > 0 {
			new_flags = new_flags + ' ' + current_flags
		}
		os.setenv('ERL_FLAGS', new_flags, true)
		println('ERL_FLAGS set for shell: ${new_flags}')
	}

	// Pre-compile all .lx files under apps/
	apps_dir := os.join_path(dir, 'apps')
	if os.exists(apps_dir) {
		println('Compiling Lx sources under ${apps_dir} ...')
		compile_project('.')
	}

	// Launch rebar3 shell inside _build
	build_dir := os.join_path(dir, '_build')
	println('Starting rebar3 shell in ${build_dir} ...')
	os.chdir(build_dir) or {}
	_ := os.system('rebar3 shell')

	os.chdir(original_dir) or {}
}

// Load kernel modules from specified directory
fn load_kernel_modules(kernel_path string) {
	entries := os.ls(kernel_path) or { return }

	for entry in entries {
		if entry.ends_with('.beam') {
			module_name := os.file_name(entry).all_before_last('.')

			// Add the kernel path to code path
			code_path_cmd := 'erl -noshell -eval \'code:add_patha("${kernel_path}").\' -s init stop'
			os.execute(code_path_cmd)

			println('  Loaded kernel module: ${module_name}')
		}
	}
}

// Create global symlink
fn create_symlink(force bool) {
	lx_path := os.executable()
	target := '/usr/local/bin/lx'

	if os.exists(target) {
		if force {
			// Try remove existing symlink/file
			os.rm(target) or {
				eprintln('Failed to remove existing target ${target}: ${err}')
				exit(1)
			}
			println('Removed existing ${target}')
		} else {
			println('Symlink already exists at ${target}')
			println('Use: sudo lx symlink --force  to overwrite')
			return
		}
	}

	os.symlink(lx_path, target) or {
		eprintln('Failed to create symlink: ${err}')
		eprintln('You may need to run with sudo: sudo lx symlink [--force]')
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

// Generate initial template for a new app including application, supervisor and a worker
fn generate_initial_app_template(app_name string) string {
	return 'application {\n' +
		'  description: "' + app_name + ' application",\n' +
		'  vsn: "0.1.0",\n' +
		'  applications: [:kernel, :stdlib],\n' +
		'  registered: [:' + app_name + '_sup]\n' +
		'}\n\n' +
		'supervisor ' + app_name + '_sup do\n' +
		'  strategy = :one_for_one\n' +
		'  children = [:' + app_name + '_worker]\n' +
		'end\n\n' +
		'worker ' + app_name + '_worker do\n' +
		'  def init(_args) do\n' +
		'    {:ok, %{}}\n' +
		'  end\n' +
		'end\n\n' +
		'def main() do\n' +
		'  :ok\n' +
		'end\n'
}
