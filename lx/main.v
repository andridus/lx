module main

import os
import compile

// Main function to handle command-line arguments and delegate tasks.
fn main() {
	if os.args.len < 2 {
		print_help()
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

// =============================
// Helper Functions for Commands
// =============================

// print_help prints the command-line help message.
fn print_help() {
	println('LX Compiler - OTP Project Support')
	println('')
	println('USAGE:')
	println('    lx new <project>           Create new project skeleton (apps/ + <project>.config)')
	println('    lx add app <name>          Add app folder with <name>.lx')
	println('    lx compile <file|dir>      Compile LX files to Erlang')
	println('    lx .                       Compile project and generate rebar3 structure')
	println('    lx run <file.lx>           Compile and run single LX file')
	println('    lx shell [dir]             Launch rebar3 shell in project')
	println('    lx symlink [--force]       Create/overwrite global symlink at /usr/local/bin/lx')
	println('    lx --version               Show version information')
	println('    lx --help                  Show this help message')
	println('')
	println('PROJECT LAYOUT:')
	println('    <project>/')
	println('      apps/')
	println('      <project>.config')
}

// create_new_project creates a new minimal project skeleton.
fn create_new_project(project_name string) {
	if os.exists(project_name) {
		eprintln('Error: Directory "${project_name}" already exists')
		exit(1)
	}

	println('Creating project: ${project_name}')

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

	project_yml := '# ${project_name}.yml - Lx project configuration\n' + 'erl_opts:\n' +
		'  - debug_info\n' + '  - nowarn_unused_vars\n' + 'deps:\n' +
		'  # Add your dependencies here\n' + '  # Example:\n' + '  # jsx: "3.1.0"\n' +
		'  # cowboy:\n' + '  #   git: "https://github.com/ninenines/cowboy.git"\n' +
		'  #   branch: "master"\n'
	os.write_file('${project_name}/${project_name}.yml', project_yml) or {
		eprintln('Failed to create ${project_name}.yml: ${err}')
		exit(1)
	}

	println('Created project ${project_name}')
	println('To get started:')
	println('  cd ${project_name}')
	println('  lx add app ${project_name}')
	println('  lx .')
	println('  lx shell')
}

// add_app_to_project adds a new app to the current project directory.
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

	initial_lx := generate_initial_app_template(app_name)
	os.write_file(os.join_path(app_dir, '${app_name}.lx'), initial_lx) or {
		eprintln('Failed to create initial LX file: ${err}')
		exit(1)
	}

	println('Added app ${app_name} at ${app_dir}')
}

// compile_target compiles a file or directory.
fn compile_target(target string, run_rebar bool) {
	if !os.exists(target) {
		eprintln('Error: "${target}" not found')
		exit(1)
	}

	if os.is_dir(target) {
		if os.exists(os.join_path(target, 'apps')) {
			compile_project(target)
			return
		}

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
			exit_code := os.system('rebar3 compile')
			if exit_code != 0 {
				eprintln('rebar3 compile failed with exit code: ${exit_code}')
				exit(1)
			}
			println('rebar3 compile completed successfully')
		}
	} else {
		compile.compile_file(target)
	}
}

// compile_project compiles an Lx project.
fn compile_project(project_dir string) {
	ensure_rebar_build_files(project_dir)
	apps_dir := os.join_path(project_dir, 'apps')
	if !os.exists(apps_dir) {
		eprintln('Error: apps/ directory not found in ${project_dir}')
		exit(1)
	}

	build_dir := os.join_path(project_dir, '_build')
	build_apps_root := os.join_path(build_dir, 'apps')
	os.mkdir_all(build_apps_root) or {}

	apps := os.ls(apps_dir) or { []string{} }
	mut shell_apps := []string{}
	for app_name in apps {
		app_path := os.join_path(apps_dir, app_name)
		if !os.is_dir(app_path) {
			continue
		}

		app_build_dir := os.join_path(build_apps_root, app_name)
		app_src_dir := os.join_path(app_build_dir, 'src')
		app_include_dir := os.join_path(app_build_dir, 'include')
		os.mkdir_all(app_src_dir) or {}
		os.mkdir_all(app_include_dir) or {}

		entries := os.ls(app_path) or { []string{} }
		mut app_src_written := false
		for entry in entries {
			if entry.ends_with('.lx') {
				lx_path := os.join_path(app_path, entry)
				build_lx_path := os.join_path(app_src_dir, entry)
				os.cp(lx_path, build_lx_path) or {}
				compile.compile_file(build_lx_path)
				app_src_written = app_src_written || os.exists(os.join_path(app_src_dir, '${app_name}.app.src'))
			}
		}

		app_app_src_path := os.join_path(app_src_dir, '${app_name}.app.src')
		if !app_src_written {
			os.write_file(app_app_src_path, default_app_src(app_name)) or {}
		} else {
			mut content := os.read_file(app_app_src_path) or { '' }
			if content.len > 0 && !content.contains('{mod,') {
				if content.contains('{env,') {
					content = content.replace('{env,', '  {mod, {' + app_name +
						'_app, []}},\n  {env,')
				} else {
					content = content.replace('\n ]}.', ',\n  {mod, {' + app_name +
						'_app, []}}\n ]}.')
				}
				os.write_file(app_app_src_path, content) or {}
			}
		}

		app_app_module_path := os.join_path(app_src_dir, app_name + '_app.erl')
		if !os.exists(app_app_module_path) {
			os.write_file(app_app_module_path, generate_default_application_module(app_name)) or {}
		}

		shell_apps << app_name
	}

	update_rebar_shell_apps(build_dir, shell_apps)
	original_dir := os.getwd()
	os.chdir(build_dir) or {}
	println('Running rebar3 compile in ${build_dir} ...')
	exit_code := os.system('rebar3 compile')
	os.chdir(original_dir) or {}
	if exit_code != 0 {
		eprintln('rebar3 compile failed with exit code: ${exit_code}')
		exit(1)
	}
	println('rebar3 compile completed successfully')
}

// run_single_file compiles and runs a single LX file.
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
		':main()]).\' -s init stop')
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

// launch_shell launches the rebar3 shell.
fn launch_shell(dir string) {
	ensure_rebar_build_files(dir)
	original_dir := os.getwd()
	os.chdir(dir) or {
		eprintln('Failed to change directory: ${err}')
		exit(1)
	}

	rebar_check := os.execute('which rebar3')
	if rebar_check.exit_code != 0 {
		eprintln('Error: rebar3 not found in PATH')
		eprintln('Please install rebar3: https://rebar3.readme.io/docs/getting-started')
		os.chdir(original_dir) or {}
		exit(1)
	}

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

	apps_dir := os.join_path(dir, 'apps')
	if os.exists(apps_dir) {
		println('Compiling Lx sources under ${apps_dir} ...')
		compile_project('.')
	}

	build_dir := os.join_path(dir, '_build')
	println('Starting rebar3 shell in ${build_dir} ...')
	os.chdir(build_dir) or {}
	_ := os.system('rebar3 shell')
	os.chdir(original_dir) or {}
}

// create_symlink creates a global symlink for the LX executable.
fn create_symlink(force bool) {
	lx_path := os.executable()
	target := '/usr/local/bin/lx'

	if os.exists(target) {
		if force {
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

// =======================
// Project Utility Functions
// =======================

// ensure_rebar_build_files ensures the necessary rebar files exist in _build.
fn ensure_rebar_build_files(project_dir string) {
	build_dir := os.join_path(project_dir, '_build')
	rebar_path := os.join_path(build_dir, 'rebar.config')
	config_dir := os.join_path(build_dir, 'config')
	sys_config_path := os.join_path(config_dir, 'sys.config')
	os.mkdir_all(build_dir) or {}

	project_name := if project_dir == '.' { os.base(os.getwd()) } else { os.base(project_dir) }
	project_yml_path := os.join_path(project_dir, '${project_name}.yml')
	mut erl_opts := 'debug_info, nowarn_unused_vars'
	mut deps_content := '[]'

	if os.exists(project_yml_path) {
		yml_content := os.read_file(project_yml_path) or { '' }
		if yml_content.len > 0 {
			parsed_config := parse_project_yml(yml_content)
			if parsed_config.erl_opts.len > 0 {
				erl_opts = parsed_config.erl_opts
			}
			if parsed_config.deps.len > 0 {
				deps_content = parsed_config.deps
			}
		}
	}

	content := '{erl_opts, [${erl_opts}]}.' + '\n' + '{deps, ${deps_content}}.' + '\n' +
		'{apps_dir, ["apps/*"]}.' + '\n' + '{shell, [' + '\n' + '  {config, "config/sys.config"}' +
		'\n' + ']}.' + '\n' + '\n'
	os.write_file(rebar_path, content) or {}
	println('Created ${rebar_path}')

	if !os.exists(sys_config_path) {
		os.mkdir_all(config_dir) or {}
		os.write_file(sys_config_path, '[] .\n') or {}
		println('Created ${sys_config_path}')
	}
}

// update_rebar_shell_apps updates the rebar.config with shell apps list.
fn update_rebar_shell_apps(build_dir string, apps []string) {
	rebar_path := os.join_path(build_dir, 'rebar.config')
	mut content := os.read_file(rebar_path) or { '' }
	apps_list := '[' + apps.join(', ') + ']'
	if content.contains('{shell,') {
		if content.contains('{apps,') {
			start := content.index('{apps,') or { -1 }
			if start >= 0 {
				end := content.index_after('}', start) or { -1 }
				if end >= 0 {
					before := content[..start]
					after := content[end + 1..]
					content = before + '{apps, ' + apps_list + '}' + after
				}
			}
		} else {
			content = content.replace('{shell, [', '{shell, [\n  {apps, ' + apps_list + '},')
		}
	} else {
		content += '{shell, [\n  {apps, ' + apps_list + '},\n  {config, "config/sys.config"}\n]}.\n'
	}
	os.write_file(rebar_path, content) or {}
}

// generate_default_application_module generates a minimal default application behaviour module for an app.
fn generate_default_application_module(app_name string) string {
	mod := app_name + '_app'
	mut s := ''
	s += '-module(' + mod + ').\n'
	s += '-behaviour(application).\n\n'
	s += '-export([start/2, stop/1]).\n\n'
	s += 'start(_Type, _Args) ->\n'
	s += '    {ok, self()}.\n\n'
	s += 'stop(_State) ->\n'
	s += '    ok.\n'
	return s
}

// default_app_src generates a default .app.src content for an app.
fn default_app_src(app_name string) string {
	mut applications := '[kernel, stdlib]'
	project_dir := os.getwd()
	apps_dir := os.join_path(project_dir, 'apps')
	if os.exists(apps_dir) {
		main_lx_path := os.join_path(apps_dir, app_name, '${app_name}.lx')
		if os.exists(main_lx_path) {
			content := os.read_file(main_lx_path) or { '' }
			if content.len > 0 {
				if content.contains('deps [') {
					start_idx := content.index('deps [') or { -1 }
					if start_idx >= 0 {
						end_idx := content.index_after(']', start_idx) or { -1 }
						if end_idx >= 0 {
							deps_content := content[start_idx + 6..end_idx]
							mut deps_list := []string{}
							lines := deps_content.split('\n')
							for line in lines {
								trimmed := line.trim_space()
								if trimmed.starts_with(':') {
									dep_name := trimmed[1..].trim_space()
									if dep_name.len > 0 {
										deps_list << dep_name
									}
								}
							}
							if deps_list.len > 0 {
								applications = '[kernel, stdlib, ' + deps_list.join(', ') + ']'
							}
						}
					}
				}
			}
		}
	}

	return '{application, ${app_name},\n [\n  {description, "An OTP application"},\n  {vsn, "0.1.0"},\n  {registered, []},\n  {applications, ${applications}},\n  {mod, {${app_name}_app, []}},\n  {env, []}\n ]}.\n'
}

// rewrite_app_name rewrites the application name line in a generated .app.src content.
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

// find_lx_files is a helper function to find .lx files in a directory.
fn find_lx_files(dir string) []string {
	mut files := []string{}
	entries := os.ls(dir) or { return files }
	for entry in entries {
		full_path := os.join_path(dir, entry)
		if os.is_dir(full_path) {
			files << find_lx_files(full_path)
		} else if entry.ends_with('.lx') {
			files << full_path
		}
	}
	return files
}

// generate_initial_app_template generates the initial template for a new app.
fn generate_initial_app_template(app_name string) string {
	return 'application {\n' + '  description: "' + app_name + ' application",\n' +
		'  vsn: "0.1.0",\n' + '  deps: []\n' + '}\n\n' + 'supervisor ' + app_name + '_sup do\n' +
		'  strategy :one_for_one\n' + '  children [:' + app_name + '_worker]\n' + 'end\n\n' +
		'worker ' + app_name + '_worker do\n' + '  def init(_args) do\n' + '    {:ok, %{}}\n' +
		'  end\n' + 'end\n\n' + 'def main() do\n' + '  :ok\n' + 'end\n'
}

// ==========================
// Project Configuration Parsing
// ==========================

// ProjectConfig holds parsed project configuration.
struct ProjectConfig {
mut:
	erl_opts string
	deps     string
}

// parse_project_yml parses a project YAML file.
fn parse_project_yml(yml_content string) ProjectConfig {
	mut config := ProjectConfig{
		erl_opts: 'debug_info, nowarn_unused_vars'
		deps:     '[]'
	}
	lines := yml_content.split('\n')
	mut in_erl_opts := false
	mut in_deps := false
	mut erl_opts_list := []string{}
	mut deps_list := []string{}
	mut current_dep := ''
	mut current_dep_git := ''
	mut current_dep_branch := ''
	for line in lines {
		trimmed := line.trim_space()
		if trimmed.starts_with('#') || trimmed.len == 0 {
			continue
		}
		if trimmed == 'erl_opts:' {
			in_erl_opts = true
			in_deps = false
			continue
		}
		if trimmed == 'deps:' {
			in_erl_opts = false
			in_deps = true
			continue
		}
		if in_erl_opts && trimmed.starts_with('- ') {
			opt := trimmed[2..].trim_space()
			if opt.len > 0 {
				erl_opts_list << opt
			}
		}
		if in_deps {
			if !trimmed.starts_with('  ') && trimmed.contains(':') && !trimmed.starts_with('git:') &&
				!trimmed.starts_with('branch:') {
				parts := trimmed.split(':')
				if parts.len >= 2 {
					current_dep = parts[0].trim_space()
					mut version := parts[1].trim_space().trim('"')
					if version.starts_with('"') {
						version = version[1..]
					}
					if version.ends_with('"') {
						version = version[..version.len - 1]
					}
					if version.len == 0 {
						current_dep_git = ''
						current_dep_branch = ''
					} else {
						deps_list << '{${current_dep}, "${version}"}'
						current_dep = ''
					}
				}
			} else if trimmed.contains('git:') {
				git_pos := trimmed.index('git:') or { -1 }
				if git_pos >= 0 {
					git_content := trimmed[git_pos + 4..].trim_space()
					mut git_url := git_content.trim('"')
					if git_url.starts_with('"') {
						git_url = git_url[1..]
					}
					if git_url.ends_with('"') {
						git_url = git_url[..git_url.len - 1]
					}
					current_dep_git = git_url
				}
			} else if trimmed.contains('branch:') {
				branch_pos := trimmed.index('branch:') or { -1 }
				if branch_pos >= 0 {
					branch_content := trimmed[branch_pos + 7..].trim_space()
					mut branch := branch_content.trim('"')
					if branch.starts_with('"') {
						branch = branch[1..]
					}
					if branch.ends_with('"') {
						branch = branch[..branch.len - 1]
					}
					current_dep_branch = branch
					if current_dep.len > 0 && current_dep_git.len > 0 {
						deps_list << '{${current_dep}, {git, "${current_dep_git}", {branch, "${current_dep_branch}"}}}'
						current_dep = ''
						current_dep_git = ''
						current_dep_branch = ''
					}
				}
			}
		}
	}
	if erl_opts_list.len > 0 {
		config.erl_opts = erl_opts_list.join(', ')
	}
	if deps_list.len > 0 {
		config.deps = '[${deps_list.join(', ')}]'
	}
	return config
}