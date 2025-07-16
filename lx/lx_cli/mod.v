module lx_cli

import os
import compiler

// CLI configuration
pub struct CLIConfig {
pub mut:
	debug_tokens bool
	debug_types  bool
	no_rebar_compile bool
}

// Command handlers
pub fn handle_new_command(args []string) {
	if args.len < 1 {
		eprintln('Usage: lx new <app_name>')
		exit(1)
	}

	app_name := args[0]

	// Validate app name
	if app_name.len == 0 {
		eprintln('App name cannot be empty')
		exit(1)
	}

	// Extract the actual app name from the path
	actual_app_name := os.file_name(app_name)

	if !is_valid_app_name(actual_app_name) {
		eprintln('Invalid app name: ${actual_app_name}')
		eprintln('App name must contain only lowercase letters, numbers, and underscores')
		exit(1)
	}

	// Create the application
	create_new_app(app_name)
}

pub fn handle_compile_command(args []string) {
	if args.len < 1 {
		eprintln('Usage: lx compile <input_file_or_project> [--debug-tokens] [--debug-types] [--no-rebar-compile]')
		exit(1)
	}

	input_path := args[0]
	mut config := CLIConfig{}

	// Parse command line flags
	for i in 1 .. args.len {
		match args[i] {
			'--debug-tokens' {
				config.debug_tokens = true
			}
			'--debug-types' {
				config.debug_types = true
			}
			'--no-rebar-compile' {
				config.no_rebar_compile = true
			}
			else {
				eprintln('Unknown flag: ${args[i]}')
				exit(1)
			}
		}
	}

	if !os.exists(input_path) {
		eprintln('Input file or project not found: ${input_path}')
		exit(1)
	}

	// Check if it's a project directory or a single file
	if os.is_dir(input_path) {
		compile_project(input_path, config)
	} else {
		compile_single_file(input_path, config)
	}
}

pub fn handle_shell_command(args []string) {
	mut project_path := ''

	if args.len == 0 {
		// No path provided, check if we're in a project directory
		current_dir := os.getwd()
		if is_valid_project_directory(current_dir) {
			project_path = current_dir
		} else {
			eprintln('Not in a valid Lx project directory and no project path provided.')
			eprintln('Usage: lx shell [project_path]')
			exit(1)
		}
	} else {
		// Path provided
		project_path = args[0]
		if !os.exists(project_path) {
			eprintln('Project path not found: ${project_path}')
			exit(1)
		}

		if !is_valid_project_directory(project_path) {
			eprintln('Not a valid Lx project directory: ${project_path}')
			exit(1)
		}
	}

	start_project_shell(project_path)
}

pub fn handle_symlink_command() {
	link_path := '/usr/local/bin/lx'
	binary_path := os.real_path(os.executable())

	// Remove existing symlink or file
	if os.exists(link_path) {
		os.rm(link_path) or {
			eprintln('Failed to remove existing file: ${err}')
			eprintln('You may need to run this command with sudo.')
			exit(1)
		}
	}

	os.symlink(binary_path, link_path) or {
		eprintln('Failed to create symlink: ${err}')
		eprintln('You may need to run this command with sudo.')
		exit(1)
	}
	println('Symlink created: ${link_path} -> ${binary_path}')
}

// Helper functions
fn is_valid_app_name(name string) bool {
	for c in name {
		if !((c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) || c == `_`) {
			return false
		}
	}
	return true
}

fn create_new_app(app_name string) {
	// Extract the actual app name from the path
	actual_app_name := os.file_name(app_name)

	// Create directory structure
	os.mkdir_all(app_name) or {
		eprintln('Failed to create directory: ${err}')
		exit(1)
	}

	src_dir := '${app_name}/src'
	os.mkdir_all(src_dir) or {
		eprintln('Failed to create src directory: ${err}')
		exit(1)
	}

	// Create main application file
	main_content := generate_main_file(actual_app_name)
	main_file := '${src_dir}/${actual_app_name}.lx'
	os.write_file(main_file, main_content) or {
		eprintln('Failed to create main file: ${err}')
		exit(1)
	}

	// Create application configuration file
	app_content := generate_application_file(actual_app_name)
	app_file := '${app_name}/application.lx'
	os.write_file(app_file, app_content) or {
		eprintln('Failed to create application file: ${err}')
		exit(1)
	}

	println('Created new Lx application: ${app_name}')
	println('Files created:')
	println('  - ${main_file}')
	println('  - ${app_file}')
	println('')
	println('To compile and run:')
	println('  lx compile ${app_name}')
}

fn compile_project(project_path string, config CLIConfig) {
	// Check if it's a valid Lx project
	app_file := '${project_path}/application.lx'
	if !os.exists(app_file) {
		eprintln('Not a valid Lx project: missing application.lx file')
		exit(1)
	}

	src_dir := '${project_path}/src'
	if !os.exists(src_dir) {
		eprintln('Not a valid Lx project: missing src directory')
		exit(1)
	}

	// Get project name from path
	project_name := os.file_name(project_path)

	// Create _build directory structure
	build_dir := '${project_path}/_build'
	project_build_dir := '${build_dir}/${project_name}'
	build_src_dir := '${project_build_dir}/src'

	os.mkdir_all(build_src_dir) or {
		eprintln('Failed to create build directory: ${err}')
		exit(1)
	}

	// Find all .lx files in src directory
	lx_files := find_lx_files(src_dir)

	if lx_files.len == 0 {
		eprintln('No .lx files found in src directory')
		exit(1)
	}

	// Compile each .lx file
	mut comp := compiler.new_compiler()
	if config.debug_tokens {
		comp.enable_debug_tokens()
	}
	if config.debug_types {
		comp.enable_debug_types()
	}

	for lx_file in lx_files {
		println('Compiling ${lx_file}...')

		// Read source file
		source := os.read_file(lx_file) or {
			eprintln('Failed to read file: ${lx_file}: ${err}')
			exit(1)
		}

		// Compile to Erlang
		result := comp.compile(source, lx_file)

		// Generate output filename
		base_name := os.file_name(lx_file).replace('.lx', '')
		output_file := '${build_src_dir}/${base_name}.erl'

		// Write Erlang code
		os.write_file(output_file, result.code) or {
			eprintln('Failed to write output file: ${err}')
			exit(1)
		}
	}

	// Generate .app.src file
	app_src_content := generate_app_src_file(project_name)
	app_src_file := '${build_src_dir}/${project_name}.app.src'
	os.write_file(app_src_file, app_src_content) or {
		eprintln('Failed to create .app.src file: ${err}')
		exit(1)
	}

	// Generate rebar.config file
	rebar_config_content := generate_rebar_config()
	rebar_config_file := '${project_build_dir}/rebar.config'
	os.write_file(rebar_config_file, rebar_config_content) or {
		eprintln('Failed to create rebar.config file: ${err}')
		exit(1)
	}

	println('Build files generated in ${build_dir}')

	// Run rebar compilation if not disabled
	if !config.no_rebar_compile {
		run_rebar_compile(project_build_dir)
	}
}

fn compile_single_file(file_path string, config CLIConfig) {
	// Check if file has .lx extension
	if !file_path.ends_with('.lx') {
		eprintln('Input file must have .lx extension: ${file_path}')
		exit(1)
	}

	// Use the compiler module to compile the file
	mut comp := compiler.new_compiler()
	if config.debug_tokens {
		comp.enable_debug_tokens()
	}
	if config.debug_types {
		comp.enable_debug_types()
	}
	comp.compile_file(file_path)
}

fn find_lx_files(dir_path string) []string {
	mut lx_files := []string{}

	files := os.ls(dir_path) or {
		eprintln('Failed to list directory: ${dir_path}')
		return lx_files
	}

	for file in files {
		full_path := '${dir_path}/${file}'
		if os.is_file(full_path) && file.ends_with('.lx') {
			lx_files << full_path
		}
	}

	return lx_files
}

fn run_rebar_compile(project_dir string) {
	println('Running rebar3 compile...')

	// Ensure rebar3 is available
	rebar_path := ensure_rebar3_available()

	// Change to project directory and run rebar3 compile
	original_dir := os.getwd()
	os.chdir(project_dir) or {
		eprintln('Failed to change to project directory: ${err}')
		exit(1)
	}

	// Run rebar3 compile
	result := os.execute('${rebar_path} compile')

	// Change back to original directory
	os.chdir(original_dir) or {
		eprintln('Failed to change back to original directory: ${err}')
	}

	if result.exit_code != 0 {
		eprintln('Rebar compilation failed:')
		eprintln(result.output)
		exit(1)
	}

	println('Rebar compilation successful!')
}

fn generate_main_file(app_name string) string {
	return '# Main application module
def main() do
    {:ok, "Application started successfully"}
end
'
}

fn generate_application_file(app_name string) string {
	return 'application {
  description: "${app_name} - A Lx Application",
  vsn: "1.0.0",
  applications: [:kernel, :stdlib],
  registered: [],
  env: %{
    debug: true,
    timeout: 5000,
    port: "8080"
  },
  deps: []
}
'
}

fn generate_app_src_file(app_name string) string {
	return '{application, ${app_name},
 [{description, "${app_name} - A Lx Application"},
  {vsn, "1.0.0"},
  {registered, []},
  {applications, [kernel, stdlib]},
  {env, []},
  {modules, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
'
}

fn generate_rebar_config() string {
	return '{erl_opts, [debug_info]}.
{deps, []}.
{shell, [
    {config, "config/sys.config"},
    {apps, []}
]}.
'
}

fn is_valid_project_directory(path string) bool {
	// Check if it's a valid Lx project
	app_file := '${path}/application.lx'
	src_dir := '${path}/src'

	return os.exists(app_file) && os.exists(src_dir)
}

fn start_project_shell(project_path string) {
	println('Starting shell for project: ${project_path}')

	// First compile the project to ensure it's up to date
	config := CLIConfig{
		debug_tokens: false
		debug_types: false
		no_rebar_compile: false
	}

	compile_project(project_path, config)

	// Get project name from path
	project_name := os.file_name(project_path)
	project_build_dir := '${project_path}/_build/${project_name}'

	// Ensure rebar3 is available
	rebar_path := ensure_rebar3_available()

	// Change to project build directory
	original_dir := os.getwd()
	os.chdir(project_build_dir) or {
		eprintln('Failed to change to project build directory: ${err}')
		exit(1)
	}

		println('Starting Erlang shell...')
	println('Project: ${project_name}')
	println('Build directory: ${project_build_dir}')
	println('Type "q()." to quit the shell')
	println('')

	// Start rebar3 shell interactively
	exit_code := os.system('${rebar_path} shell')

	// Change back to original directory
	os.chdir(original_dir) or {
		eprintln('Failed to change back to original directory: ${err}')
	}

	if exit_code != 0 {
		eprintln('Shell exited with code: ${exit_code}')
		exit(exit_code)
	}
}

fn ensure_rebar3_available() string {
	// First check if rebar3 is already in PATH
	rebar_check := os.execute('which rebar3')
	if rebar_check.exit_code == 0 {
		return 'rebar3'
	}

	// Check if we have a local rebar3 installation
	home_dir := os.home_dir()
	local_rebar := '${home_dir}/.local/bin/rebar3'

	if os.exists(local_rebar) {
		return local_rebar
	}

	// Download and install rebar3
	println('Rebar3 not found. Downloading and installing...')

	// Create local bin directory if it doesn't exist
	local_bin_dir := '${home_dir}/.local/bin'
	os.mkdir_all(local_bin_dir) or {
		eprintln('Failed to create local bin directory: ${err}')
		exit(1)
	}

	// Download rebar3
	download_url := 'https://github.com/erlang/rebar3/releases/latest/download/rebar3'
	download_result := os.execute('curl -L -o ${local_rebar} ${download_url}')

	if download_result.exit_code != 0 {
		eprintln('Failed to download rebar3: ${download_result.output}')
		exit(1)
	}

	// Make it executable
	chmod_result := os.execute('chmod +x ${local_rebar}')
	if chmod_result.exit_code != 0 {
		eprintln('Failed to make rebar3 executable: ${chmod_result.output}')
		exit(1)
	}

	// Verify installation
	verify_result := os.execute('${local_rebar} version')
	if verify_result.exit_code != 0 {
		eprintln('Failed to verify rebar3 installation: ${verify_result.output}')
		exit(1)
	}

	println('Rebar3 installed successfully at ${local_rebar}')
	return local_rebar
}