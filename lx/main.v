module main

import os
import compiler

fn main() {
	args := os.args
	if args.len < 2 {
		eprintln('Usage: lx <command> [options]')
		eprintln('Commands:')
		eprintln('  compile <input_file> [--debug-tokens] [--debug-types]  - Compile a .lx file')
		eprintln('  new <app_name>                                       - Create a new Lx application')
		eprintln('  symlink                                               - Create a symlink')
		exit(1)
	}

	command := args[1]
	match command {
		'compile' {
			handle_compile_command(args[2..])
		}
		'new' {
			handle_new_command(args[2..])
		}
		'symlink' {
			handle_symlink_command()
		}
		else {
			eprintln('Unknown command: ${command}')
			eprintln('Available commands: compile, new, symlink')
			exit(1)
		}
	}
}

fn handle_compile_command(args []string) {
	if args.len < 1 {
		eprintln('Usage: lx compile <input_file> [--debug-tokens] [--debug-types]')
		exit(1)
	}

	input_file := args[0]
	mut debug_tokens := false
	mut debug_types := false

	// Parse command line flags
	for i in 1 .. args.len {
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

fn handle_new_command(args []string) {
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

	if !is_valid_app_name(app_name) {
		eprintln('Invalid app name: ${app_name}')
		eprintln('App name must contain only lowercase letters, numbers, and underscores')
		exit(1)
	}

	// Check if directory already exists
	if os.exists(app_name) {
		eprintln('Directory already exists: ${app_name}')
		exit(1)
	}

	// Create the application
	create_new_app(app_name)
}

fn handle_symlink_command() {
	link_path := '/usr/local/bin/lx'
	binary_path := os.real_path(os.executable())

	if os.exists(link_path) {
		if os.is_link(link_path) {
			println('Symlink already exists: ${link_path}')
			return
		} else {
			eprintln('File or directory named "${link_path}" already exists and is not a symlink.')
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

fn is_valid_app_name(name string) bool {
	for c in name {
		if !((c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) || c == `_`) {
			return false
		}
	}
	return true
}

fn create_new_app(app_name string) {
	// Create directory
	os.mkdir(app_name) or {
		eprintln('Failed to create directory: ${err}')
		exit(1)
	}

	// Create main application file
	main_content := generate_main_file(app_name)
	main_file := '${app_name}/${app_name}.lx'
	os.write_file(main_file, main_content) or {
		eprintln('Failed to create main file: ${err}')
		exit(1)
	}

	// Create application configuration file
	app_content := generate_application_file(app_name)
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
	println('  cd ${app_name}')
	println('  v run ../lx ${app_name}.lx')
}

fn generate_main_file(app_name string) string {
	return 'deps [:erlang, :kernel, :stdlib]

# Main application module
def main() do
    println("Hello from ${app_name}!")
    .{:ok, "Application started successfully"}
end

# Example function
def greet(name :: string) do
    "Hello, " ++ name ++ "!"
end

# Example with pattern matching
def process_message(message :: any) do
    case message do
        .{:hello, name} -> greet(name)
        .{:bye, name} -> "Goodbye, " ++ name ++ "!"
        _ -> "Unknown message"
    end
end

# Example with records
record Config do
    debug :: boolean,
    timeout :: integer
end

def create_config(debug :: boolean, timeout :: integer) do
    Config{debug: debug, timeout: timeout}
end

# Example with maps
def process_data(data :: %{}) do
    case data do
        %{name: name, age: age} when age >= 18 ->
            "Adult: " ++ name
        %{name: name, age: age} when age < 18 ->
            "Minor: " ++ name
        _ ->
            "Invalid data"
    end
end

# Example with list comprehensions
def filter_numbers(numbers :: [integer]) do
    for num in numbers when num > 0 do
        num * 2
    end
end

# Example with fun expressions
def create_adder(n :: integer) do
    fn(x :: integer) do x + n end
end

# Example with receive
def wait_for_message(timeout :: integer) do
    receive do
        .{:data, data} -> .{:ok, data}
        .{:error, reason} -> .{:error, reason}
    after timeout do
        .{:error, "Timeout"}
    end
end

# Example with message passing
def send_message(pid :: pid, message :: any) do
    pid ! message
end

# Testing
describe "${app_name} Tests" do
    test "greet function" do
        result = greet("World")
        assert result == "Hello, World!"
    end

    test "process message" do
        result = process_message(.{:hello, "Alice"})
        assert result == "Hello, Alice!"
    end

    test "create config" do
        config = create_config(true, 5000)
        assert config.debug == true
        assert config.timeout == 5000
    end

    test "filter numbers" do
        numbers = [1, -2, 3, -4, 5]
        result = filter_numbers(numbers)
        assert result == [2, 6, 10]
    end

    test "fun expression" do
        add5 = create_adder(5)
        result = add5(10)
        assert result == 15
    end
end'
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
