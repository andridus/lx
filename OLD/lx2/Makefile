.PHONY: all compile clean test dialyzer generate debug

# Default target
all: compile

# Compile the project
compile:
	rebar3 compile


# Clean build artifacts
clean:
	rebar3 clean

# Run tests
test:
	rebar3 eunit
	rebar3 ct

# Run AST Elixir-Style specific tests
test-ast-elixir:
	rebar3 eunit --module=lx2_ast_meta_tests

# Run Task 2 specific tests
test-task2:
	rebar3 eunit --module=lx2_task2_tests

# Run dialyzer
dialyzer:
	rebar3 dialyzer

# Generate lexer and parser
generate:
	rebar3 compile

# Debug with observer
debug:
	rebar3 shell

# Install dependencies
deps:
	rebar3 get-deps

# Run specific test
test-specific:
	rebar3 eunit --module=$(TEST)

# Format code
format:
	rebar3 fmt

# Check code
check:
	rebar3 fmt --check
	rebar3 dialyzer