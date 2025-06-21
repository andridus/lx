# Makefile for lx-lang compiler
# OCaml compiler project using dune build system

.PHONY: all build install clean test run help dev watch format lint doc deps

# Default target
all: build

# Build the project
build:
	dune build

# Build and install the executable
install: build
	dune install

# Clean build artifacts
clean:
	dune clean
	rm -rf _build/

# Run tests
test: build
	dune exec tests/test_main.exe

# Run the compiler with example file
run: build
	dune exec lx my/test1.lx

# Development mode - build and run with example
dev: build run

# Watch mode for continuous compilation
watch:
	dune build --watch

# Format OCaml code
fmt:
	dune fmt

# Lint and check code quality
lint:
	dune build @check

# Generate documentation
doc:
	dune build @doc

# Install dependencies
deps:
	opam install . --deps-only

# Update dune to latest version
update-dune:
	opam update
	opam install dune --yes
	@echo "Current dune version: $$(dune --version)"
	@echo "Consider updating dune-project file if needed"

# Setup development environment
setup: deps
	opam install ocaml-lsp-server merlin utop -y

# Run REPL with project loaded
repl: build
	dune utop

# Build in release mode
release:
	dune build --profile release

# Run specific example file
run-example:
	@if [ -z "$(FILE)" ]; then \
		echo "Usage: make run-example FILE=path/to/file.lx"; \
		exit 1; \
	fi
	dune exec  --lx $(FILE)

# Show project status
status:
	@echo "=== LX-Lang Compiler Status ==="
	@echo "Build directory: _build/"
	@echo "Source files:"
	@find src/ -name "*.ml" -o -name "*.mli" -o -name "*.mll" -o -name "*.mly" | sort
	@echo "Test files:"
	@find tests/ -name "*.lx" | sort
	@echo "Executable: _build/default/src/main.exe"

# Help target
help:
	@echo "Available targets:"
	@echo "  all       - Build the project (default)"
	@echo "  build     - Build the project"
	@echo "  install   - Build and install the executable"
	@echo "  clean     - Clean build artifacts"
	@echo "  test      - Run tests"
	@echo "  run       - Build and run with example.lx"
	@echo "  dev       - Development mode (build + run)"
	@echo "  watch     - Continuous compilation mode"
	@echo "  format    - Format OCaml code"
	@echo "  lint      - Check code quality"
	@echo "  doc       - Generate documentation"
	@echo "  deps      - Install dependencies"
	@echo "  update-dune - Update dune to latest version"
	@echo "  setup     - Setup development environment"
	@echo "  repl      - Run REPL with project loaded"
	@echo "  release   - Build in release mode"
	@echo "  run-example FILE=path - Run with specific file"
	@echo "  status    - Show project status"
	@echo "  help      - Show this help message"