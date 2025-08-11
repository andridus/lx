.PHONY: all clean compile test install uninstall help

# Default target
all: compile

# Compile the project
compile: generate_parsers
	rebar3 compile

# Generate lexer and parser from .xrl and .yrl files
generate_parsers: src/lx0_lexer.erl src/lx0_parser.erl

# Generate lexer from .xrl file
src/lx0_lexer.erl: src/lx0_lexer.xrl
	erlc -o src $<

# Generate parser from .yrl file
src/lx0_parser.erl: src/lx0_parser.yrl
	erlc -o src $<

# Clean build artifacts
clean:
	rebar3 clean
	rm -f src/lx0_lexer.erl
	rm -f src/lx0_parser.erl
	rm -f lx0

# Run tests
test: compile
	rebar3 eunit

# Install dependencies
deps:
	rebar3 get-deps

# Create executable script
build: compile
	@echo '#!/bin/bash' > lx0
	@echo 'PROJECT_DIR="$$(cd "$$(dirname "$$0")" && pwd)"' >> lx0
	@echo 'cd "$$PROJECT_DIR"' >> lx0
	@echo 'rebar3 run -- lx0_cli:main(init:get_plain_arguments())' >> lx0
	@chmod +x lx0

# Create escript
escript: compile
	rebar3 escriptize

# Install the lx compiler to system
install: build
	@echo "Installing lx compiler..."
	@sudo cp lx0 /usr/local/bin/lx
	@sudo chmod +x /usr/local/bin/lx
	@echo "lx compiler installed to /usr/local/bin/lx"
	@echo "You can now use: lx compile <file.lx>"

# Install to user's local bin directory (no sudo required)
install-local: escript
	@echo "Installing lx compiler to ~/.local/bin..."
	@mkdir -p ~/.local/bin
	@cp _build/default/bin/lx ~/.local/bin/lx
	@chmod +x ~/.local/bin/lx
	@echo "lx compiler installed to ~/.local/bin/lx"
	@echo "Add ~/.local/bin to your PATH if not already there"
	@echo "You can now use: lx compile <file.lx>"

# Uninstall the lx compiler
uninstall:
	@echo "Uninstalling lx compiler..."
	@sudo rm -f /usr/local/bin/lx
	@echo "lx compiler uninstalled from /usr/local/bin/lx"

# Uninstall from user's local bin directory
uninstall-local:
	@echo "Uninstalling lx compiler from ~/.local/bin..."
	@rm -f ~/.local/bin/lx
	@echo "lx compiler uninstalled from ~/.local/bin/lx"

# Run the compiler
run: build
	./lx0

# Show help
help:
	@echo "Available targets:"
	@echo "  all            - Compile the project (default)"
	@echo "  compile        - Compile the project"
	@echo "  clean          - Clean build artifacts"
	@echo "  test           - Run tests"
	@echo "  deps           - Install dependencies"
	@echo "  build          - Create executable script"
	@echo "  install        - Install to /usr/local/bin (requires sudo)"
	@echo "  install-local  - Install to ~/.local/bin (no sudo required)"
	@echo "  uninstall      - Remove from /usr/local/bin"
	@echo "  uninstall-local- Remove from ~/.local/bin"
	@echo "  run            - Run the compiler"
	@echo "  help           - Show this help"
	@echo ""
	@echo "Installation options:"
	@echo "  make install        # System-wide installation (requires sudo)"
	@echo "  make install-local  # User-local installation (no sudo required)"