# LX Compiler Makefile
# Targets for compilation, testing, and validation

.PHONY: all compile test e2e-test add-main clean help

# Default target
all: compile

# Compile the LX compiler
compile:
	@echo "Compiling LX compiler..."
	v -o lx1 main.v
	@echo "LX compiler built successfully!"

# Run all tests
test:
	@echo "Running LX compiler tests..."
	v test .

# Add main functions to all examples
add-main:
	@echo "Adding main functions to all examples..."
	@chmod +x scripts/add_main_functions.sh
	@./scripts/add_main_functions.sh

# End-to-end test: compile and run all examples
e2e-test: add-main
	@echo "Running end-to-end validation of all examples..."
	@chmod +x scripts/validate_examples.sh
	@./scripts/validate_examples.sh

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.erl" -delete
	@find . -name "*.beam" -delete
	@find . -name "erl_crash.dump" -delete
	@echo "Clean complete!"

# Show help
help:
	@echo "LX Compiler Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  compile     - Build the LX compiler"
	@echo "  test        - Run all tests"
	@echo "  add-main    - Add main functions to all examples"
	@echo "  e2e-test    - Validate all examples end-to-end (includes add-main)"
	@echo "  clean       - Remove generated files"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make compile     # Build compiler"
	@echo "  make add-main    # Add main functions to examples"
	@echo "  make e2e-test    # Run all examples"
	@echo "  make clean       # Clean up"