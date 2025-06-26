# LX Compiler Makefile

.PHONY: build test clean help

# Default target
all: build

# Build the compiler
build:
	@echo "Building LX compiler..."
	v build -o lx_compiler .

# Run tests
test:
	@echo "Running tests..."
	v test .

# Run specific test file
test-file:
	@echo "Running test file..."
	v test $(FILE)

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	rm -f lx_compiler
	rm -rf _build

# Install dependencies
deps:
	@echo "Installing dependencies..."
	v install

# Format code
fmt:
	@echo "Formatting code..."
	v fmt -w .

# Check code
check:
	@echo "Checking code..."
	v check .

# Run with help
help:
	@echo "Available targets:"
	@echo "  build     - Build the LX compiler"
	@echo "  test      - Run all tests"
	@echo "  test-file - Run specific test file (FILE=path/to/test.v)"
	@echo "  clean     - Clean build artifacts"
	@echo "  deps      - Install dependencies"
	@echo "  fmt       - Format code"
	@echo "  check     - Check code"
	@echo "  help      - Show this help"