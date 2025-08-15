# LX Compiler Makefile
# Targets for compilation, testing, and validation

.PHONY: all compile test e2e-test add-main clean help install uninstall dev

# Default target
all: compile

# Compile the LX compiler
compile:
	@echo "Compiling LX compiler..."
	v .
	@echo "LX compiler built successfully!"

# Build kernel modules
kernel: compile
	@echo "Building kernel modules..."
	@mkdir -p _build/kernel
	@for file in kernel/*.lx; do \
		if [ -f "$$file" ]; then \
			echo "  Compiling $$file..."; \
			./lx compile "$$file" --no-rebar-compile; \
			module_name=$$(basename "$$file" .lx); \
			if [ -f "$$module_name.erl" ]; then \
				erlc -o _build/kernel "$$module_name.erl"; \
				rm "$$module_name.erl"; \
			elif [ -f "kernel/$$module_name.erl" ]; then \
				erlc -o _build/kernel "kernel/$$module_name.erl"; \
				rm "kernel/$$module_name.erl"; \
			fi; \
		fi; \
	done
	@echo "Kernel modules built successfully!"

# Install LX and kernel modules to system
install: kernel
	@echo "Installing LX compiler and kernel modules..."
	@mkdir -p ~/.lx/kernel
	@cp _build/kernel/*.beam ~/.lx/kernel/ 2>/dev/null || true
	@echo "LX compiler installed to ~/.lx/kernel/"
	@sudo ./lx symlink --force
	@echo "You can now use 'lx shell' to start the LX shell"

# Development target: build, install and run tests
dev: install test
	@echo "Development build completed!"

# Uninstall LX from system
uninstall:
	@echo "Uninstalling LX compiler..."
	@rm -rf ~/.lx
	@echo "LX compiler uninstalled!"

# Run all tests
test:
	@echo "Running LX compiler tests..."
	v test .

# End-to-end test: compile and run all examples
e2e-test:
	@echo "Running end-to-end validation of all examples..."
	@chmod +x scripts/validate_examples.sh
	@./scripts/validate_examples.sh

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.erl" -delete
	@find . -name "*.beam" -delete
	@find . -name "erl_crash.dump" -delete
	@rm -rf _build
	@echo "Clean complete!"

# Show help
help:
	@echo "LX Compiler Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  compile     - Build the LX compiler"
	@echo "  kernel      - Build kernel modules (.lx -> .beam)"
	@echo "  install     - Install LX and kernel modules to ~/.lx/kernel"
	@echo "  uninstall   - Remove LX installation from system"
	@echo "  dev         - Build, install and run tests"
	@echo "  test        - Run all tests"
	@echo "  e2e-test    - Validate all examples end-to-end"
	@echo "  clean       - Remove generated files"
	@echo "  help        - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make install     # Full build and install"
	@echo "  make dev         # Build, install and test"
	@echo "  make kernel      # Build kernel modules only"
	@echo "  make clean       # Clean up"