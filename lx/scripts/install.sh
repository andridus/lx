#!/bin/bash

# LX Compiler Installation Script
# This script builds and installs the LX compiler with kernel modules

set -e

echo "LX Compiler Installation Script"
echo "================================"

# Check if V is available
if ! command -v v &> /dev/null; then
    echo "Error: V compiler not found in PATH"
    echo "Please install V from: https://vlang.io/"
    exit 1
fi

# Check if erlc is available
if ! command -v erlc &> /dev/null; then
    echo "Error: Erlang compiler (erlc) not found in PATH"
    echo "Please install Erlang/OTP"
    exit 1
fi

# Build and install
echo "Building LX compiler and kernel modules..."
make install

echo ""
echo "Installation completed successfully!"
echo ""
echo "You can now use:"
echo "  lx shell [project_dir]  # Start LX shell with kernel modules loaded"
echo "  lx compile <file.lx>    # Compile LX files"
echo "  lx run <file.lx>        # Run single LX file"
echo ""
echo "Kernel modules are installed in: ~/.lx/kernel/"
echo "To uninstall, run: make uninstall"