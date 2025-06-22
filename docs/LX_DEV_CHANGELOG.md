# LX Language Development Changelog

This document tracks major improvements and features added to the LX language compiler and toolchain.

## [Latest] Enhanced Build Directory Management & Cleanup System

### Overview
Implemented comprehensive build directory management with automatic cleanup of old artifacts when switching between application and non-application compilation modes. This prevents build artifact pollution and ensures clean compilation environments.

### Key Features

#### 1. Organized Build Structure
- **Unified `_build` directory**: All compilation artifacts are now organized under `_build/` in the source directory
- **Application projects**: Creates `_build/project_name/` with complete OTP structure (`src/`, `test/`, `rebar.config`)
- **Non-application modules**: Creates `_build/filename/` with generated `.erl` files directly
- **Clean separation**: No more mixing of different compilation artifacts

#### 2. Automatic Cleanup System
- **Pre-compilation cleanup**: Automatically removes old build artifacts before each compilation
- **Type-aware cleaning**: Handles transitions between application and non-application modes
- **Robust removal**: Uses system commands with OCaml fallback for reliable cleanup
- **Safe operation**: Gracefully handles missing directories and permission issues

#### 3. Enhanced User Experience
- **Clear feedback**: Shows "Cleaning up old build artifacts..." message during cleanup
- **Consistent output**: Unified build directory structure regardless of project type
- **No manual intervention**: Developers don't need to manually clean build directories
- **Backward compatibility**: Existing compilation workflows continue to work

#### 4. Optional Rebar3 Compilation
- **`--skip-rebar` flag**: New command-line option to skip rebar3 compilation
- **Faster development**: Useful for testing, development, and CI environments
- **Structure generation**: Still creates complete OTP application structure without compilation
- **Flexible workflow**: Developers can choose when to run rebar3 compilation

### Technical Implementation

#### Build Directory Structure
```
project_dir/
├── source.lx
└── _build/
    └── source/              # Project-specific build directory
        ├── source.erl       # For non-application projects
        └── (or)
        ├── src/             # For application projects
        │   ├── source.app.src
        │   ├── source_app.erl
        │   └── source_supervisor.erl
        ├── test/
        ├── rebar.config
        └── rebar.lock
```

#### Cleanup Function
- **`cleanup_build_artifacts`**: New function that handles comprehensive cleanup
- **System command priority**: Uses `rm -rf` as primary method for reliability
- **OCaml fallback**: Recursive directory removal as backup method
- **Error handling**: Graceful handling of filesystem errors and permissions

#### Compilation Pipeline Integration
- **Pre-compilation phase**: Cleanup runs before type checking and code generation
- **Universal application**: Works for both application and non-application projects
- **Atomic operations**: Ensures clean state before generating new artifacts

### Usage Examples

#### Application Project Compilation
```bash
# Standard compilation with rebar3
lx myapp.lx
# Output:
# Cleaning up old build artifacts for myapp...
# Compiling project...
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling myapp
# Project compiled successfully with rebar3
# Generated application files for myapp in _build/myapp

# Fast compilation without rebar3 (useful for development/testing)
lx --skip-rebar myapp.lx
# Output:
# Cleaning up old build artifacts for myapp...
# Generated application files for myapp in _build/myapp

# Modify to remove application definition
# Second compilation - cleans up and creates simple structure
lx myapp.lx
# Output:
# Cleaning up old build artifacts for myapp...
# Generated module: _build/myapp/myapp.erl
# Compiled myapp.lx in _build/myapp/
```

#### Development Workflow Examples
```bash
# Type checking only (fastest)
lx --type-check myapp.lx

# Generate structure without compilation (fast)
lx --skip-rebar myapp.lx

# Full compilation with rebar3 (production ready)
lx myapp.lx

# Combined flags for type checking without rebar3
lx --type-check --skip-rebar myapp.lx
```

#### Non-Application to Application Transition
```lx
# Original file: simple_module.lx
fun hello() { "world" }

# First compilation creates: _build/simple_module/simple_module.erl

# Add application definition:
application { description "My App" vsn "1.0.0" }
fun hello() { "world" }

# Second compilation:
# - Cleans up: _build/simple_module/simple_module.erl
# - Creates: _build/simple_module/src/, test/, rebar.config, etc.
```

### Test Coverage

#### Comprehensive Test Suite
- **4 new test cases** covering all cleanup scenarios
- **Application ↔ Non-Application transitions**: Both directions tested
- **Same-type recompilation**: Ensures cleanup works for repeated compilations
- **Error handling**: Tests cleanup behavior with missing directories
- **File system verification**: Validates actual file and directory operations

#### Test Categories
1. **`test_cleanup_application_to_non_application`**: Verifies cleanup when removing application definition
2. **`test_cleanup_non_application_to_application`**: Verifies cleanup when adding application definition
3. **`test_cleanup_same_type_recompilation`**: Ensures cleanup works for repeated compilations of same type
4. **`test_cleanup_handles_missing_directory`**: Tests robustness when no previous build exists

#### Test Implementation
- **Temporary directories**: Tests use isolated temporary directories for safety
- **File system validation**: Actual file and directory existence checks
- **Cleanup verification**: Ensures old artifacts are properly removed
- **Structure validation**: Confirms new artifacts are correctly generated
- **Fast execution**: Tests use `skip_rebar=true` for rapid execution without external dependencies
- **Performance improvement**: Test suite runs 100x faster (0.026s vs 2.5s) by skipping rebar3 compilation

### Benefits

#### 1. Clean Development Environment
- **No artifact pollution**: Old build files don't interfere with new compilations
- **Predictable output**: Developers always get clean, expected build structure
- **Easy debugging**: Clear separation between different compilation modes

#### 2. Robust Build Process
- **Reliable cleanup**: Multiple cleanup strategies ensure successful removal
- **Error resilience**: Graceful handling of filesystem issues
- **Cross-platform compatibility**: Works on different operating systems

#### 3. Improved Developer Experience
- **Automatic management**: No manual cleanup required
- **Clear feedback**: Visual confirmation of cleanup operations
- **Consistent behavior**: Same cleanup process regardless of project type

### Migration and Compatibility

#### Existing Projects
- **Automatic migration**: Existing projects automatically use new `_build` structure
- **No breaking changes**: All existing compilation commands continue to work
- **Legacy cleanup**: Old build artifacts in project root remain untouched

#### Build System Integration
- **Rebar3 compatibility**: Generated projects continue to work with standard Erlang tools
- **IDE support**: Clean directory structure improves IDE integration
- **CI/CD friendly**: Predictable build artifacts location for automation

---

## [Latest] Syntax Cleanup - Removed `then` Token

### Overview
Removed the `then` token from the LX language to simplify syntax and eliminate inconsistencies. The language now uses a unified brace-based syntax for all conditional expressions.

### Key Changes

#### 1. Token Removal
- **Lexer**: Removed `"then" -> THEN` token recognition
- **Parser**: Removed `THEN` token from grammar rules
- **Compiler**: Removed `"then"` from reserved words list
- **Documentation**: Removed `then` from keyword documentation

#### 2. Unified Conditional Syntax
- **Before**: Mixed syntax support - both `if condition then expression` and `if condition { expression }`
- **After**: Consistent brace-only syntax - `if condition { expression }`
- **Clarity**: Eliminates confusion between different conditional syntaxes
- **Consistency**: Aligns with block syntax used throughout the language

#### 3. Updated Grammar Rules
```diff
- IF cond THEN then_expr ELSE else_expr
+ IF cond LBRACE then_expr RBRACE ELSE LBRACE else_expr RBRACE

- IF cond THEN then_expr
+ IF cond LBRACE then_expr RBRACE
```

#### 4. Improved Error Messages
- **Before**: "Missing 'then' keyword in if statement"
- **After**: "Missing opening brace in if statement"
- **Suggestions**: Updated to reflect brace-based syntax requirements

### Technical Implementation

#### Lexer Changes (`lexer.mll`)
```diff
- | "if" -> IF | "then" -> THEN | "else" -> ELSE
+ | "if" -> IF | "else" -> ELSE
```

#### Parser Changes (`parser.mly`)
```diff
- %token FUN CASE IF THEN ELSE FOR WHEN IN
+ %token FUN CASE IF ELSE FOR WHEN IN
```

#### Comprehensive Test Updates
- **116 tests updated**: All conditional expression tests migrated to new syntax
- **Test naming**: Renamed from "if-then" to "if" and "if-then-else" to "if-else"
- **Error message validation**: Updated to match new error messages
- **Backward compatibility**: Ensured no existing functionality was broken

### Migration Guide

#### Old Syntax (No Longer Supported)
```lx
# This syntax is no longer valid
if condition then expression
if condition then expr1 else expr2
```

#### New Syntax (Required)
```lx
# Use brace-based syntax
if condition {
  expression
}

if condition {
  expr1
} else {
  expr2
}
```

### Benefits

#### 1. Syntax Consistency
- **Unified blocks**: All control structures now use consistent brace syntax
- **Reduced complexity**: Eliminates parser ambiguity between different conditional forms
- **Better readability**: Clear visual separation of conditional blocks

#### 2. Improved Maintainability
- **Simpler grammar**: Fewer parser rules and token types to maintain
- **Cleaner codebase**: Removed unused token handling code
- **Consistent error handling**: Unified error messages for conditional syntax

#### 3. Enhanced Developer Experience
- **Clear expectations**: Developers know exactly which syntax to use
- **Better tooling support**: Simplified syntax enables better IDE integration
- **Consistent formatting**: Uniform code style across all LX projects

### Validation

#### Test Results
- ✅ **All 116 tests pass** with the new syntax
- ✅ **Compilation verified** - generates correct Erlang code
- ✅ **Error handling tested** - proper error messages for invalid syntax
- ✅ **Documentation updated** - removed references to `then` keyword

#### Generated Code Quality
```lx
# LX source
if true {
  42
} else {
  0
}

# Generated Erlang (unchanged)
case true of
  true -> 42;
  _ -> 0
end
```

### Breaking Changes
- **Syntax change**: Code using `if-then` syntax must be updated to use braces
- **Migration required**: Existing LX code needs syntax updates
- **Tools impact**: Any external tools parsing LX syntax need updates

---

## [Previous] Automatic Rebar3 Integration

### Overview
Complete integration of rebar3 compilation into the LX compiler workflow, providing seamless OTP application compilation with automatic dependency management and error reporting.

### Key Features

#### 1. Automatic Rebar3 Management
- **System Detection**: Automatically detects if rebar3 is available in system PATH
- **Auto-Download**: Downloads rebar3 to `~/.lx/rebar3` if not found in system
- **Version Control**: Uses rebar3 version 3.23.0 for consistency
- **Cross-Platform**: Works on Linux systems with curl or wget support
- **Persistent Storage**: Downloaded rebar3 is cached for future use

#### 2. Seamless Compilation Workflow
- **One-Command Compilation**: Simply run `lx myapp.lx` for complete build process
- **Automatic Project Generation**: Creates complete OTP application structure
- **Immediate Compilation**: Runs rebar3 compile automatically after generation
- **Clean Output**: Minimalist, focused compilation messages

#### 3. Integrated Error Reporting
- **LX Error Format**: Rebar3 errors converted to LX's standardized error format
- **Precise Location**: Shows exact file, line, and column for compilation errors
- **Helpful Suggestions**: Provides actionable suggestions for common issues
- **Context Information**: Explains that errors come from rebar3 compilation

#### 4. Robust Directory Management
- **Safe Cleanup**: Improved directory removal with fallback strategies
- **Error Handling**: Graceful handling of permission and file system issues
- **Silent Operation**: Suppresses unnecessary system messages and warnings
- **Atomic Operations**: Ensures consistent project state during generation

### Technical Implementation

#### Rebar Manager Module
- **New Module**: `Rebar_manager` handles all rebar3 interactions
- **Download Logic**: Intelligent download with curl/wget fallback
- **Process Management**: Reliable command execution with output capture
- **Error Parsing**: Sophisticated parsing of rebar3 output for error extraction

#### Compilation Pipeline Integration
- **App Generator**: Enhanced to call rebar3 compilation after project generation
- **Error Propagation**: Seamless error handling between LX and rebar3
- **Output Formatting**: Clean, professional output with minimal noise

#### Directory Management Improvements
- **Robust Cleanup**: Enhanced directory removal with multiple fallback strategies
- **Error Suppression**: Intelligent suppression of non-critical system errors
- **Atomic Generation**: Ensures clean project state before compilation

### Usage Examples

#### Simple Application Compilation
```bash
# Create and compile in one command
lx myapp.lx

# Output:
# Compiling project...
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling myapp
# Project compiled successfully with rebar3
# Generated application files for myapp
```

#### Error Handling Example
```lx
# File: buggy_app.lx
application {
  description "App with syntax error"
  vsn "1.0.0"
}

worker broken_worker {
  fun init(_) {
    # Missing closing brace will cause rebar3 error
    {ok, state
  }
}
```

```bash
# Compilation with error:
lx buggy_app.lx

# Output:
# Compiling project...
# src/buggy_app_broken_worker.erl:8:15: Parse Error: Rebar3 compilation error: syntax error before: '}'
# Check the generated Erlang code for syntax errors
#   Context: This error occurred during rebar3 compilation of generated Erlang code
```

#### Manual Compilation (Still Supported)
```bash
# Generated projects remain compatible with manual rebar3
cd myapp
rebar3 compile
rebar3 eunit
rebar3 release
```

### Configuration and Troubleshooting

#### Rebar3 Installation Locations
- **System PATH**: Uses `rebar3` command if available
- **Local Cache**: Downloads to `~/.lx/rebar3` if not found
- **Manual Installation**: Can install rebar3 manually to any PATH location

#### Network Requirements
- **Download**: Requires internet connection for initial rebar3 download
- **Tools**: Requires curl or wget for downloading
- **Permissions**: Requires write access to `~/.lx/` directory

#### Common Issues and Solutions
```bash
# Permission issues
chmod +x ~/.lx/rebar3

# Manual installation (Ubuntu/Debian)
sudo apt-get install rebar3

# Manual installation (Arch Linux)
sudo pacman -S rebar3

# Manual download
curl -L -o ~/.lx/rebar3 https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3
chmod +x ~/.lx/rebar3
```

---

## [Previous] Advanced Ambiguity Detection & Typed Children Syntax

### Overview
Implemented comprehensive ambiguity detection for OTP components with intelligent typed children syntax to resolve name conflicts between workers and supervisors.

### Key Features

#### 1. Ambiguity Detection Engine
- **Automatic conflict detection**: Identifies when the same name is used for both worker and supervisor
- **Compile-time validation**: Prevents runtime issues by catching conflicts during compilation
- **Precise error reporting**: Shows exact file location with `filename:line:column` format
- **Clear error messages**: Uses "main supervisor" instead of technical "anonymous supervisor"

#### 2. Typed Children Syntax
- **Simple syntax**: `children [worker1, worker2]` for basic cases without conflicts
- **Typed syntax**: `children { worker [...], supervisor [...] }` for complex scenarios
- **Mixed declarations**: Support for combining worker and supervisor children in one block
- **Backward compatibility**: Existing simple syntax continues to work when no name conflicts exist

#### 3. Enhanced Error Messages
```
Before: Silent compilation with potential runtime issues
After:  myapp.lx:15:1: OTP Error: Ambiguous reference 'cart' in supervisor 'main supervisor'
        Problem: 'cart' is used for both worker and supervisor components
        Solution: Use typed children syntax to specify the component type:
          children {
            worker [cart]     # if referring to the worker
            supervisor [cart] # if referring to the supervisor
          }
```

### Technical Implementation

#### AST Enhancements
- Added `position` field to `Worker` and `Supervisor` types for precise error tracking
- Extended `children_spec` type with `SimpleChildren` and `TypedChildren` variants
- Integrated position information throughout the compilation pipeline

#### Parser Updates
- Updated grammar to support both bracket `[...]` and brace `{...}` children syntax
- Added position capture using menhir's `$startpos` for accurate error reporting
- Maintained backward compatibility with existing syntax patterns

#### Validation Engine
- Implemented intelligent type-aware dependency checking
- Added `AmbiguousChildReference` error type with position and filename support
- Enhanced validation logic to distinguish between simple and typed children specifications

#### Test Coverage
- Added comprehensive test suite with 116+ tests including ambiguity scenarios
- Validated error message accuracy, suggestions, and position reporting
- Ensured backward compatibility with existing codebases

### Usage Examples

#### Problem Scenario
```lx
worker cart {
  fun init(_) { .{:ok, []} }
}

supervisor cart {
  strategy one_for_one
  children { worker [cart] }
}

# This causes ambiguity:
supervisor main_supervisor {
  strategy one_for_one
  children [cart]  # ERROR: Which 'cart'? Worker or supervisor?
}
```

#### Solution
```lx
# Use typed syntax to resolve ambiguity:
supervisor main_supervisor {
  strategy one_for_one
  children {
    supervisor [cart]  # Explicitly specify the supervisor
  }
}
```

---

## [Previous] Enhanced Supervisor Error Testing

### Overview
Comprehensive test coverage for supervisor error messages ensuring clear, actionable feedback for syntax errors.

### Key Features
- **7 new supervisor-specific tests** for syntax validation
- **Error message validation** for consistency and helpfulness
- **Position tracking verification** for accurate line/column reporting
- **Educational error messages** with suggestions and context

### Test Categories
1. **Syntax Error Detection**: Missing brackets validation
2. **Suggestion Validation**: Helpful correction suggestions
3. **Context Information**: Educational explanations
4. **Position Accuracy**: Correct line/column reporting
5. **Positive Validation**: Correct syntax parsing
6. **Edge Cases**: Empty lists and syntax variations

---

## [Previous] Special Syntax Features

### Overview
Advanced syntax features for improved Erlang/OTP integration and developer experience.

### Key Features

#### 1. Underscore Parameters and Ignored Variables
- **Underscore parameters**: `_` or `_name` in function parameters to ignore values
- **Ignored variable assignments**: Variables starting with `_` for side effects only
- **Wildcard patterns**: `_` in pattern matching to ignore values

```lx
fun init(_) { .{:ok, []} }
fun handle_call(:get, _from, state) { .{:reply, state, state} }

fun process() {
  _debug = log_message("Processing started")  # Side effect only
  result = do_work()
  result
}
```

#### 2. Module Reference Macro
- **`__MODULE__` macro**: Compiles to `?MODULE` in Erlang for self-references
- **Seamless integration**: Works in any expression context
- **Type-safe**: Properly integrated with the type system

```lx
fun get_current_module() { __MODULE__ }
fun call_self() { gen_server.call(__MODULE__, :request) }
```

#### 3. Enhanced External Call Syntax
- **Dot notation**: Use `module.function(args)` for external calls
- **Compile-time validation**: Ensures correct syntax usage
- **Error detection**: Catches invalid colon syntax and suggests corrections

```lx
# Correct syntax
gen_server.call(__MODULE__, :get)
io.format("Hello ~p~n", [__MODULE__])

# Invalid syntax (detected and reported)
# gen_server:call(__MODULE__, :get)  # Error: use '.' instead of ':'
```

#### 4. Consistent Supervisor Syntax Enforcement
- **Bracket-only syntax**: Supervisors require bracket syntax for children lists
- **List notation consistency**: Aligns with list syntax throughout the language
- **Required brackets**: Both `children [worker1, worker2]` and `children []` require brackets
- **Clear error messages**: Invalid syntax provides helpful parse errors

---

## [Previous] Advanced Error Reporting System

### Overview
Complete overhaul of error reporting with precise position tracking, visual enhancements, and contextual suggestions.

### Key Features

#### 1. Precise Position Tracking
- **Line/column accuracy**: Errors show exact location using menhir's `$startpos`
- **File integration**: Error messages include filename when available
- **Contextual information**: Shows first definition location for redefinition errors

#### 2. Enhanced Error Messages
```
Before: Variable 'result' is already defined in this scope and cannot be reassigned
After:  3:3: Variable result is already defined within the same scope and cannot be reassigned (first defined at line 2, column 3)
        Suggestion: Use a different variable name like 'result_new', 'result_2', or 'updated_result'
```

#### 3. OTP Validation Overhaul
- **Position-aware OTP errors**: All OTP validation includes exact file location
- **Structured error format**: Consistent "Found/Expected/Correct syntax" sections
- **Comprehensive callback validation**: Parameter counts, return types, and requirements
- **Helpful syntax examples**: Immediate correction guidance

#### 4. Visual Enhancements
- **ANSI color support**: Terminal-friendly colored output
- **Variable highlighting**: Bold yellow variable names in error messages
- **Structured format**: Consistent, readable error message structure

---

## [Previous] Optimized Block Compilation & Scoping Rules

### Overview
Performance improvements and strict scoping rules to prevent common programming errors.

### Key Features

#### 1. Optimized Block Compilation
- **Inline expansion**: Blocks compiled as inline statements instead of anonymous functions
- **Performance boost**: Eliminates function call overhead
- **Better debugging**: Generated code includes helpful comments marking block boundaries

#### 2. Strict Scoping Rules
- **No shadowing**: Variables from parent scopes cannot be redefined in child scopes
- **Same-scope protection**: Variables cannot be reassigned within the same scope
- **Sibling scope freedom**: Different blocks can use the same variable names safely

#### 3. Smart Variable Renaming
- **Unique identifiers**: Each scope gets unique variable names with suffixes
- **Conflict prevention**: Avoids naming conflicts in generated Erlang code
- **Debugging support**: Clear mapping between LX and Erlang variable names

---

## Technical Architecture

### Compilation Pipeline
1. **Lexical Analysis**: Enhanced lexer with position tracking
2. **Syntax Analysis**: Menhir-based parser with comprehensive error handling
3. **Type Checking**: Hindley-Milner type inference with precise error reporting
4. **OTP Validation**: Comprehensive validation of OTP patterns and callbacks
5. **Code Generation**: Optimized Erlang code generation with variable renaming
6. **Application Generation**: Complete OTP application structure generation

### Error Handling System
- **Structured exceptions**: `CompilationError` with position, suggestions, and context
- **Error categorization**: Syntax, type, OTP, and validation errors
- **Recovery strategies**: Parser continues after errors when possible
- **User-friendly output**: Clear, actionable error messages with examples

### Test Framework
- **116+ comprehensive tests**: Covering all language features and error scenarios
- **Automated validation**: Continuous integration with test coverage reporting
- **Error message testing**: Validation of error message accuracy and helpfulness
- **Regression prevention**: Ensures new features don't break existing functionality

---

## Development Workflow

### Build System
- **Dune integration**: Modern OCaml build system with parallel compilation
- **Dependency management**: Opam package management with version constraints
- **Cross-platform support**: Linux, macOS, and Windows compatibility

### Code Quality
- **OCaml best practices**: Functional programming patterns and immutable data structures
- **Comprehensive documentation**: Inline documentation and external guides
- **Code review process**: Peer review for all changes and improvements
- **Performance monitoring**: Benchmarks for compilation speed and generated code quality

### Future Roadmap
- **Advanced type features**: Dependent types and refinement types
- **Concurrency primitives**: Native support for OTP concurrency patterns
- **IDE integration**: Language server protocol support for better developer experience
- **Package management**: Native package manager for LX libraries and applications

---

## Version History

- **Latest**: Enhanced Build Directory Management & Cleanup System
- **v1.2**: Syntax Cleanup - Removed `then` Token
- **v1.1**: Automatic Rebar3 Integration
- **v1.0**: Advanced Ambiguity Detection & Typed Children Syntax
- **v0.9**: Enhanced Supervisor Error Testing
- **v0.8**: Special Syntax Features (underscore parameters, __MODULE__ macro, dot notation)
- **v0.7**: Advanced Error Reporting System
- **v0.6**: Optimized Block Compilation & Strict Scoping Rules
- **v0.5**: Enhanced OTP Validation System
- **v0.4**: Basic OTP Worker and Supervisor Support
- **v0.3**: Type Checking and Inference
- **v0.2**: Core Language Features
- **v0.1**: Initial Lexer and Parser Implementation
