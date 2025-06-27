## [Unreleased]

### Added
- **Parser Base Structure**: Complete recursive descent parser implementation with token management, error recovery, and precedence table
- **Error Recovery System**: Robust error handling that continues parsing after encountering invalid tokens, synchronizing to block boundaries (def, defp, worker, supervisor, etc.)
- **Expression Parsing**: Comprehensive parsing of literals, variables, assignments, and function calls including external module calls (mod.fun)
- **Data Structures Parsing**: Complete parsing of tuples, lists, cons expressions, and maps with nested structure support
- **Operator Precedence**: Full implementation of operator precedence and associativity for arithmetic, comparison, and logical operators
- **Recovery Strategy**: Parser continues processing multiple expressions even when errors occur, reporting all errors found in the code
- **Guards (when expressions)**: Complete implementation of Erlang-style guards with support for type tests, logical operators (and, or, not, andalso, orelse), and operator conversion (!= to /=, <= to =<)
- **Send operator (!)**: Message passing operator for inter-process communication with proper precedence and AST structure
- **Receive expressions**: Blocking pattern matching for process messages with timeout support and guard expressions
- **Records**: Structured data types with named fields, including creation, field access, updates, and pattern matching
- **Maps**: Key-value data structures with atom keys (colon syntax) and general keys (arrow syntax), including creation, access, and pattern matching
- **Fun expressions**: Anonymous functions with support for single and multi-clause definitions, pattern matching, and closures
- **Binary/Bitstring support**: Complete implementation for binary creation, pattern matching, and type specifications
- **List Comprehensions (for loops)**: Enhanced for loops with guards, pattern matching, and complex data structure iteration
- **Match/Rescue expressions**: Error handling construct with individual and sequential pattern matching steps
- **OTP Components**: Worker and supervisor definitions with validation and proper callback requirements
- **Module System**: Dependency management with global and per-file declarations, type validation via BEAM files
- **Enhanced Pattern Matching**: Support for complex patterns including maps, records, tuples, and nested structures
- **Type System Improvements**: Comprehensive type checking for all new constructs with proper error reporting

### Fixed
- Fixed parser error handling to register errors but continue parsing, ensuring all syntax errors are reported
- Fixed parser synchronization to skip tokens until finding block boundaries after encountering errors
- Fixed operator precedence parsing to correctly handle complex expressions with multiple operators
- Fixed data structure parsing to support nested tuples, lists, and maps with proper error recovery
- Fixed `utils.unescape_string` to use `result += s[i..i+1]` for correct string handling, ensuring all foundation tests pass and proper V idioms are followed.
- Fixed string literal parsing in lexer to correctly handle escaped quotes (`\"`) and prevent premature string termination.
- Fixed lexer transitions for strings to properly validate invalid characters (like `@`) within string literals.
- Fixed record update compilation to use correct record type names instead of generic #record syntax
- Fixed map pattern matching to handle mixed key types and provide clear error messages for missing fields
- Fixed guard compilation to properly convert Lx operators to Erlang equivalents
- Fixed send operator precedence and associativity for correct message passing semantics
- Fixed receive expression compilation to generate proper Erlang receive blocks with timeout handling
- Fixed fun expression compilation to support both simple and multi-clause anonymous functions
- Fixed binary pattern matching to handle complex specifications and size expressions
- Fixed OTP validator to properly check required callbacks and component structure
- Fixed module generation to handle dependencies and generate correct Erlang module structure