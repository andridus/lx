# Lx Language Support for VSCode

This extension provides comprehensive syntax highlighting and language support for the Lx programming language.

## Features

- **Syntax Highlighting**: Full support for Lx syntax including:
  - Function definitions (`def`, `defp`, `fn`)
  - Control flow (`if`, `case`, `with`, `for`, `receive`)
  - OTP components (`worker`, `supervisor`, `application`)
  - Data structures (maps, records, tuples, lists, binaries)
  - Pattern matching and operators
  - Type specifications and guards
  - Testing framework (`describe`, `test`, `assert`)
  - **Type System**: Type aliases, annotations, and modifiers (`type`, `opaque`, `nominal`, `::`)
  - **Directives**: Compile-time metadata (`@reflection`, `@inline`, `@deprecated`)

- **Language Features**:
  - Auto-closing brackets and quotes
  - Auto-surrounding with brackets
  - Smart indentation for Lx code blocks
  - Code folding with region markers
  - Word pattern matching for Lx identifiers

- **Color Theme**: Custom dark theme optimized for Lx syntax

## Supported Syntax

### Type System
- **Type Aliases**: `type name :: type_expression`
- **Opaque Types**: `type opaque name :: type_expression`
- **Nominal Types**: `type nominal name :: type_expression`
- **Type Annotations**: `variable :: type` and `function(param :: type) :: return_type`
- **Union Types**: `type number :: integer | float`
- **Function Types**: `(integer, string) -> boolean`

### Directives
- **`@reflection`**: Prints detailed type information during compilation
- **`@inline`**: Marks function for inlining optimization
- **`@deprecated`**: Marks function as deprecated

### Data Structures
- **Maps**: `%{ key: value, "key" => value }`
- **Records**: `Record{ field: value }`
- **Tuples**: `.{ value1, value2 }`
- **Lists**: `[1, 2, 3]` and `[head | tail]`
- **Binaries**: `<<1, 2, 3>>` with type specifiers

### Functions and Control Flow
- **Function definitions**: `def func() do ... end`
- **Anonymous functions**: `fn(x) do x * 2 end`
- **Pattern matching**: `case value do ... end`
- **With expressions**: `with pattern <= expr do ... end`
- **Receive expressions**: `receive do ... after ... end`

### OTP Components
- **Workers**: `worker name do ... end`
- **Supervisors**: `supervisor name do ... end`
- **Applications**: `application do ... end`

## Usage

1. Install this extension in VSCode
2. Open any `.lx` file to see syntax highlighting
3. The extension will automatically provide language features

## Contributing

Pull requests are welcome! For major changes, please open an issue first to discuss what you would like to change.

## License

MIT
