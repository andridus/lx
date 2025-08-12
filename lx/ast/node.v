module ast

pub struct Node {
pub:
	id       int
	kind     NodeKind
	value    string
	children []Node
	position Position
}

pub enum NodeKind {
	// Literals
	integer
	float
	string
	boolean
	atom
	nil

	// Variables
	variable_binding // x = value
	variable_ref     // x (usage)
	identifier       // Generic identifier (including function args)

	// Blocks
	block // do ... end or -> ... end (multiple expressions)

	// Function structure
	function           // def function_name(args) do body end or (args) -> body
	function_parameter // function parameter definition
	lambda_expression  // fn(x) -> body end

	// Module structure
	module

	// Binary operators
	function_caller // +(a, b), *(a, b), >(a, b), etc.
	external_function_call // module:function(args) - external module function calls
	parentheses     // (expression)
	directive_call  // $print(a), $type(a)

	// List Operations
	list_literal // [1, 2, 3] ou []
	list_cons    // [head | tail]

	// Tuple Operations
	tuple_literal // {1, 2, 3} ou {}

	// Map Operations
	map_literal // %{key: value} ou %{}
	map_access  // map[key]

	// Record Operations
	record_definition // record Person { name :: string, age :: integer }
	record_field      // name :: string or name = value :: type or name = value
	record_literal    // Person{name: "João", age: 30}
	record_access     // user.name
	record_update     // %{record | campo: valor}

	// Pattern Matching
	case_expression // case expr do ... end
	case_clause     // pattern -> expression
	pattern_match   // pattern in case clause
	pattern_binding // [head | tail] = list

	// Type System
	type_alias      // type Name = Type
	type_annotation // :: Type

	// Task 11: Control Flow
	if_expr     // if condition do ... else ... end
	with_expr   // with pattern <- expr do ... else ... end
	clause_list // list of with clauses
	match_expr  // match pattern <- expr [rescue error do ... end] [continuation]

	// Task 11: Concurrency and Processes
	spawn_expr     // spawn(fn -> ... end)
	send_expr      // pid ! message
	receive_expr   // receive do pattern -> expr; pattern -> expr end
	supervisor_def // supervisor name do ... end
	worker_def     // worker name do ... end

	// Task 11: Binaries and Bitstrings
	binary_literal // <<1, 2, 3>>, <<"hello">>
	binary_pattern // <<version:8, data:32/binary>>
	binary_segment // segment within binary

	// Task 11: Custom Types
	type_def       // type status :: :ok | :error | :pending
	union_type     // :ok | :error | :pending
	generic_type   // result(T)
	opaque_type    // opaque user_id :: integer
	nominal_type   // nominal email :: string
	recursive_type // type list(T) :: [] | {T, list(T)}

	// Task 11: Module System
	deps_declaration   // deps [:cowboy, :outro_modulo]
	application_config // application { ... }
	import_statement   // import Module

	// Task 11: Advanced Features
	string_interpolation // "Hello, #{name}!"
	anonymous_function   // fn(x) -> x * 2 end
	lambda_call          // fun.(args)
	list_comprehension   // for x in list when condition do expr end
	directive            // @doc, @spec
	test_block           // describe, test, assert
}

pub struct Position {
pub:
	line   int
	column int
	file   string
}

pub struct Type {
pub:
	name   string
	params []Type
	// Support for specialized types (e.g., :ok is a specialized atom)
	specialized_value ?string
}

// Task 11: Binary segment for bitstring operations
pub struct BinarySegment {
pub:
	value    Node
	size     ?Node
	options  []string
	position Position
}

// Task 11: Type definition structures
pub struct TypeDef {
pub:
	name       string
	parameters []string
	variants   []TypeVariant
	position   Position
}

pub struct TypeVariant {
pub:
	name     string
	types    []Type
	position Position
}

// Task 11: String interpolation segment
pub struct InterpolationSegment {
pub:
	is_expression bool
	content       string
	expression    ?Node
	position      Position
}

// Helper methods for Node
pub fn (n Node) str() string {
	return '{${n.kind}, [id: ${n.id}, pos: ${n.position}], ${n.value}, ${n.children}}'
}

pub fn (p Position) str() string {
	return '${p.file}:${p.line}:${p.column}'
}

pub fn (t Type) str() string {
	// If it's a specialized type, show the specialized value
	if specialized := t.specialized_value {
		if t.params.len == 0 {
			return '${t.name}(${specialized})'
		}
		params_str := t.params.map(it.str()).join(', ')
		return '${t.name}(${specialized}, ${params_str})'
	}

	if t.params.len == 0 {
		return t.name
	}
	params_str := t.params.map(it.str()).join(', ')
	return '${t.name}(${params_str})'
}

pub fn (n Node) tree_str(indent int) string {
	pad := '  '.repeat(indent)
	mut result := '${pad}{${n.kind}, [], ${n.value}, ['

	if n.children.len > 0 {
		result += '\n'
		for i, child in n.children {
			result += child.tree_str(indent + 1)
			if i < n.children.len - 1 {
				result += ','
			}
			result += '\n'
		}
		result += '${pad}]'
	} else {
		result += ']'
	}

	result += '}'
	return result
}
