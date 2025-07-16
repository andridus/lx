module analysis

// TypeTable manages the mapping between AST node IDs and their type information
// This is the core of the Hindley-Milner implementation strategy
pub struct TypeTable {
pub mut:
	types          map[int]TypeInfo // ast_id -> TypeInfo
	next_id        int = 1 // Next available ID (start with 1, 0 reserved for errors)
	debug_mode     bool     // Enable detailed logging
	operations_log []string // Log of all operations for debugging
}

// new_type_table creates a new empty type table
pub fn new_type_table() TypeTable {
	return TypeTable{
		types:          map[int]TypeInfo{}
		next_id:        1
		debug_mode:     false
		operations_log: []string{}
	}
}

// new_type_table_with_debug creates a new type table with debug mode enabled
pub fn new_type_table_with_debug() TypeTable {
	mut tt := new_type_table()
	tt.debug_mode = true
	tt.log_operation('TypeTable created with debug mode enabled')
	return tt
}

// enable_debug enables debug mode
pub fn (mut tt TypeTable) enable_debug() {
	tt.debug_mode = true
	tt.log_operation('Debug mode enabled')
}

// disable_debug disables debug mode
pub fn (mut tt TypeTable) disable_debug() {
	tt.debug_mode = false
	tt.log_operation('Debug mode disabled')
}

// log_operation logs an operation if debug mode is enabled
fn (mut tt TypeTable) log_operation(operation string) {
	if tt.debug_mode {
		tt.operations_log << operation
		println('[TYPE_TABLE] ${operation}')
	}
}

// assign_type assigns a type to an AST node ID
pub fn (mut tt TypeTable) assign_type(ast_id int, type_info TypeInfo) {
	if ast_id <= 0 {
		panic('Invalid ast_id: ${ast_id}')
	}

	old_type := tt.types[ast_id] or { TypeInfo{} }
	tt.types[ast_id] = type_info

	if tt.debug_mode {
		if ast_id in tt.types && old_type.generic != '' {
			tt.log_operation('REASSIGN: AST ID ${ast_id} changed from ${old_type} to ${type_info}')
		} else {
			tt.log_operation('ASSIGN: AST ID ${ast_id} <- ${type_info}')
		}
	}
}

// assign_type_with_pos assigns a type to an AST node ID with position information
pub fn (mut tt TypeTable) assign_type_with_pos(ast_id int, type_info TypeInfo, pos string, expr_text string) {
	if ast_id <= 0 {
		panic('Invalid ast_id: ${ast_id}')
	}

	old_type := tt.types[ast_id] or { TypeInfo{} }
	tt.types[ast_id] = type_info

	if tt.debug_mode {
		if ast_id in tt.types && old_type.generic != '' {
			tt.log_operation('REASSIGN: ${pos} "${expr_text}" :: ${old_type} -> ${type_info}')
		} else {
			tt.log_operation('ASSIGN: ${pos} "${expr_text}" :: ${type_info}')
		}
	}
}

// get_type retrieves the type for an AST node ID
pub fn (tt TypeTable) get_type(ast_id int) ?TypeInfo {
	if tt.debug_mode {
		if ast_id in tt.types {
			type_info := tt.types[ast_id]
			println('[TYPE_TABLE] GET: AST ID ${ast_id} -> ${type_info} (generic: "${type_info.generic}")')
			return type_info
		} else {
			println('[TYPE_TABLE] GET: AST ID ${ast_id} -> NOT FOUND')
			return none
		}
	}

	// Non-debug version
	if ast_id in tt.types {
		return tt.types[ast_id]
	}
	return none
}

// generate_id generates a new unique AST ID
pub fn (mut tt TypeTable) generate_id() int {
	id := tt.next_id
	tt.next_id++
	tt.log_operation('GENERATE_ID: Generated new AST ID ${id}')
	return id
}

// has_type checks if an AST node ID has a type assigned
pub fn (tt TypeTable) has_type(ast_id int) bool {
	exists := ast_id in tt.types
	if tt.debug_mode {
		println('[TYPE_TABLE] HAS_TYPE: AST ID ${ast_id} -> ${exists}')
	}
	return exists
}

// debug_print prints the entire type table for debugging
pub fn (tt TypeTable) debug_print() {
	println('=== TYPE TABLE DEBUG ===')
	println('Total types: ${tt.types.len}')
	println('Next ID: ${tt.next_id}')
	println('Debug mode: ${tt.debug_mode}')
	println('Operations logged: ${tt.operations_log.len}')

	println('\nType assignments:')
	for ast_id, type_info in tt.types {
		println('  AST ID ${ast_id}: ${type_info}')
	}

	if tt.debug_mode && tt.operations_log.len > 0 {
		println('\nOperations log:')
		for i, op in tt.operations_log {
			println('  ${i + 1}: ${op}')
		}
	}
	println('========================')
}

// debug_print_summary prints a summary of the type table
pub fn (tt TypeTable) debug_print_summary() {
	println('=== TYPE TABLE SUMMARY ===')
	println('Total types: ${tt.types.len}')
	println('Next ID: ${tt.next_id}')

	// Group types by generic
	mut type_counts := map[string]int{}
	for _, type_info in tt.types {
		generic_str := type_info.generic
		type_counts[generic_str] = type_counts[generic_str] + 1
	}

	println('Type distribution:')
	for generic, count in type_counts {
		println('  ${generic}: ${count}')
	}
	println('=========================')
}

// validate_completeness validates that all expected AST nodes have types
pub fn (tt TypeTable) validate_completeness(expected_ids []int) ![]int {
	mut missing := []int{}

	for ast_id in expected_ids {
		if !tt.has_type(ast_id) {
			missing << ast_id
		}
	}

	if tt.debug_mode {
		if missing.len > 0 {
			println('[TYPE_TABLE] VALIDATION: Missing types for AST IDs: ${missing}')
		} else {
			println('[TYPE_TABLE] VALIDATION: All expected AST IDs have types')
		}
	}

	return missing
}

// get_all_types returns all types in the table
pub fn (tt TypeTable) get_all_types() map[int]TypeInfo {
	return tt.types
}

// clear removes all types from the table
pub fn (mut tt TypeTable) clear() {
	tt.types.clear()
	tt.next_id = 1
	tt.operations_log.clear()
	tt.log_operation('TypeTable cleared')
}

// get_operations_log returns the operations log
pub fn (tt TypeTable) get_operations_log() []string {
	return tt.operations_log
}

// print_operations_log prints the operations log
pub fn (tt TypeTable) print_operations_log() {
	println('=== TYPE TABLE OPERATIONS LOG ===')
	for i, op in tt.operations_log {
		println('${i + 1}: ${op}')
	}
	println('================================')
}

// print_detailed_type_assignments prints type assignments with source code context
pub fn (tt TypeTable) print_detailed_type_assignments() {
	println('=== DETAILED TYPE ASSIGNMENTS ===')

	// Group by position for better readability
	mut assignments_by_line := map[int][]string{}

	for op in tt.operations_log {
		if op.starts_with('ASSIGN: ') {
			// Extract line number from position
			parts := op.split(' ')
			if parts.len >= 2 {
				pos_part := parts[1]
				if pos_part.contains(':') {
					line_parts := pos_part.split(':')
					if line_parts.len >= 2 {
						line_num := line_parts[0].int()
						if line_num !in assignments_by_line {
							assignments_by_line[line_num] = []
						}
						assignments_by_line[line_num] << op.replace('ASSIGN: ', '')
					}
				}
			}
		}
	}

	// Print assignments sorted by line number
	mut sorted_lines := assignments_by_line.keys()
	sorted_lines.sort()

	for line_num in sorted_lines {
		println('Line ${line_num}:')
		for assignment in assignments_by_line[line_num] {
			println('  ${assignment}')
		}
	}

	println('==================================')
}
