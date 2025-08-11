module parser

import ast

@[heap]
pub struct DirectivesTable {
pub mut:
	moduledoc string
	docs      map[string]ast.Node
}

pub fn new_directives_table() &DirectivesTable {
	dt := &DirectivesTable{}
	return dt
}

pub fn (dt &DirectivesTable) update_moduledoc(moduledoc string) {
	unsafe {
		dt.moduledoc = moduledoc
	}
}

pub fn (dt &DirectivesTable) add_doc(function_key string, doc_node ast.Node) {
	unsafe {
		dt.docs[function_key] = doc_node
	}
}

pub fn (dt &DirectivesTable) get_moduledoc() string {
	return dt.moduledoc
}

pub fn (dt &DirectivesTable) get_doc(function_key string) ast.Node {
	return dt.docs[function_key]
}

pub fn (dt &DirectivesTable) has_doc(function_key string) bool {
	return function_key in dt.docs
}
