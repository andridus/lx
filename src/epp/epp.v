module epp

import ast

pub type Number = f64 | int | u8
pub type Atom = string

pub struct Tuple {
pub mut:
	elements []Any
}

pub struct List {
pub mut:
	elements []Any
}

pub type Any = Atom | List | Number | Tuple

pub struct PreProcessor {
pub:
	is_script bool
pub mut:
	erlast Any
}

pub fn new_list(list []Any) Any {
	return Any(List{list})
}

pub fn new_number(n Number) Any {
	return Any(n)
}

pub fn new_tuple(list []Any) Any {
	return Any(Tuple{list})
}

pub fn new_atom(s string) Any {
	return Any(Atom(s))
}

fn append_element(tuple_or_list Any, element Any) Any {
	match tuple_or_list {
		Tuple {
			mut b := tuple_or_list as Tuple
			b.elements << element
			return b
		}
		List {
			mut b := tuple_or_list as List
			b.elements << element
			return b
		}
		else {
			return tuple_or_list
		}
	}
}

pub fn process_ast(a ast.Node) PreProcessor {
	mut pp := PreProcessor{
		is_script: true
	}
	pp.erlast = new_list([pp.convert_to_erlast(a)])
	return pp
}

fn (mut pp PreProcessor) convert_to_erlast(node ast.Node) Any {
	if node.nodes.len > 0 {
		mut tuple := pp.convert_node_left_to_erlast(node.left)
		for n in node.nodes {
			tuple = append_element(tuple, pp.convert_to_erlast(n))
		}
		return tuple
	} else {
		return pp.convert_node_left_to_erlast(node.left)
	}
}

fn (mut pp PreProcessor) convert_node_left_to_erlast(left ast.NodeLeft) Any {
	return match left {
		ast.Atom {
			val := left.value
			atom := match val {
				'+', '-', '*', '/' { new_atom('op') }
				else { new_atom('atom') }
			}
			new_tuple([atom, new_number(1), new_atom(left.value)])
		}
		ast.Node {
			pp.convert_to_erlast(left)
		}
		f64 {
			new_tuple([new_atom('float'), new_number(1), new_number(left)])
		}
		int {
			new_tuple([new_atom('integer'), new_number(1), new_number(left)])
		}
		string {
			mut elements := []Any{}
			for i in left.u8_array() {
				elements << new_number(i)
			}
			new_list(elements)
		}
	}
}
