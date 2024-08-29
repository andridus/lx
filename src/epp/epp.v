// module epp

// import ast

// pub type Number = f64 | int | u8
// pub type Atom = string

// pub struct Tuple {
// pub mut:
// 	elements []Any
// }

// pub struct List {
// pub mut:
// 	elements []Any
// }

// pub fn (n Number) to_string() string {
// 	return match n {
// 		f64, int, u8 { n.str() }
// 	}
// }

// pub fn (many []Any) to_string() string {
// 	return many.map(|m| m.to_string()).join('.\n') + '.'
// }

// pub fn (any Any) to_string() string {
// 	return match any {
// 		Atom {
// 			any.str()
// 		}
// 		Number {
// 			any.to_string()
// 		}
// 		List {
// 			mut items := []string{}
// 			for e in any.elements {
// 				items << e.to_string()
// 			}
// 			'[${items.join(',')}]'
// 		}
// 		Tuple {
// 			mut items := []string{}
// 			for e in any.elements {
// 				items << e.to_string()
// 			}
// 			'{${items.join(',')}}'
// 		}
// 	}
// }

// pub type Any = Atom | List | Number | Tuple

// pub struct ExportedFunction {}

// pub struct ModuleOptions {
// pub:
// 	is_script bool
// }

// pub struct PreProcessor {
// pub mut:
// 	erlast         []Any
// 	exported_fns   []ExportedFunction
// 	module_options ModuleOptions
// 	labels         int
// }

// pub fn new_list(list []Any) Any {
// 	return Any(List{list})
// }

// pub fn new_number(n Number) Any {
// 	return Any(n)
// }

// pub fn new_tuple(list []Any) Any {
// 	return Any(Tuple{list})
// }

// pub fn new_atom(s string) Any {
// 	return Any(Atom(s))
// }

// fn append_element(tuple_or_list Any, element Any) Any {
// 	match tuple_or_list {
// 		Tuple {
// 			mut b := tuple_or_list as Tuple
// 			b.elements << element
// 			return b
// 		}
// 		List {
// 			mut b := tuple_or_list as List
// 			b.elements << element
// 			return b
// 		}
// 		else {
// 			return tuple_or_list
// 		}
// 	}
// }

// pub fn process(a ast.Node) PreProcessor {
// 	mut pp := PreProcessor{}
// 	pp.add_module()
// 	pp.add_exported_functions()
// 	pp.add_attributes()
// 	pp.add_labels()
// 	pp.add_functions(a)
// 	// pp.erlast << pp.convert_to_erlast(a)
// 	pp.add_default_module_functions()
// 	return pp
// }

// fn (mut pp PreProcessor) add_module() {
// 	pp.erlast << new_tuple([new_atom('module'), new_atom('script')])
// }

// fn (mut pp PreProcessor) add_exported_functions() {
// 	// {exports, [{module_info,0},{module_info,1},{run,0}]}.
// 	pp.erlast << new_tuple([
// 		new_atom('exports'),
// 		new_list([
// 			new_tuple([
// 				new_atom('module_info'),
// 				new_number(0),
// 			]),
// 			new_tuple([
// 				new_atom('module_info'),
// 				new_number(1),
// 			]),
// 			new_tuple([
// 				new_atom('run'),
// 				new_number(0),
// 			]),
// 		]),
// 	])
// }

// fn (mut pp PreProcessor) add_attributes() {
// 	pp.erlast << new_tuple([new_atom('attributes'), new_list([])])
// }

// fn (mut pp PreProcessor) add_labels() {
// 	pp.erlast << new_tuple([new_atom('labels'), new_number(7)])
// }

// fn (mut pp PreProcessor) add_functions(node ast.Node) {
// 	pp.erlast << new_tuple([new_atom('function'), new_atom('run'),
// 		new_number(0), new_number(2)])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(1)])
// 	pp.erlast << new_tuple([
// 		new_atom('func_info'),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('script'),
// 		]),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('run'),
// 		]),
// 		new_number(0),
// 	])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(2)])
// 	body_function := if node.nodes.len > 0 {
// 		pp.convert_node_to_erlast(node)
// 	} else {
// 		new_tuple([
// 			new_atom('move'),
// 			pp.convert_node_left_to_erlast(node.left),
// 			new_tuple([
// 				new_atom('x'),
// 				new_number(0),
// 			]),
// 		])
// 	}
// 	pp.erlast << body_function
// 	pp.erlast << new_atom('return')
// }

// fn (mut pp PreProcessor) add_default_module_functions() {
// 	pp.erlast << new_tuple([new_atom('function'), new_atom('module_info'),
// 		new_number(0), new_number(4)])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(3)])
// 	pp.erlast << new_tuple([
// 		new_atom('func_info'),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('script'),
// 		]),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('module_info'),
// 		]),
// 		new_number(0),
// 	])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(4)])
// 	pp.erlast << new_tuple([
// 		new_atom('move'),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('script'),
// 		]),
// 		new_tuple([
// 			new_atom('x'),
// 			new_number(0),
// 		]),
// 	])
// 	pp.erlast << new_tuple([
// 		new_atom('call_ext_only'),
// 		new_number(1),
// 		new_tuple([
// 			new_atom('extfunc'),
// 			new_atom('erlang'),
// 			new_atom('get_module_info'),
// 			new_number(1),
// 		]),
// 	])

// 	pp.erlast << new_tuple([new_atom('function'), new_atom('module_info'),
// 		new_number(1), new_number(6)])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(5)])
// 	pp.erlast << new_tuple([
// 		new_atom('func_info'),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('script'),
// 		]),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('module_info'),
// 		]),
// 		new_number(1),
// 	])
// 	pp.erlast << new_tuple([new_atom('label'), new_number(6)])
// 	pp.erlast << new_tuple([
// 		new_atom('move'),
// 		new_tuple([
// 			new_atom('x'),
// 			new_number(0),
// 		]),
// 		new_tuple([
// 			new_atom('x'),
// 			new_number(1),
// 		]),
// 	])
// 	pp.erlast << new_tuple([
// 		new_atom('move'),
// 		new_tuple([
// 			new_atom('atom'),
// 			new_atom('script'),
// 		]),
// 		new_tuple([
// 			new_atom('x'),
// 			new_number(0),
// 		]),
// 	])
// 	pp.erlast << new_tuple([
// 		new_atom('call_ext_only'),
// 		new_number(2),
// 		new_tuple([
// 			new_atom('extfunc'),
// 			new_atom('erlang'),
// 			new_atom('get_module_info'),
// 			new_number(2),
// 		]),
// 	])
// }

// fn (mut pp PreProcessor) add_label(qtd int) {
// 	pp.labels += qtd
// }

// fn (mut pp PreProcessor) convert_node_to_erlast(node ast.Node) Any {
// 	if node.nodes.len > 0 {
// 		mut tuple := pp.convert_node_left_to_erlast(node.left)
// 		mut list_params := new_list([])
// 		for n in node.nodes {
// 			list_params = append_element(list_params, pp.convert_node_to_erlast(n))
// 		}
// 		tuple = append_element(tuple, list_params)
// 		return append_element(tuple, new_tuple([new_atom('x'),
// 			new_number(0)]))
// 	} else {
// 		return pp.convert_node_left_to_erlast(node.left)
// 	}
// }

// fn (mut pp PreProcessor) convert_node_left_to_erlast(left ast.NodeLeft) Any {
// 	return match left {
// 		ast.Atom {
// 			val := left.value
// 			atom := match val {
// 				'+', '-', '*', '/' { new_atom('gc_bif') }
// 				else { new_atom('atom') }
// 			}
// 			new_tuple([
// 				atom,
// 				new_atom('\'${left.value}\''),
// 				new_tuple([new_atom('f'), new_number(0)]),
// 				new_number(1),
// 			])
// 		}
// 		ast.Node {
// 			// pp.convert_to_erlast(left)
// 			new_tuple([])
// 		}
// 		f64 {
// 			new_tuple([new_atom('float'), new_number(left)])
// 		}
// 		int {
// 			new_tuple([new_atom('integer'), new_number(left)])
// 		}
// 		string {
// 			mut elements := []Any{}
// 			for i in left.u8_array() {
// 				elements << new_number(i)
// 			}
// 			new_list(elements)
// 		}
// 	}
// }
