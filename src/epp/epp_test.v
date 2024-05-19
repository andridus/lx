module epp

import parser

fn input_module_start_script() string {
	return '{module,script}.
{exports,[{module_info,0},{module_info,1},{run,0}]}.
{attributes,[]}.
{labels,7}.
{function,run,0,2}.
{label,1}.
{func_info,{atom,script},{atom,run},0}.
{label,2}.'
}

fn input_module_info(start_label int) string {
	mut label := start_label
	return '{function,module_info,0,${label + 1}}.
{label,${label}}.
{func_info,{atom,script},{atom,module_info},0}.
{label,${
		label + 1}}.
{move,{atom,script},{x,0}}.
{call_ext_only,1,{extfunc,erlang,get_module_info,1}}.
{function,module_info,1,${
		label + 3}}.
{label,${label + 2}}.
{func_info,{atom,script},{atom,module_info},1}.
{label,${
		label + 3}}.
{move,{x,0},{x,1}}.
{move,{atom,script},{x,0}}.
{call_ext_only,2,{extfunc,erlang,get_module_info,2}}.'
}

fn test_preprocessor_ast_to_erlast() {
	source0 := '1'
	expected1 := '${input_module_start_script()}\n{move,{integer,1},{x,0}}.\nreturn.\n${input_module_info(3)}'
	nodes := parser.parse_stmt(source0)!
	p := process(nodes)
	assert expected1 == p.erlast.to_string()
}

fn test_preprocessor_precendece0_ast_to_erlast() {
	source0 := '1 + 2'
	expected1 := '${input_module_start_script()}\n{gc_bif,\'+\',{f,0},1,[{integer,1},{integer,2}],{x,0}}.\nreturn.\n${input_module_info(3)}'
	nodes := parser.parse_stmt(source0)!
	p := process(nodes)
	assert expected1 == p.erlast.to_string()
}

// fn test_preprocessor_precendece_ast_to_erlast() {
// 	source0 := '1 + 2 - 3'
// 	expected1 := new_list([
// 		new_tuple([
// 			new_atom('op'),
// 			new_number(1),
// 			new_atom('-'),
// 			new_tuple([
// 				new_atom('op'),
// 				new_number(1),
// 				new_atom('+'),
// 				new_tuple([
// 					new_atom('integer'),
// 					new_number(1),
// 					new_number(1),
// 				]),
// 				new_tuple([
// 					new_atom('integer'),
// 					new_number(1),
// 					new_number(2),
// 				]),
// 			]),
// 			new_tuple([
// 				new_atom('integer'),
// 				new_number(1),
// 				new_number(3),
// 			]),
// 		]),
// 	])
// 	nodes := parser.parse_stmt(source0)!
// 	p := process(nodes)
// 	assert expected1 == p.erlast
// }
// fn test_preprocessor_labels() {
// 	source0 := '1 + 2'
// 	expected1 := new_list([
// 		new_tuple([
// 			new_atom('op'),
// 			new_number(1),
// 			new_atom('+'),
// 			new_tuple([
// 				new_atom('integer'),
// 				new_number(1),
// 				new_number(1),
// 			]),
// 			new_tuple([
// 				new_atom('integer'),
// 				new_number(1),
// 				new_number(2),
// 			]),
// 		]),
// 	])
// 	nodes := parser.parse_stmt(source0)!
// 	p := process(nodes)
// 	assert 2 == p.labels
// }
