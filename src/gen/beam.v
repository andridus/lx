module gen

import parser
import ast
import strings

pub struct BeamGen {
	module_name string
mut:
	labels          int
	out             strings.Builder
	inside_function bool
}

pub fn generate_beam(text string) !string {
	ast0 := parser.parse_stmt(text)!
	mut g := generate_beam_from_ast(ast0)
	return g.build_scripting()
}

fn generate_beam_from_ast(node ast.Node) BeamGen {
	mut g := BeamGen{
		module_name: 'script'
		out:         strings.new_builder(1000)
	}
	g.writeln(g.generate_from_node(node))
	return g
}

@[inline]
fn (mut g BeamGen) write(str string) {
	g.out.write_string(str)
}

@[inline]
fn (mut g BeamGen) writeln(str string) {
	g.out.write_string(str)
}

fn (mut g BeamGen) generate_from_node(node ast.Node) string {
	kind := node.kind
	return match kind {
		ast.Nil {
			node.left.to_str()
		}
		ast.Atom {
			node.left.to_str()
		}
		ast.Float {
			node.left.to_str()
		}
		ast.Integer {
			'{integer, ${node.left.to_str()}}'
		}
		ast.Function {
			// g.labels += 2

			if kind.position == .infix {
				function := node.left.to_str()
				left := g.generate_from_node(node.nodes[0])
				right := g.generate_from_node(node.nodes[1])
				mut str_function := strings.new_builder(1000)
				if !g.inside_function {
					str_function.writeln('{function, run, 0, 2}.')
					str_function.writeln('  {label,${g.labels++}}.')
					str_function.writeln('    {func_info,{atom,${g.module_name}},{atom,run},0}')
					str_function.writeln('  {label,${g.labels++}}.')
					g.inside_function = true
				}
				str_function.writeln('    {gc_bif,\'${function}\',{f,0},1,[{x,0},${right}],{x,0}}.')
				if !g.inside_function {
					str_function.writeln('    return.')
					str_function.str()
				}
			} else {
				panic('undefined function')
			}
		}
	}
}

fn (mut g BeamGen) build_scripting() string {
	mut string_scripting := strings.new_builder(1000)
	string_scripting.writeln('\n{module, ${g.module_name}}.  %% version = 0')
	string_scripting.writeln('{exports, [{module_info,0},{module_info,1},{run,0}]}.')
	string_scripting.writeln('{attributes, []}.')
	string_scripting.writeln('{labels, ${g.labels}}.')
	string_scripting.write_string(g.out.str())
	string_scripting.writeln('{function, module_info, 0, ${g.labels + 2}}.')
	string_scripting.writeln('  {label,${g.labels + 1}}.')
	string_scripting.writeln('    {func_info,{atom,${g.module_name}},{atom,module_info},0}.')
	string_scripting.writeln('  {label,${g.labels + 2}}.')
	string_scripting.writeln('    {move,{atom,${g.module_name}},{x,0}}.')
	string_scripting.writeln('    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.')
	string_scripting.writeln('{function, module_info, 1, ${g.labels + 4}}.')
	string_scripting.writeln('  {label,${g.labels + 3}}.')
	string_scripting.writeln('    {func_info,{atom,${g.module_name}},{atom,module_info},1}.')
	string_scripting.writeln('  {label,${g.labels + 4}}.')
	string_scripting.writeln('    {move,{x,0},{x,1}}.')
	string_scripting.writeln('    {move,{atom,${g.module_name}},{x,0}}.')
	string_scripting.writeln('    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.')
	return string_scripting.str()
}
