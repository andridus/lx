module main

import compiler
import backend.codegen

fn generates_erlang(lx_code string) (string, string) {
	mut comp := compiler.new_compiler()
	result := comp.compile(lx_code, 'test', '')
	return result.code, result.hrl_content
}

fn generates_erlang_result(lx_code string) codegen.CodegenResult {
	mut comp := compiler.new_compiler()
	result := comp.compile(lx_code, 'test', '')
	return result
}

fn generates_worker_supervisor(lx_code string) compiler.WorkerSupervisorResult {
	mut comp := compiler.new_compiler()
	result := comp.compile_for_testing(lx_code, 'test')
	return result
}
