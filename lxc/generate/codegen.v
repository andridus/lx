module generate

import ast
import typechecker

// CodeGenerator defines the interface for all code generators
pub interface CodeGenerator {
	// generate_module generates code for an entire module
	generate_module(module ast.ModuleStmt, type_ctx &typechecker.TypeContext) CodegenResult

	// get_file_extension returns the file extension for generated files
	get_file_extension() string

	// get_module_header generates the module header
	get_module_header(module_name string) string

	// get_module_footer generates the module footer
	get_module_footer() string
}

// CodegenResult represents the result of code generation
pub struct CodegenResult {
pub mut:
	success     bool
	code        string
	errors      []string
	warnings    []string
	module_name string
	file_path   string
}

// new_result creates a new code generation result
pub fn new_result(success bool, code string, module_name string, file_path string) CodegenResult {
	return CodegenResult{
		success:     success
		code:        code
		module_name: module_name
		file_path:   file_path
		errors:      []
		warnings:    []
	}
}

// BaseCodeGenerator provides common functionality for all generators
pub struct BaseCodeGenerator {
}

// new_base_generator creates a new base generator
pub fn new_base_generator() BaseCodeGenerator {
	return BaseCodeGenerator{}
}

// Default implementations for interface methods
pub fn (gen BaseCodeGenerator) get_file_extension() string {
	return '.erl'
}

pub fn (gen BaseCodeGenerator) get_module_header(module_name string) string {
	return '-module(${module_name}).\n'
}

pub fn (gen BaseCodeGenerator) get_module_footer() string {
	return ''
}
