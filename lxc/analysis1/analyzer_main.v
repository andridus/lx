module analysis1

import ast
import errors
import time

pub struct Analyzer {
pub mut:
	type_checker        TypeChecker
	variable_checker    VariableChecker
	directive_manager   Manager
	linter              Linter
	stop_on_first_error bool
	enable_warnings     bool
}

pub struct AnalysisResult {
pub mut:
	success       bool
	module_stmt   ast.ModuleStmt
	type_context  &TypeContext
	errors        []errors.CompilationError
	warnings      []errors.CompilationError
	phase_results map[AnalysisPhase]PhaseResult
}

pub enum AnalysisPhase {
	variable_check
	directive_processing
	linting
	type_checking
}

pub struct PhaseResult {
pub:
	success       bool
	duration      f64 // in milliseconds
	error_count   int
	warning_count int
}

pub fn new_analyzer() Analyzer {
	return Analyzer{
		type_checker:        new_type_checker()
		variable_checker:    new_variable_checker()
		directive_manager:   new_manager()
		linter:              new_linter()
		stop_on_first_error: false
		enable_warnings:     true
	}
}

pub fn (mut a Analyzer) analyze_module(module_stmt ast.ModuleStmt) AnalysisResult {
	mut ctx := new_type_context()
	mut result := AnalysisResult{
		success:       true
		module_stmt:   module_stmt
		type_context:  &ctx
		errors:        []
		warnings:      []
		phase_results: map[AnalysisPhase]PhaseResult{}
	}

	// Phase 1: Variable checking
	mut phase_start := time.now()
	variable_errors := a.variable_checker.check_module(module_stmt)
	result.errors << variable_errors
	result.phase_results[.variable_check] = PhaseResult{
		success:       variable_errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   variable_errors.len
		warning_count: 0
	}

	if a.stop_on_first_error && variable_errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 2: Directive processing
	phase_start = time.now()
	directive_errors := a.directive_manager.process_directives(module_stmt)
	result.errors << directive_errors
	result.phase_results[.directive_processing] = PhaseResult{
		success:       directive_errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   directive_errors.len
		warning_count: 0
	}

	if a.stop_on_first_error && directive_errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 3: Linting
	phase_start = time.now()
	lint_result := a.linter.lint_module(module_stmt)
	result.errors << lint_result.errors
	result.warnings << lint_result.warnings
	result.phase_results[.linting] = PhaseResult{
		success:       lint_result.errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   lint_result.errors.len
		warning_count: lint_result.warnings.len
	}

	if a.stop_on_first_error && lint_result.errors.len > 0 {
		result.success = false
		return result
	}

	// Phase 4: Type checking
	phase_start = time.now()
	type_result := a.type_checker.check_module(module_stmt)
	result.errors << type_result.errors
	result.warnings << type_result.warnings
	result.type_context = type_result.context
	result.phase_results[.type_checking] = PhaseResult{
		success:       type_result.errors.len == 0
		duration:      f64(time.now().unix_milli() - phase_start.unix_milli())
		error_count:   type_result.errors.len
		warning_count: type_result.warnings.len
	}

	result.success = result.errors.len == 0
	return result
}
