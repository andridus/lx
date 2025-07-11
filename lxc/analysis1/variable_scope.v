module analysis1

import ast

pub struct VariableScope {
pub mut:
	variables map[string]VariableBinding
	functions map[string]FunctionBinding
	level     int
}

pub struct VariableBinding {
pub mut:
	name     string
	position ast.Position
	defined  bool
	used     bool
}

pub struct FunctionBinding {
pub:
	name     string
	position ast.Position
	arity    int
	clauses  []ast.FunctionClause
}

pub struct ScopeManager {
pub mut:
	scope_stack []VariableScope
}

pub fn new_scope_manager() ScopeManager {
	return ScopeManager{
		scope_stack: [new_variable_scope()]
	}
}

fn new_variable_scope() VariableScope {
	return VariableScope{
		variables: map[string]VariableBinding{}
		functions: map[string]FunctionBinding{}
		level:     0
	}
}

pub fn (mut sm ScopeManager) enter_scope() {
	sm.scope_stack << VariableScope{
		variables: map[string]VariableBinding{}
		functions: map[string]FunctionBinding{}
		level:     sm.scope_stack.len
	}
}

pub fn (mut sm ScopeManager) exit_scope() {
	if sm.scope_stack.len > 1 {
		sm.scope_stack.delete_last()
	}
}

pub fn (mut sm ScopeManager) bind_variable(name string, position ast.Position) {
	if sm.scope_stack.len > 0 {
		sm.scope_stack[sm.scope_stack.len - 1].variables[name] = VariableBinding{
			name:     name
			position: position
			defined:  true
			used:     false
		}
	}
}

pub fn (mut sm ScopeManager) use_variable(name string, position ast.Position) {
	for i := sm.scope_stack.len - 1; i >= 0; i-- {
		if name in sm.scope_stack[i].variables {
			mut binding := sm.scope_stack[i].variables[name]
			binding.used = true
			sm.scope_stack[i].variables[name] = binding
			return
		}
	}
}

pub fn (sm &ScopeManager) has_binding_local(name string) bool {
	if sm.scope_stack.len == 0 {
		return false
	}
	return name in sm.scope_stack[sm.scope_stack.len - 1].variables
}

pub fn (sm &ScopeManager) has_binding_in_parent(name string) bool {
	for i := sm.scope_stack.len - 2; i >= 0; i-- {
		if name in sm.scope_stack[i].variables {
			return true
		}
	}
	return false
}

pub fn (sm &ScopeManager) has_binding(name string) bool {
	for i := sm.scope_stack.len - 1; i >= 0; i-- {
		if name in sm.scope_stack[i].variables {
			return true
		}
	}
	return false
}

pub fn (sm &ScopeManager) get_binding(name string) ?VariableBinding {
	for i := sm.scope_stack.len - 1; i >= 0; i-- {
		if name in sm.scope_stack[i].variables {
			return sm.scope_stack[i].variables[name]
		}
	}
	return none
}

pub fn (mut sm ScopeManager) bind_function(name string, position ast.Position, clauses []ast.FunctionClause) {
	if sm.scope_stack.len > 0 {
		arity := if clauses.len > 0 { clauses[0].parameters.len } else { 0 }
		sm.scope_stack[sm.scope_stack.len - 1].functions[name] = FunctionBinding{
			name:     name
			position: position
			arity:    arity
			clauses:  clauses
		}
	}
}

pub fn (sm &ScopeManager) has_function(name string) bool {
	for i := sm.scope_stack.len - 1; i >= 0; i-- {
		if name in sm.scope_stack[i].functions {
			return true
		}
	}
	return false
}

pub fn (sm &ScopeManager) get_function(name string) ?FunctionBinding {
	for i := sm.scope_stack.len - 1; i >= 0; i-- {
		if name in sm.scope_stack[i].functions {
			return sm.scope_stack[i].functions[name]
		}
	}
	return none
}

pub fn (sm &ScopeManager) get_unused_variables() []VariableBinding {
	mut unused := []VariableBinding{}
	for scope in sm.scope_stack {
		for _, binding in scope.variables {
			if binding.defined && !binding.used {
				unused << binding
			}
		}
	}
	return unused
}

pub fn (sm &ScopeManager) get_undefined_variables() []string {
	mut undefined := []string{}
	for scope in sm.scope_stack {
		for _, binding in scope.variables {
			if binding.used && !binding.defined {
				undefined << binding.name
			}
		}
	}
	return undefined
}
