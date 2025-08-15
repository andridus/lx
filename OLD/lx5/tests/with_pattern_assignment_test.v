module main

fn test_with_pattern_assignment() {
	// Test with expression with pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	with User{id: id, active: true} = user1 when id > 0 <- user do
		"active_user"
	else
		"inactive_user"
	end
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code contains the correct pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true} = User1'), 'With expression should generate pattern assignment'
	assert erlang_code.contains('when Id > 0'), 'With expression should include guard'
	assert erlang_code.contains('"active_user"'), 'With expression should include success body'
	assert erlang_code.contains('"inactive_user"'), 'With expression should include else body'

	println('✓ With expression pattern assignment test passed')
}

fn test_with_pattern_without_assignment() {
	// Test with expression without pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	with User{id: id, active: true} when id > 0 <- user do
		"active_user"
	else
		"inactive_user"
	end
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code does not contain pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true}'), 'With expression should generate pattern without assignment'
	assert !erlang_code.contains('= User1'), 'With expression should not have assignment when not specified'
	assert erlang_code.contains('when Id > 0'), 'With expression should include guard'
	assert erlang_code.contains('"active_user"'), 'With expression should include success body'
	assert erlang_code.contains('"inactive_user"'), 'With expression should include else body'

	println('✓ With expression without pattern assignment test passed')
}

fn test_case_pattern_assignment() {
	// Test case expression with pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	case user do
		User{id: id, active: true} = user1 when id > 0 ->
			"active_user"
		_ ->
			"inactive_user"
	end
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code contains the correct pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true} = User1'), 'Case expression should generate pattern assignment'
	assert erlang_code.contains('when Id > 0'), 'Case expression should include guard'
	assert erlang_code.contains('"active_user"'), 'Case expression should include success body'
	assert erlang_code.contains('"inactive_user"'), 'Case expression should include else body'

	println('✓ Case expression pattern assignment test passed')
}

fn test_case_pattern_without_assignment() {
	// Test case expression without pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	case user do
		User{id: id, active: true} when id > 0 ->
			"active_user"
		_ ->
			"inactive_user"
	end
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code does not contain pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true}'), 'Case expression should generate pattern without assignment'
	assert !erlang_code.contains('= User1'), 'Case expression should not have assignment when not specified'
	assert erlang_code.contains('when Id > 0'), 'Case expression should include guard'
	assert erlang_code.contains('"active_user"'), 'Case expression should include success body'
	assert erlang_code.contains('"inactive_user"'), 'Case expression should include else body'

	println('✓ Case expression without pattern assignment test passed')
}

fn test_match_pattern_assignment() {
	// Test match expression with pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	match User{id: id, active: true} = user1 when id > 0 <- user
	"active_user"
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code contains the correct pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true} = User1'), 'Match expression should generate pattern assignment'
	assert erlang_code.contains('when Id > 0'), 'Match expression should include guard'
	assert erlang_code.contains('"active_user"'), 'Match expression should include success body'

	println('✓ Match expression pattern assignment test passed')
}

fn test_match_pattern_without_assignment() {
	// Test match expression without pattern assignment
	lx_code := 'record User { id :: integer, active :: boolean }

def check_user_status(user) do
	match User{id: id, active: true} when id > 0 <- user
	"active_user"
end'

	erlang_code, hrl_content := generates_erlang(lx_code)

	// Verify the generated code does not contain pattern assignment
	assert erlang_code.contains('#user{id = Id, active = true}'), 'Match expression should generate pattern without assignment'
	assert !erlang_code.contains('= User1'), 'Match expression should not have assignment when not specified'
	assert erlang_code.contains('when Id > 0'), 'Match expression should include guard'
	assert erlang_code.contains('"active_user"'), 'Match expression should include success body'

	println('✓ Match expression without pattern assignment test passed')
}

fn test_consistent_pattern_assignment_behavior() {
	println('Testing consistent pattern assignment behavior across case, match, and with expressions...')

	test_with_pattern_assignment()
	test_with_pattern_without_assignment()
	test_case_pattern_assignment()
	test_case_pattern_without_assignment()
	test_match_pattern_assignment()
	test_match_pattern_without_assignment()

	println('✓ All pattern assignment tests passed - behavior is consistent across expressions')
}
