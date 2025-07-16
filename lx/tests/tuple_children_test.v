module main

fn test_supervisor_with_tuple_children() {
	lx_code := '
supervisor tuple_supervisor do
	children [
		{:worker1, :permanent, 5000, :worker},
		{:worker2, :temporary, 3000, :worker}
	]
	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one supervisor
	assert result.supervisor_files.len == 1
	assert 'tuple_supervisor' in result.supervisor_files

	supervisor_code := result.supervisor_files['tuple_supervisor']
	assert supervisor_code.contains('-module(tuple_supervisor).')
	assert supervisor_code.contains('-behaviour(supervisor).')
	assert supervisor_code.contains('start_link()')
	assert supervisor_code.contains('{{one_for_one, 5, 10}')
	assert supervisor_code.contains('{worker1, {worker1, start_link, []}, permanent, 5000, worker, [worker1]}')
	assert supervisor_code.contains('{worker2, {worker2, start_link, []}, temporary, 3000, worker, [worker2]}')
}

fn test_supervisor_with_mixed_tuple_children() {
	lx_code := '
supervisor mixed_supervisor do
	children [
		{:worker1, :permanent, 5000, :worker},
		{:sup1, :permanent, :infinity, :supervisor},
		{:worker2, :temporary, 3000, :worker}
	]
	strategy :one_for_all
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one supervisor
	assert result.supervisor_files.len == 1
	assert 'mixed_supervisor' in result.supervisor_files

	supervisor_code := result.supervisor_files['mixed_supervisor']
	assert supervisor_code.contains('-module(mixed_supervisor).')
	assert supervisor_code.contains('{{one_for_all, 5, 10}')
	assert supervisor_code.contains('{worker1, {worker1, start_link, []}, permanent, 5000, worker, [worker1]}')
	assert supervisor_code.contains('{sup1, {sup1, start_link, []}, permanent, infinity, supervisor, [sup1]}')
	assert supervisor_code.contains('{worker2, {worker2, start_link, []}, temporary, 3000, worker, [worker2]}')
}

fn test_supervisor_with_empty_tuple_children() {
	lx_code := '
supervisor empty_supervisor do
	children []
	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one supervisor
	assert result.supervisor_files.len == 1
	assert 'empty_supervisor' in result.supervisor_files

	supervisor_code := result.supervisor_files['empty_supervisor']
	println('Empty supervisor code: ${supervisor_code}')
	assert supervisor_code.contains('-module(empty_supervisor).')
	assert supervisor_code.contains('{{one_for_one, 5, 10}, [\n        \n    ]}')
}