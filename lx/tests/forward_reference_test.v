module tests

import compile

fn test_worker_forward_references() {
	code := '
	worker test_worker do
		def init(_args) do
			{:ok, %{}}
		end

		def start_process() do
			process_data()
		end

		def process_data() do
			:processed
		end
	end
	'

	erlang_code := compile.compile_string(code, 'test.lx') or {
		panic('Failed to compile worker with forward references: ${err}')
	}

	// Should compile successfully without "undefined function" errors
	assert erlang_code.len > 0, 'Worker forward references should compile successfully'
	assert erlang_code.contains('process_data()'), 'Should contain forward function call'
	assert erlang_code.contains('process_data() ->'), 'Should contain function definition'
}

fn test_supervisor_forward_references() {
	code := '
	supervisor test_sup do
		strategy :one_for_one
		children [:test_worker]

		def supervisor_func() do
			helper_func()
		end

		def helper_func() do
			:ok
		end
	end
	'

	erlang_code := compile.compile_string(code, 'test.lx') or {
		panic('Failed to compile supervisor with forward references: ${err}')
	}

	// Should compile successfully without "undefined function" errors
	assert erlang_code.len > 0, 'Supervisor forward references should compile successfully'
	assert erlang_code.contains('helper_func()'), 'Should contain forward function call'
	assert erlang_code.contains('helper_func() ->'), 'Should contain function definition'
}

fn test_mixed_worker_supervisor_forward_references() {
	code := '
	supervisor test_sup do
		strategy :one_for_one
		children [:test_worker]

		def sup_start() do
			sup_helper()
		end

		def sup_helper() do
			:sup_ok
		end
	end

	worker test_worker do
		def init(_args) do
			{:ok, %{}}
		end

		def worker_start() do
			worker_helper()
		end

		def worker_helper() do
			:worker_ok
		end
	end
	'

	// For mixed compilation, we need to use compile_multi_file
	result := compile.compile_multi_file(code, 'test.lx', 'test') or {
		panic('Failed to compile mixed worker/supervisor with forward references: ${err}')
	}

	// Should compile successfully without "undefined function" errors
	assert result.files.len > 0, 'Mixed worker/supervisor forward references should compile successfully'

	// Check supervisor file
	mut sup_found := false
	mut worker_found := false
	for filename, file_content in result.files {
		if filename.contains('test_sup_sup') {
			sup_found = true
			assert file_content.contains('sup_helper()'), 'Should contain supervisor forward function call'
			assert file_content.contains('sup_helper() ->'), 'Should contain supervisor function definition'
		}
		if filename.contains('test_worker') {
			worker_found = true
			assert file_content.contains('worker_helper()'), 'Should contain worker forward function call'
			assert file_content.contains('worker_helper() ->'), 'Should contain worker function definition'
		}
	}

	assert sup_found, 'Supervisor file should be generated'
	assert worker_found, 'Worker file should be generated'
}
