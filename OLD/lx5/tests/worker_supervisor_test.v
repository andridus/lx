module main

fn test_basic_worker_generation() {
	lx_code := '
worker my_worker do
	def init(args) do
		{:ok, args}
	end

	def handle_call(request, _from, state) do
		{:reply, request, state}
	end

	def handle_cast(msg, state) do
		{:noreply, state}
	end
end'

	result := generates_worker_supervisor(lx_code)

	// Main module should be empty
	assert result.main_code == '-module(test).\n\n'
	assert result.main_hrl == ''

	// Should have one worker
	assert result.worker_files.len == 1
	assert 'my_worker' in result.worker_files

	worker_code := result.worker_files['my_worker']
	assert worker_code.contains('-module(my_worker).')
	assert worker_code.contains('-behaviour(gen_server).')
	assert worker_code.contains('start_link(Args)')
	assert worker_code.contains('init(Args)')
	assert worker_code.contains('handle_call(Request, _from, State)')
	assert worker_code.contains('handle_cast(Msg, State)')
}

fn test_worker_with_multiple_handles() {
	lx_code := '
worker task_worker do
	def init(initial_state) do
		{:ok, initial_state}
	end

	def handle_call({:get_state}, _from, state) do
		{:reply, state, state}
	end

	def handle_call({:set_state, new_state}, _from, _state) do
		{:reply, :ok, new_state}
	end

	def handle_cast({:update, value}, state) do
		{:noreply, state + value}
	end

	def handle_info(:timeout, state) do
		{:noreply, state}
	end
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one worker
	assert result.worker_files.len == 1
	assert 'task_worker' in result.worker_files

	worker_code := result.worker_files['task_worker']
	assert worker_code.contains('-module(task_worker).')
	assert worker_code.contains('handle_call({get_state}, _from, State)')
	assert worker_code.contains('handle_call({set_state, New_state}, _from, _state)')
	assert worker_code.contains('handle_cast({update, Value}, State)')
	assert worker_code.contains('handle_info(timeout, State)')
}

fn test_supervisor_with_list_children() {
	lx_code := '
supervisor my_supervisor do
	children [worker1, worker2]
	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one supervisor
	assert result.supervisor_files.len == 1
	assert 'my_supervisor' in result.supervisor_files

	supervisor_code := result.supervisor_files['my_supervisor']
	assert supervisor_code.contains('-module(my_supervisor).')
	assert supervisor_code.contains('-behaviour(supervisor).')
	assert supervisor_code.contains('start_link()')
	assert supervisor_code.contains('{{one_for_one, 5, 10}')
	assert supervisor_code.contains('{worker1, {worker1, start_link, []}, permanent, 5000, worker, [worker1]}')
	assert supervisor_code.contains('{worker2, {worker2, start_link, []}, permanent, 5000, worker, [worker2]}')
}

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

fn test_supervisor_with_map_children() {
	lx_code := '
supervisor web_supervisor do
	children [
		{:web_server, :permanent, 5000, :worker},
		{:database, :permanent, 10000, :worker}
	]

	strategy :one_for_all
end'

	result := generates_worker_supervisor(lx_code)

	// Should have one supervisor
	assert result.supervisor_files.len == 1
	assert 'web_supervisor' in result.supervisor_files

	supervisor_code := result.supervisor_files['web_supervisor']
	assert supervisor_code.contains('-module(web_supervisor).')
	assert supervisor_code.contains('{{one_for_all, 5, 10}')
	assert supervisor_code.contains('{web_server, {web_server, start_link, []}, permanent, 5000, worker, [web_server]}')
	assert supervisor_code.contains('{database, {database, start_link, []}, permanent, 10000, worker, [database]}')
}

fn test_supervisor_with_rest_for_one_strategy() {
	lx_code := '
supervisor chain_supervisor do
	children [
		{:step1, :permanent, 5000, :worker},
		{:step2, :permanent, 5000, :worker},
		{:step3, :permanent, 5000, :worker}
	]

	strategy :rest_for_one
end'

	result := generates_worker_supervisor(lx_code)

	supervisor_code := result.supervisor_files['chain_supervisor']
	assert supervisor_code.contains('{{rest_for_one, 5, 10}')
	assert supervisor_code.contains('{step1, {step1, start_link, []}, permanent, 5000, worker, [step1]}')
	assert supervisor_code.contains('{step2, {step2, start_link, []}, permanent, 5000, worker, [step2]}')
	assert supervisor_code.contains('{step3, {step3, start_link, []}, permanent, 5000, worker, [step3]}')
}

fn test_worker_and_supervisor_together() {
	lx_code := '
worker counter_worker do
	def init(initial_value) do
		{:ok, initial_value}
	end

	def handle_call(:get, _from, state) do
		{:reply, state, state}
	end

	def handle_call(:increment, _from, state) do
		{:reply, state + 1, state + 1}
	end

	def handle_cast(:reset, _state) do
		{:noreply, 0}
	end
end

supervisor counter_supervisor do
	children [
		{:counter_worker, :permanent, 5000, :worker}
	]

	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)

	// Should have both worker and supervisor
	assert result.worker_files.len == 1
	assert result.supervisor_files.len == 1
	assert 'counter_worker' in result.worker_files
	assert 'counter_supervisor' in result.supervisor_files

	worker_code := result.worker_files['counter_worker']
	assert worker_code.contains('-module(counter_worker).')
	assert worker_code.contains('handle_call(get, _from, State)')
	assert worker_code.contains('handle_call(increment, _from, State)')
	assert worker_code.contains('handle_cast(reset, _state)')

	supervisor_code := result.supervisor_files['counter_supervisor']
	assert supervisor_code.contains('-module(counter_supervisor).')
	assert supervisor_code.contains('{counter_worker, {counter_worker, start_link, []}, permanent, 5000, worker, [counter_worker]}')
}

fn test_worker_with_complex_init() {
	lx_code := '
worker config_worker do
	def init(%{database: db_config, cache: cache_config}) do
		case setup_database(db_config) do
			{:ok, db_conn} ->
				case setup_cache(cache_config) do
					{:ok, cache_conn} ->
						{:ok, %{db: db_conn, cache: cache_conn}}
					{:error, reason} ->
						{:stop, reason}
				end
			{:error, reason} ->
				{:stop, reason}
		end
	end

	def handle_call({:query, sql}, _from, %{db: db} = state) do
		result = :database.query(db, sql)
		{:reply, result, state}
	end

	def handle_cast({:cache_clear}, %{cache: cache} = state) do
		:cache.clear(cache)
		{:noreply, state}
	end
end'

	result := generates_worker_supervisor(lx_code)

	worker_code := result.worker_files['config_worker']
	assert worker_code.contains('-module(config_worker).')
	assert worker_code.contains('init(#{database => Db_config, cache => Cache_config})')
	assert worker_code.contains('handle_call({query, Sql}, _from, #{db => Db} = State)')
	assert worker_code.contains('handle_cast({cache_clear}, #{cache => Cache} = State)')
}

fn test_supervisor_with_mixed_child_types() {
	lx_code := '
supervisor mixed_supervisor do
	children [
		{:worker1, :permanent, 5000, :worker},
		{:sub_supervisor, :permanent, :infinity, :supervisor},
		{:worker2, :temporary, 3000, :worker}
	]

	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)

	supervisor_code := result.supervisor_files['mixed_supervisor']
	assert supervisor_code.contains('{worker1, {worker1, start_link, []}, permanent, 5000, worker, [worker1]}')
	assert supervisor_code.contains('{sub_supervisor, {sub_supervisor, start_link, []}, permanent, infinity, supervisor, [sub_supervisor]}')
	assert supervisor_code.contains('{worker2, {worker2, start_link, []}, temporary, 3000, worker, [worker2]}')
}

fn test_worker_with_all_callback_types() {
	lx_code := '
worker full_worker do
	def init(args) do
		{:ok, args}
	end

	def handle_call(request, from, state) do
		{:reply, {:call_response, request}, state}
	end

	def handle_cast(msg, state) do
		{:noreply, [msg | state]}
	end

	def handle_info(info, state) do
		{:noreply, state}
	end

	def terminate(reason, state) do
		:ok
	end

	def code_change(old_vsn, state, extra) do
		{:ok, state}
	end
end'

	result := generates_worker_supervisor(lx_code)

	worker_code := result.worker_files['full_worker']
	assert worker_code.contains('-module(full_worker).')
	assert worker_code.contains('handle_call(Request, From, State)')
	assert worker_code.contains('handle_cast(Msg, State)')
	assert worker_code.contains('handle_info(Info, State)')
	assert worker_code.contains('terminate(Reason, State)')
	assert worker_code.contains('code_change(Old_vsn, State, Extra)')
}

fn test_supervisor_with_empty_children() {
	lx_code := '
supervisor empty_supervisor do
	children []
	strategy :one_for_one
end'

	result := generates_worker_supervisor(lx_code)
	supervisor_code := result.supervisor_files['empty_supervisor']
	assert supervisor_code.contains('-module(empty_supervisor).')
	assert supervisor_code.contains('{{one_for_one, 5, 10}')
}

fn test_worker_with_guards_in_handles() {
	lx_code := '
worker guarded_worker do
	def init(state) do
		{:ok, state}
	end

	def handle_call(request, _from, state) when is_integer(request) do
		{:reply, request * 2, state}
	end

	def handle_call(request, _from, state) when is_binary(request) do
		{:reply, "Got: " ++ request, state}
	end

	def handle_cast(msg, state) when is_atom(msg) do
		{:noreply, [msg | state]}
	end
end'

	result := generates_worker_supervisor(lx_code)

	worker_code := result.worker_files['guarded_worker']
	assert worker_code.contains('-module(guarded_worker).')
	assert worker_code.contains('handle_call(Request, _from, State) when is_integer(Request)')
	assert worker_code.contains('handle_call(Request, _from, State) when is_binary(Request)')
	assert worker_code.contains('handle_cast(Msg, State) when is_atom(Msg)')
}

fn test_supervisor_with_complex_children_spec() {
	lx_code := '
supervisor complex_supervisor do
	children [
		{:main_worker, :permanent, 5000, :worker},
		{:backup_worker, :temporary, 3000, :worker}
	]
	strategy :one_for_all
	end'

	result := generates_worker_supervisor(lx_code)

	supervisor_code := result.supervisor_files['complex_supervisor']
	assert supervisor_code.contains('-module(complex_supervisor).')
	assert supervisor_code.contains('{{one_for_all, 5, 10}')
	assert supervisor_code.contains('{main_worker, {main_worker, start_link, []}, permanent, 5000, worker, [main_worker]}')
	assert supervisor_code.contains('{backup_worker, {backup_worker, start_link, []}, temporary, 3000, worker, [backup_worker]}')
}

fn test_multiple_workers_different_names() {
	lx_code := '
worker worker_one do
	def init(args) do
		{:ok, args}
	end

	def handle_call(request, _from, state) do
		{:reply, request, state}
	end
end

worker worker_two do
	def init(args) do
		{:ok, args}
	end

	def handle_call(request, _from, state) do
		{:reply, request, state}
	end
end'

	result := generates_worker_supervisor(lx_code)

	// Should have two workers
	assert result.worker_files.len == 2
	assert 'worker_one' in result.worker_files
	assert 'worker_two' in result.worker_files

	worker_one_code := result.worker_files['worker_one']
	assert worker_one_code.contains('-module(worker_one).')

	worker_two_code := result.worker_files['worker_two']
	assert worker_two_code.contains('-module(worker_two).')
}
