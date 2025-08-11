// module main

// fn test_simple_worker() {
// 	lx_code := '
// worker user_manager do
//   def init(_) do
//     {:ok, %{users: [], count: 0}}
//   end

//   def handle_call(:get_count, _from, state) do
//     {:reply, state[:count], state}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([init/1, handle_call/3]).

// init(_) ->
// {ok, #{users => [], count => 0}}.

// handle_call(get_count, _From, State) ->
// {reply, maps:get(count, State), State}.

// '
// 	assert erl1.success
// 	// TODO: Fix worker syntax (not implemented)
// 	// assert erl1.code == expected
// }

// fn test_simple_supervisor() {
// 	lx_code := '
// supervisor top_supervisor do
//   strategy one_for_one
//   children [
//     user_manager,
//     session_manager
//   ]
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([init/1]).

// init(_) ->
// {ok, {{one_for_one, 5, 10}, [
// {user_manager, {user_manager, start_link, []}, permanent, 5000, worker, [user_manager]},
// {session_manager, {session_manager, start_link, []}, permanent, 5000, worker, [session_manager]}
// ]}}.

// '
// 	assert erl1.success
// 	// TODO: Fix supervisor syntax (not implemented)
// 	// assert erl1.code == expected
// }

// fn test_worker_with_state() {
// 	lx_code := '
// worker counter do
//   def init(_) do
//     {:ok, 0}
//   end

//   def handle_call(:increment, _from, state) do
//     {:reply, :ok, state + 1}
//   end

//   def handle_call(:get, _from, state) do
//     {:reply, state, state}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([init/1, handle_call/3]).

// init(_) ->
// {ok, 0}.

// handle_call(increment, _From, State) ->
// {reply, ok, State + 1};
// handle_call(get, _From, State) ->
// {reply, State, State}.

// '
// 	assert erl1.success
// 	// TODO: Fix worker with multiple handle_call clauses
// 	// assert erl1.code == expected
// }

// fn test_worker_with_cast() {
// 	lx_code := '
// worker logger do
//   def init(_) do
//     {:ok, []}
//   end

//   def handle_cast({:log, message}, state) do
//     {:noreply, [message | state]}
//   end

//   def handle_call(:get_logs, _from, state) do
//     {:reply, state, state}
//   end
// end'
// 	erl1 := generates_erlang(lx_code)
// 	expected := '-module(main).
// -export([init/1, handle_call/3, handle_cast/2]).

// init(_) ->
// {ok, []}.

// handle_cast({log, Message}, State) ->
// {noreply, [Message | State]}.

// handle_call(get_logs, _From, State) ->
// {reply, State, State}.

// '
// 	assert erl1.success
// 	// TODO: Fix worker with handle_cast
// 	// assert erl1.code == expected
// }
