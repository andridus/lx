module main

fn test_receive_expression_simple() {
	lx_code := '
def simple_receive() do
  receive do
    1 ->
      x = 10
      y = 20
      x + y
    2 -> 0
    _ -> -1
  end
end'
	expected := '-module(main).
-export([simple_receive/0]).

-spec simple_receive() -> any().
simple_receive() ->
receive
    1 -> X_aaaa = 10,
    Y_baaa = 20,
    X_aaaa + Y_baaa;
    2 -> 0;
    _ -> -1
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_receive_expression_with_timeout() {
	lx_code := '
def receive_with_timeout() do
  receive do
    1 ->
      x = 10
      y = 20
      {x, y}
    2 -> :ok
    _ -> :unknown
  after 1000 ->
    timeout_msg = "Timeout occurred"
    error_code = -1
    {timeout_msg, error_code}
  end
end'
	expected := '-module(main).
-export([receive_with_timeout/0]).

-spec receive_with_timeout() -> any().
receive_with_timeout() ->
receive
    1 -> X_aaaa = 10,
    Y_baaa = 20,
    {X_aaaa, Y_baaa};
    2 -> ok;
    _ -> unknown
after 1000 ->
    Timeout_msg_caaa = "Timeout occurred",
    Error_code_daaa = -1,
    {Timeout_msg_caaa, Error_code_daaa}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_receive_expression_tuple_pattern() {
	lx_code := '
def tuple_receive() do
  receive do
    {start, pid} ->
      msg = "Process started"
      response = {msg, pid}
      response
    {stop, reason} ->
      cleanup_msg = "Cleaning up"
      final_msg = {cleanup_msg, reason}
      final_msg
    _ ->
      unknown_msg = "Unknown message"
      default_response = {unknown_msg, :error}
      default_response
  end
end'
	expected := '-module(main).
-export([tuple_receive/0]).

-spec tuple_receive() -> any().
tuple_receive() ->
receive
    {Start, Pid} -> Msg_aaaa = "Process started",
    Response_baaa = {Msg_aaaa, Pid},
    Response_baaa;
    {Stop, Reason} -> Cleanup_msg_caaa = "Cleaning up",
    Final_msg_daaa = {Cleanup_msg_caaa, Reason},
    Final_msg_daaa;
    _ -> Unknown_msg_eaaa = "Unknown message",
    Default_response_faaa = {Unknown_msg_eaaa, error},
    Default_response_faaa
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_receive_expression_atom_patterns() {
	lx_code := '
def atom_receive() do
  receive do
    :start ->
      status = "started"
      code = 1
      {status, code}
    :stop ->
      status = "stopped"
      code = 0
      {status, code}
    :error ->
      status = "error"
      code = -1
      {status, code}
    _ ->
      status = "unknown"
      code = 999
      {status, code}
  after 2000 ->
    timeout_status = "timeout"
    timeout_code = -2
    {timeout_status, timeout_code}
  end
end'
	expected := '-module(main).
-export([atom_receive/0]).

-spec atom_receive() -> any().
atom_receive() ->
receive
    start -> Status_aaaa = "started",
    Code_baaa = 1,
    {Status_aaaa, Code_baaa};
    stop -> Status_caaa = "stopped",
    Code_daaa = 0,
    {Status_caaa, Code_daaa};
    error -> Status_eaaa = "error",
    Code_faaa = -1,
    {Status_eaaa, Code_faaa};
    _ -> Status_gaaa = "unknown",
    Code_haaa = 999,
    {Status_gaaa, Code_haaa}
after 2000 ->
    Timeout_status_iaaa = "timeout",
    Timeout_code_jaaa = -2,
    {Timeout_status_iaaa, Timeout_code_jaaa}
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_receive_expression_nested() {
	lx_code := '
def nested_receive() do
  receive do
    :outer ->
      inner_result = receive do
        :inner ->
          x = 100
          y = 200
          x + y
        _ -> 0
      after 1000 ->
        timeout_val = -1
        timeout_val
      end
      final = inner_result * 2
      final
    _ -> 0
  end
end'
	expected := '-module(main).
-export([nested_receive/0]).

-spec nested_receive() -> any().
nested_receive() ->
receive
    outer -> Inner_result_aaaa = receive
    inner -> X_baaa = 100,
    Y_caaa = 200,
    X_baaa + Y_caaa;
    _ -> 0
after 1000 ->
    Timeout_val_daaa = -1,
    Timeout_val_daaa
end,
    Final_eaaa = Inner_result_aaaa * 2,
    Final_eaaa;
    _ -> 0
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}

fn test_receive_expression_single_statement() {
	lx_code := '
def single_statement_receive() do
  receive do
    1 -> :ok
    2 -> :error
    _ -> :unknown
  after 100 ->
    :timeout
  end
end'
	expected := '-module(main).
-export([single_statement_receive/0]).

-spec single_statement_receive() -> any().
single_statement_receive() ->
receive
    1 -> ok;
    2 -> error;
    _ -> unknown
after 100 ->
    timeout
end.

'
	erl1 := generates_erlang(lx_code)
	assert erl1.success
	assert erl1.code == expected
}
