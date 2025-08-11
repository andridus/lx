module main

fn test_receive_multiline_simple() {
	lx_code := '
def simple_receive() do
  receive do
    1 ->
      x = 10
      y = 20
      x + y
    2 -> 0
    _ -> 999
  end
end'
	expected := '-module(test).
-export([simple_receive/0]).

-spec simple_receive() -> any().
simple_receive() ->
receive
    1 -> X_aaaa = 10,
    Y_baaa = 20,
    X_aaaa + Y_baaa;
    2 -> 0;
    _ -> 999
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}

fn test_receive_multiline_with_timeout() {
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
    msg = "timeout"
    code = 999
    {msg, code}
  end
end'
	expected := '-module(test).
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
    Msg_caaa = <<"timeout"/utf8>>,
    Code_daaa = 999,
    {Msg_caaa, Code_daaa}
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == ''
}
