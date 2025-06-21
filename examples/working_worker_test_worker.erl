-module(working_worker_test_worker).
-behaviour(gen_server).
-compile(export_all).

init(Args) ->
    {ok, 0}.