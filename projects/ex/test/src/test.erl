-module(test).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Key Token Test

start(_StartType, _StartArgs) ->
    {ok, self()}.

stop(_State) ->
    ok.
