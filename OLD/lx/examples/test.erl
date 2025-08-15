-module(test).
-export([main/0]).

main() ->

    %% Unsupported Core Erlang expression: {'case',1,
                                        {integer,1,1},
                                        [{clause,2,
                                          [{integer,2,1}],
                                          [],
                                          [{var,2,false}]}]}.

