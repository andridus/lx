-module(simple_case).
-export([main/0]).

main() ->

    %% Unsupported Core Erlang expression: {error,
                                        {invalid_case_structure,1,
                                         [{integer,1,1},
                                          [{clause,2,
                                            [{integer,2,1},
                                             [{ident,2,"true"}]]}]]}}.

