-module(t).
-export([process_typed_list/1]).

-spec process_typed_list([integer()]) -> binary() | integer().
process_typed_list([]) ->
    <<"empty"/utf8>>;
process_typed_list([HEAD_1 | TAIL_2]) ->
    HEAD_1 * 2.
