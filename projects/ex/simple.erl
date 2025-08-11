-module(simple).
-export([parse_header/1]).

-spec parse_header(binary()) -> binary | {integer(), integer(), integer()}.
parse_header(Packet) when is_binary(Packet) ->
case Packet of
    <<Version:8/integer, Type1:8/integer, Length:16/integer-big>> ->
        {Version, Type1, Length};
    Error ->
        Error
end.

