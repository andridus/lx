-module(test_record_multiline).
-export([main/0]).

-record(user, {id = nil :: integer(), name = nil :: binary(), email = nil :: binary(), created_at = nil :: integer()}).
-record(session, {user_id = nil :: integer(), token = nil :: binary(), expires_at = nil :: integer()}).
-spec main() -> atom().
main() ->
    ok.
