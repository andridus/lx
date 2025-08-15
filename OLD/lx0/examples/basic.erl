-module(basic).
-export([create_user/2, get_user_info/1, calculate_age/1]).

-record(user, {name, age}).



create_user(Name, Age) ->
    #user{name = Name, age = Age}.
get_user_info(User) ->
    "User info".
calculate_age(Birth_year) ->
    2024 - Birth_year.