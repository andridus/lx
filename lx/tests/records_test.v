module main

fn test_simple_record_definition() {
	lx_code := 'record Person { name :: string, age :: integer }

def simple_record() do
    person = Person{name: "João", age: 30}
    person
end'

	expected := '-module(test).
-export([simple_record/0]).

-include("test.hrl").

-spec simple_record() -> #person{}.
simple_record() ->
    PERSON_1 = #person{name = <<"João"/utf8>>, age = 30},
    PERSON_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_field_access() {
	lx_code := 'record Person { name :: string, age :: integer }

def field_access() do
    person = Person{name: "Maria", age: 25}
    name = person.name
    age = person.age
    {name, age}
end'

	expected := '-module(test).
-export([field_access/0]).

-include("test.hrl").

-spec field_access() -> {binary(), integer()}.
field_access() ->
    PERSON_1 = #person{name = <<"Maria"/utf8>>, age = 25},
    NAME_2 = PERSON_1#person.name,
    AGE_3 = PERSON_1#person.age,
    {NAME_2, AGE_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_update() {
	lx_code := 'record Person { name :: string, age :: integer }

def record_update() do
    person = Person{name: "João", age: 30}
    updated_person = %Person{person | age: 31}
    updated_person
end'

	expected := '-module(test).
-export([record_update/0]).

-include("test.hrl").

-spec record_update() -> #person{}.
record_update() ->
    PERSON_1 = #person{name = <<"João"/utf8>>, age = 30},
    UPDATED_PERSON_2 = PERSON_1#person{age = 31},
    UPDATED_PERSON_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_variables() {
	lx_code := 'record Person { name :: string, age :: integer }

def record_with_vars() do
    name = "João"
    age = 30
    person = Person{name: name, age: age}
    person
end'

	expected := '-module(test).
-export([record_with_vars/0]).

-include("test.hrl").

-spec record_with_vars() -> #person{}.
record_with_vars() ->
    NAME_1 = <<"João"/utf8>>,
    AGE_2 = 30,
    PERSON_3 = #person{name = NAME_1, age = AGE_2},
    PERSON_3.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_expressions() {
	lx_code := 'record Point { x :: integer, y :: integer }

def point_with_expr() do
    a = 10
    b = 5
    point = Point{x: a + b, y: a * b}
    point
end'

	expected := '-module(test).
-export([point_with_expr/0]).

-include("test.hrl").

-spec point_with_expr() -> #point{}.
point_with_expr() ->
    A_1 = 10,
    B_2 = 5,
    POINT_3 = #point{x = A_1 + B_2, y = A_1 * B_2},
    POINT_3.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_access_with_expressions() {
	lx_code := 'record Point { x :: integer, y :: integer }

def access_with_expr() do
    point = Point{x: 10, y: 20}
    sum = point.x + point.y
    product = point.x * point.y
    {sum, product}
end'

	expected := '-module(test).
-export([access_with_expr/0]).

-include("test.hrl").

-spec access_with_expr() -> {integer(), integer()}.
access_with_expr() ->
    POINT_1 = #point{x = 10, y = 20},
    SUM_2 = POINT_1#point.x + POINT_1#point.y,
    PRODUCT_3 = POINT_1#point.x * POINT_1#point.y,
    {SUM_2, PRODUCT_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_access_with_parentheses() {
	lx_code := 'record Point { x :: integer, y :: integer }

def access_with_parens() do
    point = Point{x: 10, y: 20}
    result = (point.x + point.y) * 2
    result
end'

	expected := '-module(test).
-export([access_with_parens/0]).

-include("test.hrl").

-spec access_with_parens() -> integer().
access_with_parens() ->
    POINT_1 = #point{x = 10, y = 20},
    RESULT_2 = (POINT_1#point.x + POINT_1#point.y) * 2,
    RESULT_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_update_with_parentheses() {
	lx_code := 'record Point { x :: integer, y :: integer }

def update_with_parens() do
    point = Point{x: 5, y: 10}
    updated = %Point{point | x: (point.x + point.y) * 2}
    updated
end'

	expected := '-module(test).
-export([update_with_parens/0]).

-include("test.hrl").

-spec update_with_parens() -> #point{}.
update_with_parens() ->
    POINT_1 = #point{x = 5, y = 10},
    UPDATED_2 = POINT_1#point{x = (POINT_1#point.x + POINT_1#point.y) * 2},
    UPDATED_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_comparisons() {
	lx_code := 'record Person { name :: string, age :: integer }

def record_with_comparisons() do
    person1 = Person{name: "João", age: 30}
    person2 = Person{name: "Maria", age: 25}

    age1 = person1.age
    age2 = person2.age

    older = age1 > age2
    same_age = age1 == age2
    different_age = age1 != age2

    {older, same_age, different_age}
end'

	expected := '-module(test).
-export([record_with_comparisons/0]).

-include("test.hrl").

-spec record_with_comparisons() -> {boolean(), boolean(), boolean()}.
record_with_comparisons() ->
    PERSON1_1 = #person{name = <<"João"/utf8>>, age = 30},
    PERSON2_2 = #person{name = <<"Maria"/utf8>>, age = 25},
    AGE1_3 = PERSON1_1#person.age,
    AGE2_4 = PERSON2_2#person.age,
    OLDER_5 = AGE1_3 > AGE2_4,
    SAME_AGE_6 = AGE1_3 == AGE2_4,
    DIFFERENT_AGE_7 = AGE1_3 /= AGE2_4,
    {OLDER_5, SAME_AGE_6, DIFFERENT_AGE_7}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_function_calls() {
	lx_code := 'record Point { x :: integer, y :: integer }

def record_with_functions() do
    point = Point{x: 10, y: 20}

    sum = +(point.x, point.y)
    product = *(point.x, point.y)
    difference = -(point.x, point.y)

    {sum, product, difference}
end'

	expected := '-module(test).
-export([record_with_functions/0]).

-include("test.hrl").

-spec record_with_functions() -> {integer(), integer(), integer()}.
record_with_functions() ->
    POINT_1 = #point{x = 10, y = 20},
    SUM_2 = POINT_1#point.x + POINT_1#point.y,
    PRODUCT_3 = POINT_1#point.x * POINT_1#point.y,
    DIFFERENCE_4 = POINT_1#point.x - POINT_1#point.y,
    {SUM_2, PRODUCT_3, DIFFERENCE_4}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_multiline() {
	lx_code := 'record Person { name :: string, age :: integer }

def multiline_record() do
    person = Person{name: "João Silva", age: 30}
    person
end'

	expected := '-module(test).
-export([multiline_record/0]).

-include("test.hrl").

-spec multiline_record() -> #person{}.
multiline_record() ->
    PERSON_1 = #person{name = <<"João Silva"/utf8>>, age = 30},
    PERSON_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_empty_fields() {
	lx_code := 'record Empty { }

def empty_record() do
    empty = Empty{}
    empty
end'

	expected := '-module(test).
-export([empty_record/0]).

-include("test.hrl").

-spec empty_record() -> #empty{}.
empty_record() ->
    EMPTY_1 = #empty{},
    EMPTY_1.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_single_field() {
	lx_code := 'record Single { value :: integer }

def single_field() do
    single = Single{value: 42}
    value = single.value
    value
end'

	expected := '-module(test).
-export([single_field/0]).

-include("test.hrl").

-spec single_field() -> integer().
single_field() ->
    SINGLE_1 = #single{value = 42},
    VALUE_2 = SINGLE_1#single.value,
    VALUE_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_boolean_fields() {
	lx_code := 'record Settings { enabled :: boolean }

def boolean_fields() do
    settings = Settings{enabled: true}
    enabled = settings.enabled
    enabled
end'

	expected := '-module(test).
-export([boolean_fields/0]).

-include("test.hrl").

-spec boolean_fields() -> boolean().
boolean_fields() ->
    SETTINGS_1 = #settings{enabled = true},
    ENABLED_2 = SETTINGS_1#settings.enabled,
    ENABLED_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_atom_fields() {
	lx_code := 'record Status { state :: atom }

def atom_fields() do
    status = Status{state: :ok}
    state = status.state
    state
end'

	expected := '-module(test).
-export([atom_fields/0]).

-include("test.hrl").

-spec atom_fields() -> atom().
atom_fields() ->
    STATUS_1 = #status{state = ok},
    STATE_2 = STATUS_1#status.state,
    STATE_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_float_fields() {
	lx_code := 'record Measurement { temperature :: float }

def float_fields() do
    measurement = Measurement{temperature: 23.5}
    temp = measurement.temperature
    temp
end'

	expected := '-module(test).
-export([float_fields/0]).

-include("test.hrl").

-spec float_fields() -> float().
float_fields() ->
    MEASUREMENT_1 = #measurement{temperature = 23.5},
    TEMP_2 = MEASUREMENT_1#measurement.temperature,
    TEMP_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_string_fields() {
	lx_code := 'record User { username :: string }

def string_fields() do
    user = User{username: "jsilva"}
    username = user.username
    username
end'

	expected := '-module(test).
-export([string_fields/0]).

-include("test.hrl").

-spec string_fields() -> binary().
string_fields() ->
    USER_1 = #user{username = <<"jsilva"/utf8>>},
    USERNAME_2 = USER_1#user.username,
    USERNAME_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_integer_fields() {
	lx_code := 'record Counter { count :: integer }

def integer_fields() do
    counter = Counter{count: 0}
    count = counter.count
    count
end'

	expected := '-module(test).
-export([integer_fields/0]).

-include("test.hrl").

-spec integer_fields() -> integer().
integer_fields() ->
    COUNTER_1 = #counter{count = 0},
    COUNT_2 = COUNTER_1#counter.count,
    COUNT_2.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_mixed_field_types() {
	lx_code := 'record Mixed { name :: string, age :: integer }

def mixed_fields() do
    mixed = Mixed{name: "Test", age: 30}
    name = mixed.name
    age = mixed.age
    {name, age}
end'

	expected := '-module(test).
-export([mixed_fields/0]).

-include("test.hrl").

-spec mixed_fields() -> {binary(), integer()}.
mixed_fields() ->
    MIXED_1 = #mixed{name = <<"Test"/utf8>>, age = 30},
    NAME_2 = MIXED_1#mixed.name,
    AGE_3 = MIXED_1#mixed.age,
    {NAME_2, AGE_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_complex_expressions() {
	lx_code := 'record Point { x :: integer, y :: integer }

def complex_expressions() do
    point = Point{x: 10, y: 20}

    distance = point.x * point.x + point.y * point.y
    midpoint_x = (point.x + point.x) / 2
    midpoint_y = (point.y + point.y) / 2

    {distance, midpoint_x, midpoint_y}
end'

	expected := '-module(test).
-export([complex_expressions/0]).

-include("test.hrl").

-spec complex_expressions() -> {integer(), integer(), integer()}.
complex_expressions() ->
    POINT_1 = #point{x = 10, y = 20},
    DISTANCE_2 = POINT_1#point.x * POINT_1#point.x + POINT_1#point.y * POINT_1#point.y,
    MIDPOINT_X_3 = (POINT_1#point.x + POINT_1#point.x) / 2,
    MIDPOINT_Y_4 = (POINT_1#point.y + POINT_1#point.y) / 2,
    {DISTANCE_2, MIDPOINT_X_3, MIDPOINT_Y_4}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_default_values() {
	lx_code := 'record Person { name :: string, age = 10 :: integer, active = true :: boolean }

def main() do
    user1 = Person{name: "fulano", age: 25}
    user2 = Person{name: "ciclano"}
    name1 = user1.name
    age1 = user1.age
    active1 = user1.active
    {user1, user2, name1, age1, active1}
end'

	expected := '-module(test).
-export([main/0]).

-include("test.hrl").

-spec main() -> {#person{}, #person{}, binary(), integer(), boolean()}.
main() ->
    USER1_1 = #person{name = <<"fulano"/utf8>>, age = 25},
    USER2_2 = #person{name = <<"ciclano"/utf8>>},
    NAME1_3 = USER1_1#person.name,
    AGE1_4 = USER1_1#person.age,
    ACTIVE1_5 = USER1_1#person.active,
    {USER1_1, USER2_2, NAME1_3, AGE1_4, ACTIVE1_5}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_mixed_defaults() {
	lx_code := 'record Config { host = "localhost" :: string, port = 8080 :: integer, debug :: boolean, timeout = 30.0 :: float }

def main() do
    config1 = Config{host: "example.com", debug: true}
    config2 = Config{debug: false, timeout: 60.0}
    host1 = config1.host
    port1 = config1.port
    debug1 = config1.debug
    timeout1 = config1.timeout
    {config1, config2, host1, port1, debug1, timeout1}
end'

	expected := '-module(test).
-export([main/0]).

-include("test.hrl").

-spec main() -> {#config{}, #config{}, binary(), integer(), boolean(), float()}.
main() ->
    CONFIG1_1 = #config{host = <<"example.com"/utf8>>, debug = true},
    CONFIG2_2 = #config{debug = false, timeout = 60.0},
    HOST1_3 = CONFIG1_1#config.host,
    PORT1_4 = CONFIG1_1#config.port,
    DEBUG1_5 = CONFIG1_1#config.debug,
    TIMEOUT1_6 = CONFIG1_1#config.timeout,
    {CONFIG1_1, CONFIG2_2, HOST1_3, PORT1_4, DEBUG1_5, TIMEOUT1_6}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_with_mixed_defaults_inferred() {
	lx_code := 'record Config { host = "localhost", port = 8080, debug :: boolean, timeout = 30.0}

def main() do
    config1 = Config{host: "example.com", debug: true}
    config2 = Config{debug: false, timeout: 60.0}
    host1 = config1.host
    port1 = config1.port
    debug1 = config1.debug
    timeout1 = config1.timeout
    {config1, config2, host1, port1, debug1, timeout1}
end'

	expected := '-module(test).
-export([main/0]).

-include("test.hrl").

-spec main() -> {#config{}, #config{}, binary(), integer(), boolean(), float()}.
main() ->
    CONFIG1_1 = #config{host = <<"example.com"/utf8>>, debug = true},
    CONFIG2_2 = #config{debug = false, timeout = 60.0},
    HOST1_3 = CONFIG1_1#config.host,
    PORT1_4 = CONFIG1_1#config.port,
    DEBUG1_5 = CONFIG1_1#config.debug,
    TIMEOUT1_6 = CONFIG1_1#config.timeout,
    {CONFIG1_1, CONFIG2_2, HOST1_3, PORT1_4, DEBUG1_5, TIMEOUT1_6}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_record_multiline_fields() {
	lx_code := 'record User {
  id :: integer,
  name :: string,
  email :: string,
  created_at :: integer
}

record Session {
  user_id :: integer,
  token :: string,
  expires_at :: integer
}

def main() do
  user = User{id: 1, name: "John", email: "john@example.com", created_at: 1234567890}
  session = Session{user_id: 1, token: "abc123", expires_at: 1234567890}
  {user, session}
end'

	expected := '-module(test).
-export([main/0]).

-include("test.hrl").

-spec main() -> {#user{}, #session{}}.
main() ->
    USER_1 = #user{id = 1, name = <<"John"/utf8>>, email = <<"john@example.com"/utf8>>, created_at = 1234567890},
    SESSION_2 = #session{user_id = 1, token = <<"abc123"/utf8>>, expires_at = 1234567890},
    {USER_1, SESSION_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}
