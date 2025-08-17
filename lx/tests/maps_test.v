module main

fn test_empty_map() {
	lx_code := 'def empty() do
    %{}
end'

	expected := '-module(test).
-export([empty/0]).

-spec empty() -> map().
empty() ->
    #{}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_simple_map_literal() {
	lx_code := 'def user() do
    %{name: "João", age: 30}
end'

	expected := '-module(test).
-export([user/0]).

-spec user() -> #{any() => any()}.
user() ->
    #{name => <<"João"/utf8>>, age => 30}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_mixed_type_map() {
	lx_code := 'def mixed() do
    %{id: 1, name: "Ana", "key": "value", 42: "answer", true: "boolean"}
end'

	expected := '-module(test).
-export([mixed/0]).

-spec mixed() -> #{any() => any()}.
mixed() ->
    #{id => 1, name => <<"Ana"/utf8>>, <<"key"/utf8>> => <<"value"/utf8>>, 42 => <<"answer"/utf8>>, true => <<"boolean"/utf8>>}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_nested_maps() {
	lx_code := 'def nested() do
    %{user: %{name: "Ana", age: 25}, settings: %{theme: "dark"}}
end'

	expected := '-module(test).
-export([nested/0]).

-spec nested() -> #{any() => any()}.
nested() ->
    #{user => #{name => <<"Ana"/utf8>>, age => 25}, settings => #{theme => <<"dark"/utf8>>}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_size() {
	lx_code := 'def size_example() do
    map = %{name: "Ana", age: 25, city: "SP", active: true}
    map_size(map)
end'

	expected := '-module(test).
-export([size_example/0]).

-spec size_example() -> integer().
size_example() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, age => 25, city => <<"SP"/utf8>>, active => true},
    map_size(MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_get_atom_key() {
	lx_code := 'def get_atom() do
    map = %{name: "João", age: 30}
    map_get(:name, map)
end'

	expected := '-module(test).
-export([get_atom/0]).

-spec get_atom() -> any().
get_atom() ->
    MAP_1 = #{name => <<"João"/utf8>>, age => 30},
    maps:get(name, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_get_string_key() {
	lx_code := 'def get_string() do
    map = %{name: "Ana", "key": "value"}
    map_get("key", map)
end'

	expected := '-module(test).
-export([get_string/0]).

-spec get_string() -> any().
get_string() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, <<"key"/utf8>> => <<"value"/utf8>>},
    maps:get(<<"key"/utf8>>, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_get_integer_key() {
	lx_code := 'def get_integer() do
    map = %{name: "Ana", 42: "answer"}
    map_get(42, map)
end'

	expected := '-module(test).
-export([get_integer/0]).

-spec get_integer() -> any().
get_integer() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, 42 => <<"answer"/utf8>>},
    maps:get(42, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_put_atom_key() {
	lx_code := 'def put_atom() do
    map = %{name: "João", age: 30}
    map_put(:age, 31, map)
end'

	expected := '-module(test).
-export([put_atom/0]).

-spec put_atom() -> map().
put_atom() ->
    MAP_1 = #{name => <<"João"/utf8>>, age => 30},
    maps:put(age, 31, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_put_string_key() {
	lx_code := 'def put_string() do
    map = %{name: "Ana", "key": "old"}
    map_put("key", "new", map)
end'

	expected := '-module(test).
-export([put_string/0]).

-spec put_string() -> map().
put_string() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, <<"key"/utf8>> => <<"old"/utf8>>},
    maps:put(<<"key"/utf8>>, <<"new"/utf8>>, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_remove_atom_key() {
	lx_code := 'def remove_atom() do
    map = %{name: "Ana", age: 25, temp: "value"}
    map_remove(:temp, map)
end'

	expected := '-module(test).
-export([remove_atom/0]).

-spec remove_atom() -> map().
remove_atom() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, age => 25, temp => <<"value"/utf8>>},
    maps:remove(temp, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_remove_string_key() {
	lx_code := 'def remove_string() do
    map = %{name: "Ana", "key": "data"}
    map_remove("key", map)
end'

	expected := '-module(test).
-export([remove_string/0]).

-spec remove_string() -> map().
remove_string() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, <<"key"/utf8>> => <<"data"/utf8>>},
    maps:remove(<<"key"/utf8>>, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_variables() {
	lx_code := 'def map_with_vars() do
    name = "João"
    age = 30
    active = true
    %{name: name, age: age, active: active}
end'

	expected := '-module(test).
-export([map_with_vars/0]).

-spec map_with_vars() -> #{any() => any()}.
map_with_vars() ->
    NAME_1 = <<"João"/utf8>>,
    AGE_2 = 30,
    ACTIVE_3 = true,
    #{name => NAME_1, age => AGE_2, active => ACTIVE_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_expressions() {
	lx_code := 'def map_with_expr() do
    a = 5
    b = 3
    %{sum: a + b, product: a * b, difference: a - b}
end'

	expected := '-module(test).
-export([map_with_expr/0]).

-spec map_with_expr() -> #{any() => any()}.
map_with_expr() ->
    A_1 = 5,
    B_2 = 3,
    #{sum => A_1 + B_2, product => A_1 * B_2, difference => A_1 - B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_parentheses() {
	lx_code := 'def map_with_parens() do
    a = 2
    b = 3
    %{result1: (a + b) * 2, result2: (a - b) * 2}
end'

	expected := '-module(test).
-export([map_with_parens/0]).

-spec map_with_parens() -> #{any() => any()}.
map_with_parens() ->
    A_1 = 2,
    B_2 = 3,
    #{result1 => (A_1 + B_2) * 2, result2 => (A_1 - B_2) * 2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_deeply_nested_maps() {
	lx_code := 'def deep_nested() do
    %{level1: %{level2: %{level3: "deep"}}}
end'

	expected := '-module(test).
-export([deep_nested/0]).

-spec deep_nested() -> #{any() => any()}.
deep_nested() ->
    #{level1 => #{level2 => #{level3 => <<"deep"/utf8>>}}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_all_literals() {
	lx_code := 'def all_literals() do
    %{integer: 42, float: 3.14, string: "hello", atom: :ok, boolean: true, nil: nil}
end'

	expected := '-module(test).
-export([all_literals/0]).

-spec all_literals() -> #{any() => any()}.
all_literals() ->
    #{integer => 42, float => 3.14, string => <<"hello"/utf8>>, atom => ok, boolean => true, nil => nil}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_comparisons() {
	lx_code := 'def comparison_map() do
    a = 5
    b = 3
    %{greater: a > b, equal: a == b, not_equal: a != b}
end'

	expected := '-module(test).
-export([comparison_map/0]).

-spec comparison_map() -> #{any() => any()}.
comparison_map() ->
    A_1 = 5,
    B_2 = 3,
    #{greater => A_1 > B_2, equal => A_1 == B_2, not_equal => A_1 /= B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_logical_operators() {
	lx_code := 'def logical_map() do
    a = true
    b = false
    %{and_result: a and b, or_result: a or b}
end'

	expected := '-module(test).
-export([logical_map/0]).

-spec logical_map() -> #{any() => any()}.
logical_map() ->
    A_1 = true,
    B_2 = false,
    #{and_result => A_1 andalso B_2, or_result => A_1 orelse B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_bitwise_operators() {
	lx_code := 'def bitwise_map() do
    a = 5
    b = 3
    %{and_result: a &&& b, or_result: a ||| b, xor_result: a ^^^ b}
end'

	expected := '-module(test).
-export([bitwise_map/0]).

-spec bitwise_map() -> #{any() => any()}.
bitwise_map() ->
    A_1 = 5,
    B_2 = 3,
    #{and_result => A_1 band B_2, or_result => A_1 bor B_2, xor_result => A_1 bxor B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_shift_operators() {
	lx_code := 'def shift_map() do
    a = 8
    %{left_shift: a <<< 2, right_shift: a >>> 1}
end'

	expected := '-module(test).
-export([shift_map/0]).

-spec shift_map() -> #{any() => any()}.
shift_map() ->
    A_1 = 8,
    #{left_shift => A_1 bsl 2, right_shift => A_1 bsr 1}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_floating_point() {
	lx_code := 'def float_map() do
    %{pi: 3.14, e: 2.718, one: 1.0}
end'

	expected := '-module(test).
-export([float_map/0]).

-spec float_map() -> #{any() => any()}.
float_map() ->
    #{pi => 3.14, e => 2.718, one => 1.0}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_atoms() {
	lx_code := 'def atom_map() do
    %{status: :ok, error: :error, timeout: :timeout}
end'

	expected := '-module(test).
-export([atom_map/0]).

-spec atom_map() -> #{any() => any()}.
atom_map() ->
    #{status => ok, error => error, timeout => timeout}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_booleans() {
	lx_code := 'def boolean_map() do
    %{flag1: true, flag2: false, flag3: true}
end'

	expected := '-module(test).
-export([boolean_map/0]).

-spec boolean_map() -> #{any() => any()}.
boolean_map() ->
    #{flag1 => true, flag2 => false, flag3 => true}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_nil() {
	lx_code := 'def nil_map() do
    %{value1: nil, value2: nil, value3: nil}
end'

	expected := '-module(test).
-export([nil_map/0]).

-spec nil_map() -> #{any() => any()}.
nil_map() ->
    #{value1 => nil, value2 => nil, value3 => nil}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_mixed_types() {
	lx_code := 'def mixed_map() do
    %{number: 42, text: "hello", flag: :ok, value: 3.14, active: true}
end'

	expected := '-module(test).
-export([mixed_map/0]).

-spec mixed_map() -> #{any() => any()}.
mixed_map() ->
    #{number => 42, text => <<"hello"/utf8>>, flag => ok, value => 3.14, active => true}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_size_empty() {
	lx_code := 'def size_empty() do
    map_size(%{})
end'

	expected := '-module(test).
-export([size_empty/0]).

-spec size_empty() -> integer().
size_empty() ->
    map_size(#{}).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_get_nonexistent() {
	lx_code := 'def get_nonexistent() do
    map = %{name: "Ana", age: 25}
    map_get(:nonexistent, map)
end'

	expected := '-module(test).
-export([get_nonexistent/0]).

-spec get_nonexistent() -> any().
get_nonexistent() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, age => 25},
    maps:get(nonexistent, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_remove_nonexistent() {
	lx_code := 'def remove_nonexistent() do
    map = %{name: "Ana", age: 25}
    map_remove(:nonexistent, map)
end'

	expected := '-module(test).
-export([remove_nonexistent/0]).

-spec remove_nonexistent() -> map().
remove_nonexistent() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, age => 25},
    maps:remove(nonexistent, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_function_calls() {
	lx_code := 'def function_calls() do
    a = 10
    b = 5
    %{sum: +(a, b), product: *(a, b), difference: -(a, b)}
end'

	expected := '-module(test).
-export([function_calls/0]).

-spec function_calls() -> #{any() => any()}.
function_calls() ->
    A_1 = 10,
    B_2 = 5,
    #{sum => A_1 + B_2, product => A_1 * B_2, difference => A_1 - B_2}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_parentheses_expressions() {
	lx_code := 'def parens_expressions() do
    a = 2
    b = 3
    c = 4
    %{result1: (a + b) * c, result2: (a - b) * c, result3: (a * b) + c}
end'

	expected := '-module(test).
-export([parens_expressions/0]).

-spec parens_expressions() -> #{any() => any()}.
parens_expressions() ->
    A_1 = 2,
    B_2 = 3,
    C_3 = 4,
    #{result1 => (A_1 + B_2) * C_3, result2 => (A_1 - B_2) * C_3, result3 => (A_1 * B_2) + C_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_variable_references() {
	lx_code := 'def var_refs() do
    x = 10
    y = 20
    z = 30
    %{first: x, second: y, third: z, sum: x + y + z}
end'

	expected := '-module(test).
-export([var_refs/0]).

-spec var_refs() -> #{any() => any()}.
var_refs() ->
    X_1 = 10,
    Y_2 = 20,
    Z_3 = 30,
    #{first => X_1, second => Y_2, third => Z_3, sum => X_1 + Y_2 + Z_3}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_size_with_variable_reference() {
	lx_code := 'def size_var_ref() do
    my_map = %{name: "Ana", age: 25, city: "SP"}
    map_size(my_map)
end'

	expected := '-module(test).
-export([size_var_ref/0]).

-spec size_var_ref() -> integer().
size_var_ref() ->
    MY_MAP_1 = #{name => <<"Ana"/utf8>>, age => 25, city => <<"SP"/utf8>>},
    map_size(MY_MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_get_with_variable_references() {
	lx_code := 'def get_var_refs() do
    my_map = %{name: "Ana", age: 25}
    my_key = :name
    map_get(my_key, my_map)
end'

	expected := '-module(test).
-export([get_var_refs/0]).

-spec get_var_refs() -> any().
get_var_refs() ->
    MY_MAP_1 = #{name => <<"Ana"/utf8>>, age => 25},
    MY_KEY_2 = name,
    maps:get(MY_KEY_2, MY_MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_put_with_variable_references() {
	lx_code := 'def put_var_refs() do
    my_map = %{name: "Ana", age: 25}
    my_key = :age
    my_value = 26
    map_put(my_key, my_value, my_map)
end'

	expected := '-module(test).
-export([put_var_refs/0]).

-spec put_var_refs() -> map().
put_var_refs() ->
    MY_MAP_1 = #{name => <<"Ana"/utf8>>, age => 25},
    MY_KEY_2 = age,
    MY_VALUE_3 = 26,
    maps:put(MY_KEY_2, MY_VALUE_3, MY_MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_remove_with_variable_references() {
	lx_code := 'def remove_var_refs() do
    my_map = %{name: "Ana", age: 25, temp: "value"}
    my_key = :temp
    map_remove(my_key, my_map)
end'

	expected := '-module(test).
-export([remove_var_refs/0]).

-spec remove_var_refs() -> map().
remove_var_refs() ->
    MY_MAP_1 = #{name => <<"Ana"/utf8>>, age => 25, temp => <<"value"/utf8>>},
    MY_KEY_2 = temp,
    maps:remove(MY_KEY_2, MY_MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_complex_map_operations() {
	lx_code := 'def complex_operations() do
    user = %{id: 1, name: "João", age: 30, "email": "joao@example.com", 42: "magic"}
    config = %{database: %{host: "localhost", port: 5432}, "api_key": "secret123"}
    _name = map_get(:name, user)
    _age = map_get(:age, user)
    _email = map_get("email", user)
    _magic = map_get(42, user)
    updated_user = map_put(:age, 31, user)
    updated_user2 = map_put("email", "new@example.com", updated_user)
    clean_user = map_remove(:id, updated_user2)
    clean_user2 = map_remove("email", clean_user)
    {user, config, clean_user2}
end'

	expected := '-module(test).
-export([complex_operations/0]).

-spec complex_operations() -> {#{any() => any()}, #{any() => any()}, map()}.
complex_operations() ->
    USER_1 = #{id => 1, name => <<"João"/utf8>>, age => 30, <<"email"/utf8>> => <<"joao@example.com"/utf8>>, 42 => <<"magic"/utf8>>},
    CONFIG_2 = #{database => #{host => <<"localhost"/utf8>>, port => 5432}, <<"api_key"/utf8>> => <<"secret123"/utf8>>},
    _NAME_3 = maps:get(name, USER_1),
    _AGE_4 = maps:get(age, USER_1),
    _EMAIL_5 = maps:get(<<"email"/utf8>>, USER_1),
    _MAGIC_6 = maps:get(42, USER_1),
    UPDATED_USER_7 = maps:put(age, 31, USER_1),
    UPDATED_USER2_8 = maps:put(<<"email"/utf8>>, <<"new@example.com"/utf8>>, UPDATED_USER_7),
    CLEAN_USER_9 = maps:remove(id, UPDATED_USER2_8),
    CLEAN_USER2_10 = maps:remove(<<"email"/utf8>>, CLEAN_USER_9),
    {USER_1, CONFIG_2, CLEAN_USER2_10}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_multiple_map_operations() {
	lx_code := 'def multiple_ops() do
    map1 = %{a: 1, b: 2}
    map2 = %{c: 3, d: 4}
    size1 = map_size(map1)
    size2 = map_size(map2)
    value1 = map_get(:a, map1)
    value2 = map_get(:c, map2)
    updated1 = map_put(:e, 5, map1)
    updated2 = map_put(:f, 6, map2)
    removed1 = map_remove(:b, updated1)
    removed2 = map_remove(:d, updated2)
    {size1, size2, value1, value2, removed1, removed2}
end'

	expected := '-module(test).
-export([multiple_ops/0]).

-spec multiple_ops() -> {integer(), integer(), any(), any(), map(), map()}.
multiple_ops() ->
    MAP1_1 = #{a => 1, b => 2},
    MAP2_2 = #{c => 3, d => 4},
    SIZE1_3 = map_size(MAP1_1),
    SIZE2_4 = map_size(MAP2_2),
    VALUE1_5 = maps:get(a, MAP1_1),
    VALUE2_6 = maps:get(c, MAP2_2),
    UPDATED1_7 = maps:put(e, 5, MAP1_1),
    UPDATED2_8 = maps:put(f, 6, MAP2_2),
    REMOVED1_9 = maps:remove(b, UPDATED1_7),
    REMOVED2_10 = maps:remove(d, UPDATED2_8),
    {SIZE1_3, SIZE2_4, VALUE1_5, VALUE2_6, REMOVED1_9, REMOVED2_10}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_lists() {
	lx_code := 'def map_with_lists() do
    %{numbers: [1, 2, 3], names: ["Ana", "João"], mixed: [1, "hello", :ok]}
end'

	expected := '-module(test).
-export([map_with_lists/0]).

-spec map_with_lists() -> #{any() => any()}.
map_with_lists() ->
    #{numbers => [1, 2, 3], names => [<<"Ana"/utf8>>, <<"João"/utf8>>], mixed => [1, <<"hello"/utf8>>, ok]}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_tuples() {
	lx_code := 'def map_with_tuples() do
    %{point: {10, 20}, person: {"Ana", 25}, mixed: {1, "hello", :ok}}
end'

	expected := '-module(test).
-export([map_with_tuples/0]).

-spec map_with_tuples() -> #{any() => any()}.
map_with_tuples() ->
    #{point => {10, 20}, person => {<<"Ana"/utf8>>, 25}, mixed => {1, <<"hello"/utf8>>, ok}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_nested_structures() {
	lx_code := 'def nested_structures() do
    %{user: %{profile: %{name: "Ana", age: 25}, settings: %{theme: "dark"}}, data: [1, 2, 3]}
end'

	expected := '-module(test).
-export([nested_structures/0]).

-spec nested_structures() -> #{any() => any()}.
nested_structures() ->
    #{user => #{profile => #{name => <<"Ana"/utf8>>, age => 25}, settings => #{theme => <<"dark"/utf8>>}}, data => [1, 2, 3]}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

// Native Map Access Tests
fn test_map_access_atom_key() {
	lx_code := 'def access_atom() do
    map = %{name: "João", age: 30}
    map[:name]
end'

	expected := '-module(test).
-export([access_atom/0]).

-spec access_atom() -> any().
access_atom() ->
    MAP_1 = #{name => <<"João"/utf8>>, age => 30},
    maps:get(name, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_access_string_key() {
	lx_code := 'def access_string() do
    map = %{name: "Ana", "key": "value"}
    map["key"]
end'

	expected := '-module(test).
-export([access_string/0]).

-spec access_string() -> any().
access_string() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, <<"key"/utf8>> => <<"value"/utf8>>},
    maps:get(<<"key"/utf8>>, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_access_integer_key() {
	lx_code := 'def access_integer() do
    map = %{name: "Ana", 42: "answer"}
    map[42]
end'

	expected := '-module(test).
-export([access_integer/0]).

-spec access_integer() -> any().
access_integer() ->
    MAP_1 = #{name => <<"Ana"/utf8>>, 42 => <<"answer"/utf8>>},
    maps:get(42, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_access_with_variable() {
	lx_code := 'def access_with_var() do
    map = %{name: "João", age: 30}
    key = :name
    map[key]
end'

	expected := '-module(test).
-export([access_with_var/0]).

-spec access_with_var() -> any().
access_with_var() ->
    MAP_1 = #{name => <<"João"/utf8>>, age => 30},
    KEY_2 = name,
    maps:get(KEY_2, MAP_1).
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_access_complex() {
	lx_code := 'def access_complex() do
    user = %{name: "Ana", age: 25, "email": "ana@example.com", 42: "magic"}
    name = user[:name]
    age = user[:age]
    email = user["email"]
    magic = user[42]
    {name, age, email, magic}
end'

	expected := '-module(test).
-export([access_complex/0]).

-spec access_complex() -> {any(), any(), any(), any()}.
access_complex() ->
    USER_1 = #{name => <<"Ana"/utf8>>, age => 25, <<"email"/utf8>> => <<"ana@example.com"/utf8>>, 42 => <<"magic"/utf8>>},
    NAME_2 = maps:get(name, USER_1),
    AGE_3 = maps:get(age, USER_1),
    EMAIL_4 = maps:get(<<"email"/utf8>>, USER_1),
    MAGIC_5 = maps:get(42, USER_1),
    {NAME_2, AGE_3, EMAIL_4, MAGIC_5}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_multiline() {
	lx_code := 'def multiline_map() do
    %{"name": "João",
      age: 30,
      active: true}
end'

	expected := '-module(test).
-export([multiline_map/0]).

-spec multiline_map() -> #{any() => any()}.
multiline_map() ->
    #{<<"name"/utf8>> => <<"João"/utf8>>, age => 30, active => true}.
'

	result := compile_lx(lx_code)
	assert result == expected
}

fn test_map_with_multiline_nested() {
	lx_code := 'def multiline_nested() do
    %{user: %{name: "Ana",
               age: 25,
               settings: %{theme: "dark",
                          notifications: true}},
      config: %{database: "postgres",
                port: 5432}}
end'

	expected := '-module(test).
-export([multiline_nested/0]).

-spec multiline_nested() -> #{any() => any()}.
multiline_nested() ->
    #{user => #{name => <<"Ana"/utf8>>, age => 25, settings => #{theme => <<"dark"/utf8>>, notifications => true}}, config => #{database => <<"postgres"/utf8>>, port => 5432}}.
'

	result := compile_lx(lx_code)
	assert result == expected
}
