module main

fn test_record_creation_simple() {
	lx_code := '
record Person {
  name :: string,
  age :: integer
}

def create_person() do
  Person{name: "Alice", age: 30}
end'
	expected := '-module(test).
-include("test.hrl").

-export([create_person/0]).


-spec create_person() -> #person{}.
create_person() ->
#person{name = <<"Alice"/utf8>>, age = 30}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(person, {name, age}).
'
}

fn test_record_field_access() {
	lx_code := '
record Person {
  name :: string,
  age :: integer
}

def get_name() do
  person = Person{name: "Bob", age: 25}
  person.name
end'
	expected := '-module(test).
-include("test.hrl").

-export([get_name/0]).


-spec get_name() -> binary().
get_name() ->
Person_aaaa = #person{name = <<"Bob"/utf8>>, age = 25},
Person_aaaa#person.name.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(person, {name, age}).
'
}

fn test_record_update_single_field() {
	lx_code := '
record Person {
  name :: string,
  age :: integer
}

def update_age() do
  person = Person{name: "Charlie", age: 35}
  Person{person | age: 36}
end'
	expected := '-module(test).
-include("test.hrl").

-export([update_age/0]).


-spec update_age() -> any().
update_age() ->
Person_aaaa = #person{name = <<"Charlie"/utf8>>, age = 35},
Person_aaaa#{age => 36}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(person, {name, age}).
'
}

fn test_record_update_multiple_fields() {
	lx_code := '
record Person {
  name :: string,
  age :: integer,
  email :: string
}

def update_person() do
  person = Person{name: "David", age: 40, email: "david@old.com"}
  Person{person | age: 41, email: "david@new.com"}
end'
	expected := '-module(test).
-include("test.hrl").

-export([update_person/0]).


-spec update_person() -> any().
update_person() ->
Person_aaaa = #person{name = <<"David"/utf8>>, age = 40, email = <<"david@old.com"/utf8>>},
Person_aaaa#{age => 41, email => <<"david@new.com"/utf8>>}.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(person, {name, age, email}).
'
}

fn test_record_patterns_simple() {
	lx_code := '
record User {
  id :: integer,
  email :: string
}

def get_user_id(user) do
  case user do
    User{id: user_id} -> user_id
    _ -> -1
  end
end'
	expected := '-module(test).
-include("test.hrl").

-export([get_user_id/1]).


-spec get_user_id(any()) -> integer().
get_user_id(User) ->
case User of
    #user{id = User_id} -> User_id;
    _ -> -1
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(user, {id, email}).
'
}

fn test_record_patterns_with_case_and_guards() {
	lx_code := '
record Person {
  name :: string,
  age :: integer
}

def process_person() do
  person = Person{name: "Eve", age: 28}
  case person do
    Person{name: "Eve", age: age} -> age
    Person{age: age} when age >= 18 -> age
    _ -> 0
  end
end'
	expected := '-module(test).
-include("test.hrl").

-export([process_person/0]).


-spec process_person() -> integer().
process_person() ->
Person_aaaa = #person{name = <<"Eve"/utf8>>, age = 28},
case Person_aaaa of
    #person{name = <<"Eve"/utf8>>, age = Age} -> Age;
    #person{age = Age} when Age >= 18 -> Age;
    _ -> 0
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(person, {name, age}).
'
}

fn test_record_patterns_multiple_fields() {
	lx_code := '
record Contact {
  name :: string,
  email :: string,
  phone :: string
}

def validate_contact(contact) do
  case contact do
    Contact{name: name, email: email} when name != "" -> {name, email}
    Contact{phone: phone} when phone != "" -> {"unknown", phone}
    _ -> {"unknown", "unknown"}
  end
end'
	expected := '-module(test).
-include("test.hrl").

-export([validate_contact/1]).


-spec validate_contact(any()) -> {binary(), binary()}.
validate_contact(Contact) ->
case Contact of
    #contact{name = Name, email = Email} when Name =/= <<""/utf8>> -> {Name, Email};
    #contact{phone = Phone} when Phone =/= <<""/utf8>> -> {<<"unknown"/utf8>>, Phone};
    _ -> {<<"unknown"/utf8>>, <<"unknown"/utf8>>}
end.

'
	code, hrl_content := generates_erlang(lx_code)
	assert code == expected
	assert hrl_content == '%% Generated header file for test module
%% Contains records and type definitions

-record(contact, {name, email, phone}).
'
}

// fn test_record_patterns_with() {
// 	lx_code := '
// record User {
//   id :: integer,
//   active :: boolean
// }

// def check_user_status(user) do
// 	with User{id: id, active: true} when id > 0 <- user do
// 		"active_user"
// 	else
// end'
// 	expected := '-module(main).
// -export([check_user_status/1]).

// -record(user, {id, active}).
// -spec check_user_status(any()) -> string().
// check_user_status(User) ->
// case User of
//     #user{id = Id, active = true} when Id > 0 -> "active_user";
//     #user{id = Id, active = false} when Id > 0 -> "inactive_user";
//     _ -> "invalid_user"
// end.

// '
// 	erl1 := generates_erlang(lx_code)
// 	assert erl1.success
// 	assert erl1.code == expected
// }
