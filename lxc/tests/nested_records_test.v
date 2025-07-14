module main

fn test_nested_records_basic() {
	lx_code := '
record Profile {
  age :: integer
}

record User {
  profile :: Profile
}

def check_adult_user(user) do
	with User{profile: Profile{age: age}} when age >= 18 <- user do
		"adult_user"
	else
		"minor_user"
	end
end'
	expected := '-module(test).
-export([check_adult_user/1]).

-record(profile, {age}).
-record(user, {profile}).
-spec check_adult_user(any()) -> binary().
check_adult_user(User) ->
case User of
    #user{profile = #profile{age = Age}} when Age >= 18 ->
        <<"adult_user"/utf8>>;
    Other ->
        <<"minor_user"/utf8>>
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_multiple_nested_records() {
	lx_code := '
record Address {
  street :: string,
  city :: string
}

record Profile {
  age :: integer,
  address :: Address
}

record User {
  name :: string,
  profile :: Profile
}

def get_user_city(user) do
	with User{profile: Profile{address: Address{city: city}}} <- user do
		city
	else
		"unknown"
	end
end'
	expected := '-module(test).
-export([get_user_city/1]).

-record(address, {street, city}).
-record(profile, {age, address}).
-record(user, {name, profile}).
-spec get_user_city(any()) -> binary().
get_user_city(User) ->
case User of
    #user{profile = #profile{address = #address{city = City}}} ->
        City;
    Other ->
        <<"unknown"/utf8>>
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_nested_records_with_guards() {
	lx_code := '
record Profile {
  age :: integer
}

record User {
  name :: string,
  profile :: Profile
}

def check_user_eligibility(user) do
	with User{name: name, profile: Profile{age: age}} when age >= 21 <- user do
		name
	else
		"not_eligible"
	end
end'
	expected := '-module(test).
-export([check_user_eligibility/1]).

-record(profile, {age}).
-record(user, {name, profile}).
-spec check_user_eligibility(any()) -> binary().
check_user_eligibility(User) ->
case User of
    #user{name = Name, profile = #profile{age = Age}} when Age >= 21 ->
        Name;
    Other ->
        <<"not_eligible"/utf8>>
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_record_with_list_of_custom_types() {
	lx_code := '
record Person {
  name :: string,
  age :: integer
}

record Company {
  name :: string,
  employees :: list(Person)
}

def create_company() do
	Company{name: "Tech Corp", employees: []}
end'
	expected := '-module(test).
-export([create_company/0]).

-record(person, {name, age}).
-record(company, {name, employees}).
-spec create_company() -> #company{}.
create_company() ->
#company{name = <<"Tech Corp"/utf8>>, employees = []}.

'
	assert generates_erlang(lx_code) == expected
}

fn test_deeply_nested_records() {
	lx_code := '
record Country {
  name :: string
}

record City {
  name :: string,
  country :: Country
}

record Address {
  street :: string,
  city :: City
}

record Person {
  name :: string,
  address :: Address
}

def get_person_country(person) do
	with Person{address: Address{city: City{country: Country{name: country}}}} <- person do
		country
	else
		"unknown"
	end
end'
	expected := '-module(test).
-export([get_person_country/1]).

-record(country, {name}).
-record(city, {name, country}).
-record(address, {street, city}).
-record(person, {name, address}).
-spec get_person_country(any()) -> binary().
get_person_country(Person) ->
case Person of
    #person{address = #address{city = #city{country = #country{name = Country}}}} ->
        Country;
    Other ->
        <<"unknown"/utf8>>
end.

'
	assert generates_erlang(lx_code) == expected
}

fn test_nested_records_creation() {
	lx_code := '
record Profile {
  age :: integer
}

record User {
  name :: string,
  profile :: Profile
}

def create_user() do
	User{name: "John", profile: Profile{age: 25}}
end'
	expected := '-module(test).
-export([create_user/0]).

-record(profile, {age}).
-record(user, {name, profile}).
-spec create_user() -> #user{}.
create_user() ->
#user{name = <<"John"/utf8>>, profile = #profile{age = 25}}.

'
	assert generates_erlang(lx_code) == expected
}
