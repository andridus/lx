-module(complex).
-export([create_person/3, create_address/3, get_person_info/1, calculate_birth_year/1, is_adult/1, format_address/1]).

-record(person, {name, age, email}).
-record(address, {street, city, country}).



create_person(Name, Age, Email) ->
    #person{name = Name, age = Age, email = Email}.
create_address(Street, City, Country) ->
    #address{street = Street, city = City, country = Country}.
get_person_info(Person) ->
    "Person info".
calculate_birth_year(Current_age) ->
    2024 - Current_age.
is_adult(Person) ->
    18.
format_address(Address) ->
    "Address info".