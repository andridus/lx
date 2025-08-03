-module(lx2_types).

-include("lx2.hrl").

-export([infer_expression/2, infer_block/2, new_type_env/0, extend_env/3, infer_variable_binding/3, infer_types/1, infer_literal/1]).

%% Type environment
%% These types are used for type inference and will be used in future implementations
%%-type type_env() :: #{lx_identifier() => lx_type()}.
%%-type lx_type() :: {type_const, atom()} | {type_var, atom()}.
%%-type substitution() :: #{atom() => lx_type()}.

%% Create new type environment
new_type_env() -> #{}.

%% Extend type environment with new variable
extend_env(Var, Type, Env) ->
    maps:put(Var, Type, Env).

%% Lookup variable in type environment
lookup_env(Var, Env) ->
    case maps:find(Var, Env) of
        {ok, Type} -> {ok, Type};
        error -> not_found
    end.

%% Infer type for expression
infer_expression(Expr, Env) ->
    if
        is_tuple(Expr) andalso tuple_size(Expr) =:= 2 ->
            case element(1, Expr) of
                variable_ref ->
                    case infer_variable_ref(element(2, Expr), Env) of
                        {ok, Type, Sub} -> {ok, Type, Sub};
                        {error, Error} -> {error, Error}
                    end;
                _ ->
                    {error, {unknown_expression, Expr}}
            end;
        is_tuple(Expr) andalso tuple_size(Expr) =:= 3 ->
            case element(1, Expr) of
                literal ->
                    {Type, Sub} = infer_literal(element(2, Expr)),
                    {ok, Type, Sub};
                variable_binding ->
                    case infer_variable_binding(element(2, Expr), element(3, Expr), Env) of
                        {ok, Type, Sub, NewEnv} -> {ok, Type, Sub, NewEnv};
                        {error, Error} -> {error, Error}
                    end;
                _ ->
                    {error, {unknown_expression, Expr}}
            end;
        true ->
            {error, {unknown_expression, Expr}}
    end.

%% Infer type for literal
infer_literal(Type) ->
    case Type of
        integer -> {new_type_const(integer), new_substitution()};
        float -> {new_type_const(float), new_substitution()};
        string -> {new_type_const(binary), new_substitution()};
        atom -> {new_type_const(atom), new_substitution()};
        boolean -> {new_type_const(boolean), new_substitution()};
        nil -> {new_type_const(nil), new_substitution()}
    end.

%% Infer type for variable reference
infer_variable_ref(Var, Env) ->
    case lookup_env(Var, Env) of
        {ok, Type} -> {ok, Type, new_substitution()};
        not_found ->
            {error, {undefined_variable, Var}}
    end.

%% Infer type for variable binding
infer_variable_binding(Var, Expr, Env) ->
    case Expr of
        {literal, Type, _Value} ->
            {ExprType, _Sub} = infer_literal(Type),
            {ok, ExprType, new_substitution(), extend_env(Var, ExprType, Env)};
        {variable_ref, RefVar} ->
            case infer_variable_ref(RefVar, Env) of
                {ok, RefType, Sub} ->
                    {ok, RefType, Sub, extend_env(Var, RefType, Env)};
                {error, Error} ->
                    {error, Error}
            end;
        {variable_binding, _RefVar, _RefExpr} ->
            % For nested variable bindings, create a type variable
            TypeVar = {type_var, Var},
            {ok, TypeVar, new_substitution(), extend_env(Var, TypeVar, Env)};
        _ ->
            % For other expressions, create a type variable
            TypeVar = {type_var, Var},
            {ok, TypeVar, new_substitution(), extend_env(Var, TypeVar, Env)}
    end.

%% Infer type for block
infer_block([Expr], Env) ->
    case infer_expression(Expr, Env) of
        {ok, Type, Sub} -> {ok, Type, Sub};
        {ok, Type, Sub, NewEnv} -> {ok, Type, Sub, NewEnv};
        {error, Error} -> {error, Error}
    end;
infer_block([Expr | Rest], Env) ->
    case infer_expression(Expr, Env) of
        {ok, _ExprType, Sub1} ->
            case infer_block(Rest, apply_substitution(Env, Sub1)) of
                {ok, RestType, Sub2} ->
                    {ok, RestType, compose_substitutions(Sub2, Sub1)};
                {error, Error} ->
                    {error, Error}
            end;
        {ok, _ExprType, Sub1, NewEnv} ->
            case infer_block(Rest, apply_substitution(NewEnv, Sub1)) of
                {ok, RestType, Sub2} ->
                    {ok, RestType, compose_substitutions(Sub2, Sub1)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Create new type constant
new_type_const(Name) -> {type_const, Name}.

%% Create new type variable
new_type_var(Name) -> {type_var, Name}.

%% Create new substitution
new_substitution() -> #{}.

%% Apply substitution to type environment
apply_substitution(Env, Sub) ->
    maps:map(fun(_Var, Type) -> apply_substitution_to_type(Type, Sub) end, Env).

%% Apply substitution to type
apply_substitution_to_type({type_var, Name}, Sub) ->
    case maps:find(Name, Sub) of
        {ok, Type} -> Type;
        error -> {type_var, Name}
    end;
apply_substitution_to_type(Type, _Sub) -> Type.

%% Compose substitutions
compose_substitutions(Sub2, Sub1) ->
    maps:merge(Sub2, Sub1).

%% Unify two types
unify(Type1, Type2) ->
    unify(Type1, Type2, new_substitution()).

unify({type_const, Name}, {type_const, Name}, Sub) ->
    {Sub, {type_const, Name}};
unify({type_const, Name1}, {type_const, Name2}, _Sub) when Name1 =/= Name2 ->
    {error, {type_mismatch, {type_const, Name1}, {type_const, Name2}}};
unify({type_var, Name}, Type, Sub) ->
    case maps:find(Name, Sub) of
        {ok, SubType} -> unify(SubType, Type, Sub);
        error ->
            NewSub = maps:put(Name, Type, Sub),
            {NewSub, Type}
    end;
unify(Type, {type_var, Name}, Sub) ->
    unify({type_var, Name}, Type, Sub);
unify(Type1, Type2, Sub) ->
    {Sub, Type1}.

%% Generalize type (convert type variables to polymorphic types)
generalize(Type, Env) ->
    FV = free_variables(Type),
    FVEnv = free_variables_env(Env),
    ForAll = sets:subtract(FV, FVEnv),
    case sets:size(ForAll) of
        0 -> Type;
        _ -> {forall, sets:to_list(ForAll), Type}
    end.

%% Get free variables in type
free_variables({type_const, _}) -> sets:new();
free_variables({type_var, Name}) -> sets:from_list([Name]);
free_variables({type_fun, ParamTypes, ReturnType}) ->
    ParamFV = sets:union([free_variables(PT) || PT <- ParamTypes]),
    ReturnFV = free_variables(ReturnType),
    sets:union(ParamFV, ReturnFV);
free_variables({forall, Vars, Type}) ->
    FV = free_variables(Type),
    sets:subtract(FV, sets:from_list(Vars)).

%% Get free variables in environment
free_variables_env(Env) ->
    maps:fold(fun(_Var, Type, Acc) ->
        sets:union(Acc, free_variables(Type))
    end, sets:new(), Env).

%% Main type inference function
infer_types(AST) ->
    Env = new_type_env(),
    case infer_function_types(AST, Env) of
        {ok, _TypedAST, Specs} ->
            {ok, Specs};
        {error, TypeErrors} ->
            {error, TypeErrors}
    end.

%% Infer types for all functions
infer_function_types([], _Env) ->
    {ok, [], []};
infer_function_types([Fun | Rest], Env) ->
    case infer_function_type(Fun, Env) of
        {ok, TypedFun, Spec} ->
            case infer_function_types(Rest, Env) of
                {ok, TypedRest, Specs} ->
                    {ok, [TypedFun | TypedRest], [Spec | Specs]};
                {error, RestError} ->
                    {error, RestError}
            end;
        {error, FunError} ->
            {error, FunError}
    end.

%% Infer type for single function
infer_function_type({function_def, Name, Params, Body}, Env) ->
    % Create type variables for parameters
    ParamTypes = [new_type_var(Param) || Param <- Params],

    % Extend environment with parameters
    ParamEnv = lists:foldl(fun({Param, Type}, AccEnv) ->
        extend_env(Param, Type, AccEnv)
    end, Env, lists:zip(Params, ParamTypes)),

    % Infer body type
    case infer_block(Body, ParamEnv) of
        {ok, BodyType, Sub} ->
            % Apply substitution to parameter types
            AppliedParamTypes = [apply_substitution_to_type(PT, Sub) || PT <- ParamTypes],

            % Create function type
            FunType = {type_fun, AppliedParamTypes, BodyType},

            % Generalize function type
            GeneralizedType = generalize(FunType, Env),

            % Create spec
            Spec = {Name, length(Params), GeneralizedType},

            {ok, {function_def, Name, Params, Body}, Spec};
        {ok, BodyType, Sub, _NewEnv} ->
            % Apply substitution to parameter types
            AppliedParamTypes = [apply_substitution_to_type(PT, Sub) || PT <- ParamTypes],

            % Create function type
            FunType = {type_fun, AppliedParamTypes, BodyType},

            % Generalize function type
            GeneralizedType = generalize(FunType, Env),

            % Create spec
            Spec = {Name, length(Params), GeneralizedType},

            {ok, {function_def, Name, Params, Body}, Spec};
        {error, BodyError} ->
            {error, BodyError}
    end.