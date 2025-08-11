-module(lx2_types).

-export([infer_types/1, check_macro_types/2, unify_types/2, register_macro_signature/4]).

-include("lx2.hrl").

%% Type environment with macro definitions
-type type_env() :: #{
    variables => #{atom() => type()},
    macros => #{atom() => [macro_signature()]}
}.

-type macro_signature() :: {
    macro_name(),
    [typed_argument()],
    type()
}.

-type typed_argument() :: {
    argument_name(),
    type()
}.

-type argument_name() :: atom().

%% Infer types for AST with macro support
infer_types(AST) ->
    Env = new_type_env(),
    case infer_ast_types(AST, Env) of
        {ok, TypedAST, Specs} ->
            {ok, update_ast_types(TypedAST), Specs};
        {error, Errors} ->
            {error, Errors}
    end.

%% Infer types for AST nodes
infer_ast_types([], _Env) ->
    {ok, [], []};
infer_ast_types([Node | Rest], Env) ->
    case infer_node_type(Node, Env) of
        {ok, TypedNode, Specs1} ->
            case infer_ast_types(Rest, Env) of
                {ok, TypedRest, Specs2} ->
                    {ok, [TypedNode | TypedRest], Specs1 ++ Specs2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Infer type for a single node
infer_node_type({Type, Meta, Args}, Env) ->
    case Type of
        % Primitives
        defmacro ->
            infer_macro_def_type({Type, Meta, Args}, Env);
        '__block__' ->
            infer_block_type({Type, Meta, Args}, Env);

        % Erlang literals
        integer ->
            {ok, {Type, Meta#{type => integer}, Args}, []};
        float ->
            {ok, {Type, Meta#{type => float}, Args}, []};
        string ->
            {ok, {Type, Meta#{type => binary}, Args}, []};
        atom ->
            {ok, {Type, Meta#{type => atom}, Args}, []};
        boolean ->
            {ok, {Type, Meta#{type => boolean}, Args}, Args};
        nil ->
            {ok, {Type, Meta#{type => nil}, Args}, []};
        underscore ->
            {ok, {Type, Meta#{type => any}, Args}, []};

        % Macro calls
        macro_call ->
            infer_macro_call_type({Type, Meta, Args}, Env);

        % Identifiers
        ident ->
            {ok, {Type, Meta#{type => any}, Args}, []};

        % Bindings
        bind ->
            infer_binding_type({Type, Meta, Args}, Env);

        % Default case
        _ ->
            {ok, {Type, Meta#{type => any}, Args}, []}
    end.

%% Infer macro definition type
infer_macro_def_type({defmacro, Meta, [Name, Args, Body]}, Env) ->
    % Register macro signature
    case infer_macro_signature(Name, Args, Body, Env) of
        {ok, _Signature, NewEnv} ->
            % Infer body types
            case infer_ast_types(Body, NewEnv) of
                {ok, TypedBody, Specs} ->
                    TypedNode = {defmacro, Meta#{type => undefined}, [Name, Args, TypedBody]},
                    {ok, TypedNode, Specs};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

%% Infer macro signature
infer_macro_signature(Name, Args, Body, Env) ->
    % Extract argument types from typed identifiers
    ArgTypes = extract_arg_types(Args),

    % Infer return type from body
    case infer_ast_types(Body, Env) of
        {ok, TypedBody, _} ->
            ReturnType = infer_return_type(TypedBody),
            Signature = {Name, ArgTypes, ReturnType},
            NewEnv = register_macro_signature(Name, ArgTypes, ReturnType, Env),
            {ok, Signature, NewEnv};
        {error, Error} ->
            {error, Error}
    end.

%% Extract argument types from typed identifiers
extract_arg_types(Args) ->
    [extract_arg_type(Arg) || Arg <- Args].

extract_arg_type({ident, _Meta, Name}) ->
    {Name, any};
extract_arg_type({typed_ident, _Meta, [Name, Type]}) ->
    {Name, Type}.

%% Infer return type from body
infer_return_type([]) ->
    any;
infer_return_type([LastNode | _]) ->
    {_Type, Meta, _Args} = LastNode,
    maps:get(type, Meta, any).

%% Infer block type
infer_block_type({'__block__', Meta, Body}, Env) ->
    case infer_ast_types(Body, Env) of
        {ok, TypedBody, Specs} ->
            ReturnType = infer_return_type(TypedBody),
            {ok, {'__block__', Meta#{type => ReturnType}, TypedBody}, Specs};
        {error, Error} ->
            {error, Error}
    end.

%% Check macro call types
check_macro_types({macro_call, Meta, [MacroName, Args]}, Env) ->
    case get_macro_signature(MacroName, length(Args), Env) of
        {ok, Signature} ->
            {ArgTypes, ArgAST} = infer_argument_types(Args, Env),
            case unify_macro_signature(Signature, ArgTypes) of
                {ok, ReturnType, _Substitution} ->
                    {ok, update_node_type({macro_call, Meta, [MacroName, ArgAST]}, ReturnType)};
                {error, Error} ->
                    {error, {macro_type_error, MacroName, Error}}
            end;
        {error, Error} ->
            {error, {macro_not_found, MacroName, Error}}
    end.

%% Infer macro call type
infer_macro_call_type({macro_call, Meta, [MacroName, Args]}, Env) ->
    case check_macro_types({macro_call, Meta, [MacroName, Args]}, Env) of
        {ok, TypedNode} ->
            {ok, TypedNode, []};
        {error, Error} ->
            {error, Error}
    end.

%% Infer binding type
infer_binding_type({bind, Meta, [Ident, Expr]}, Env) ->
    case infer_node_type(Expr, Env) of
        {ok, TypedExpr, Specs} ->
            {_Type, ExprMeta, _Args} = TypedExpr,
            ExprType = maps:get(type, ExprMeta, any),
            TypedNode = {bind, Meta#{type => ExprType}, [Ident, TypedExpr]},
            {ok, TypedNode, Specs};
        {error, Error} ->
            {error, Error}
    end.

%% Infer argument types
infer_argument_types(Args, Env) ->
    {ArgTypes, ArgAST} = lists:foldl(
        fun(Arg, {Types, AST}) ->
            case infer_node_type(Arg, Env) of
                {ok, TypedArg, _} ->
                    {_Type, Meta, _Args} = TypedArg,
                    Type = maps:get(type, Meta, any),
                    {[Type | Types], [TypedArg | AST]};
                {error, Error} ->
                    throw(Error)
            end
        end,
        {[], []},
        Args
    ),
    {lists:reverse(ArgTypes), lists:reverse(ArgAST)}.

%% Register macro signature when macro is defined
register_macro_signature(MacroName, Args, ReturnType, Env) ->
    #{macros := Macros} = Env,
    NewMacros = maps:update_with(MacroName,
        fun(Signatures) -> [{MacroName, Args, ReturnType} | Signatures] end,
        [{MacroName, Args, ReturnType}],
        Macros),
    Env#{macros := NewMacros}.

%% Get macro signature from environment
get_macro_signature(MacroName, Arity, #{macros := Macros}) ->
    case maps:find(MacroName, Macros) of
        {ok, Signatures} ->
            case find_signature_by_arity(Signatures, Arity) of
                {ok, Signature} -> {ok, Signature};
                error -> {error, {arity_mismatch, MacroName, Arity}}
            end;
        error ->
            {error, {macro_not_found, MacroName}}
    end.

%% Find signature by arity
find_signature_by_arity([{Name, Args, ReturnType} | _], Arity) when length(Args) =:= Arity ->
    {ok, {Name, Args, ReturnType}};
find_signature_by_arity([_ | Rest], Arity) ->
    find_signature_by_arity(Rest, Arity);
find_signature_by_arity([], _Arity) ->
    error.

%% Unify macro signature with argument types
unify_macro_signature({_Name, ExpectedArgs, ReturnType}, ActualArgTypes) ->
    case unify_argument_lists(ExpectedArgs, ActualArgTypes) of
        {ok, Substitution} ->
            {ok, apply_substitution(ReturnType, Substitution), Substitution};
        {error, Error} ->
            {error, Error}
    end.

%% Unify argument lists
unify_argument_lists([], []) ->
    {ok, #{}};
unify_argument_lists([{_Name, ExpectedType} | ExpectedRest], [ActualType | ActualRest]) ->
    case unify_types(ExpectedType, ActualType) of
        {ok, Substitution} ->
            case unify_argument_lists(ExpectedRest, ActualRest) of
                {ok, RestSubstitution} ->
                    {ok, maps:merge(Substitution, RestSubstitution)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_argument_lists(_, _) ->
    {error, argument_count_mismatch}.

%% Unify two types
unify_types(Type1, Type2) when Type1 =:= Type2 ->
    {ok, #{}};
unify_types({type_var, Name}, Type) ->
    {ok, #{Name => Type}};
unify_types(Type, {type_var, Name}) ->
    {ok, #{Name => Type}};
unify_types({type_fun, Args1, Return1}, {type_fun, Args2, Return2}) ->
    case unify_types(Args1, Args2) of
        {ok, Sub1} ->
            case unify_types(Return1, Return2) of
                {ok, Sub2} ->
                    {ok, maps:merge(Sub1, Sub2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_types({type_list, Element1}, {type_list, Element2}) ->
    unify_types(Element1, Element2);
unify_types({type_tuple, Elements1}, {type_tuple, Elements2}) ->
    unify_tuple_elements(Elements1, Elements2);
unify_types(_, _) ->
    {error, type_mismatch}.

%% Unify tuple elements
unify_tuple_elements([], []) ->
    {ok, #{}};
unify_tuple_elements([E1 | Rest1], [E2 | Rest2]) ->
    case unify_types(E1, E2) of
        {ok, Sub1} ->
            case unify_tuple_elements(Rest1, Rest2) of
                {ok, Sub2} ->
                    {ok, maps:merge(Sub1, Sub2)};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
unify_tuple_elements(_, _) ->
    {error, tuple_size_mismatch}.

%% Apply substitution to type
apply_substitution(Type, Substitution) ->
    case Type of
        {type_var, Name} ->
            maps:get(Name, Substitution, Type);
        {type_fun, Args, Return} ->
            {type_fun, apply_substitution(Args, Substitution), apply_substitution(Return, Substitution)};
        {type_list, Element} ->
            {type_list, apply_substitution(Element, Substitution)};
        {type_tuple, Elements} ->
            {type_tuple, [apply_substitution(E, Substitution) || E <- Elements]};
        _ ->
            Type
    end.

%% Update AST node type
update_node_type({Type, Meta, Args}, NewType) ->
    {Type, Meta#{type => NewType}, Args}.

%% Update AST types recursively
update_ast_types([]) ->
    [];
update_ast_types([Node | Rest]) ->
    [update_ast_types(Node) | update_ast_types(Rest)];
update_ast_types({Type, Meta, Args}) when is_list(Args) ->
    {Type, Meta, [update_ast_types(Arg) || Arg <- Args]};
update_ast_types({Type, Meta, Args}) ->
    {Type, Meta, Args}.

%% Create new type environment (no built-in signatures)
new_type_env() ->
    #{
        variables => #{},
        macros => #{}
    }.