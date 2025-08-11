-module(lx_types).

-include("lx.hrl").

-export([infer_types/1, check_macro_types/2, unify_types/2, new_type_env/0, register_infix_macro_signature/4]).



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
    case infer_node_types(Node, Env) of
        {ok, TypedNode, Specs} ->
            case infer_ast_types(Rest, Env) of
                {ok, TypedRest, RestSpecs} ->
                    {ok, [TypedNode | TypedRest], Specs ++ RestSpecs};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;
infer_ast_types(SingleNode, Env) ->
    % Handle single node (from parser minimal)
    case infer_node_types(SingleNode, Env) of
        {ok, TypedNode, Specs} ->
            {ok, TypedNode, Specs};
        {error, Error} ->
            {error, Error}
    end.

%% Infer types for a single node
infer_node_types({Type, Meta, Args}, Env) ->
    case Type of
        defmacro ->
            infer_macro_def_types({Type, Meta, Args}, Env);
        macro_call ->
            check_macro_types({Type, Meta, Args}, Env);
        bind ->
            infer_bind_types({Type, Meta, Args}, Env);
        integer ->
            {ok, {Type, Meta#{type => integer}, Args}, []};
        float ->
            {ok, {Type, Meta#{type => float}, Args}, []};
        string ->
            {ok, {Type, Meta#{type => binary}, Args}, []};
        boolean ->
            {ok, {Type, Meta#{type => boolean}, Args}, []};
        atom ->
            {ok, {Type, Meta#{type => atom}, Args}, []};
        nil ->
            {ok, {Type, Meta#{type => nil}, Args}, []};
        ident ->
            case maps:find(Args, maps:get(variables, Env)) of
                {ok, VarType} ->
                    {ok, {Type, Meta#{type => VarType}, Args}, []};
                error ->
                    {ok, {Type, Meta#{type => any}, Args}, []}
            end;
        _ ->
            {ok, {Type, Meta#{type => any}, Args}, []}
    end;
infer_node_types({Type, Line, Col, Value}, Env) ->
    % Handle parser minimal structure {Type, Line, Col, Value}
    case Type of
        integer ->
            {ok, {Type, Line, Col, Value}, []};
        float ->
            {ok, {Type, Line, Col, Value}, []};
        string ->
            {ok, {Type, Line, Col, Value}, []};
        boolean ->
            {ok, {Type, Line, Col, Value}, []};
        atom ->
            {ok, {Type, Line, Col, Value}, []};
        nil ->
            {ok, {Type, Line, Col, Value}, []};
        ident ->
            case maps:find(Value, maps:get(variables, Env)) of
                {ok, VarType} ->
                    {ok, {Type, Line, Col, Value}, []};
                error ->
                    {ok, {Type, Line, Col, Value}, []}
            end;
        _ ->
            {ok, {Type, Line, Col, Value}, []}
    end;
infer_node_types(Other, Env) ->
    % Handle other node types
    {ok, Other, []}.

%% Infer types for macro definitions
infer_macro_def_types({defmacro, Meta, [Name, Args, Body]}, Env) ->
    % Extract type signatures from typed arguments
    {TypedArgs, ArgTypes} = extract_typed_arguments(Args),

    % Infer body types in extended environment
    BodyEnv = Env#{variables := maps:merge(maps:get(variables, Env),
                                         maps:from_list([{Name, Type} || {Name, Type} <- ArgTypes]))},

    case infer_ast_types(Body, BodyEnv) of
        {ok, TypedBody, BodySpecs} ->
            % Assume return type is any for now (could be enhanced)
            ReturnType = any,
            Signature = {Name, ArgTypes, ReturnType},

            % Register macro signature
            NewEnv = register_macro_signature(Name, ArgTypes, ReturnType, Env),

            {ok, {defmacro, Meta#{type => undefined}, [Name, TypedArgs, TypedBody]}, BodySpecs};
        {error, Error} ->
            {error, Error}
    end.

%% Extract typed arguments and their types
extract_typed_arguments(Args) ->
    extract_typed_arguments(Args, [], []).

extract_typed_arguments([], AccArgs, AccTypes) ->
    {lists:reverse(AccArgs), lists:reverse(AccTypes)};
extract_typed_arguments([{typed_ident, Meta, [Name, TypeExpr]} | Rest], AccArgs, AccTypes) ->
    Type = extract_type_from_expr(TypeExpr),
    extract_typed_arguments(Rest,
                          [{ident, Meta#{type => Type}, Name} | AccArgs],
                          [{Name, Type} | AccTypes]);
extract_typed_arguments([{ident, Meta, Name} | Rest], AccArgs, AccTypes) ->
    extract_typed_arguments(Rest,
                          [{ident, Meta#{type => any}, Name} | AccArgs],
                          [{Name, any} | AccTypes]).

%% Extract type from type expression
extract_type_from_expr({type_basic, _Meta, Type}) ->
    Type;
extract_type_from_expr({type_fun, _Meta, [ArgTypes, ReturnType]}) ->
    {'fun', [extract_type_from_expr(T) || T <- ArgTypes], extract_type_from_expr(ReturnType)};
extract_type_from_expr({type_list, _Meta, ElementType}) ->
    {list, extract_type_from_expr(ElementType)};
extract_type_from_expr({type_tuple, _Meta, ElementTypes}) ->
    {tuple, [extract_type_from_expr(T) || T <- ElementTypes]};
extract_type_from_expr({type_var, _Meta, VarName}) ->
    {type_var, VarName};
extract_type_from_expr(_) ->
    any.

%% Infer types for bind expressions
infer_bind_types({bind, Meta, [Var, Expr]}, Env) ->
    case infer_node_types(Expr, Env) of
        {ok, TypedExpr, ExprSpecs} ->
            {_, ExprMeta, _} = TypedExpr,
            ExprType = maps:get(type, ExprMeta),

            % Update environment with variable type
            NewEnv = Env#{variables := maps:put(Var, ExprType, maps:get(variables, Env))},

            {ok, {bind, Meta#{type => ExprType}, [Var, TypedExpr]}, ExprSpecs};
        {error, Error} ->
            {error, Error}
    end.

%% Check macro call types
check_macro_types({macro_call, Meta, [MacroName, Args]}, Env) ->
    case get_macro_signature(MacroName, length(Args), Env) of
        {ok, Signature} ->
            {ArgTypes, ArgAST} = infer_argument_types(Args, Env),
            case unify_macro_signature(Signature, ArgTypes) of
                {ok, ReturnType, Substitution} ->
                    {ok, update_node_type({macro_call, Meta, [MacroName, ArgAST]}, ReturnType)};
                {error, Error} ->
                    {error, {macro_type_error, MacroName, Error}}
            end;
        {error, Error} ->
            {error, {macro_not_found, MacroName, Error}}
    end.

%% Infer types for macro arguments
infer_argument_types(Args, Env) ->
    infer_argument_types(Args, Env, [], []).

infer_argument_types([], _Env, AccTypes, AccAST) ->
    {lists:reverse(AccTypes), lists:reverse(AccAST)};
infer_argument_types([Arg | Rest], Env, AccTypes, AccAST) ->
    case infer_node_types(Arg, Env) of
        {ok, TypedArg, _} ->
            {_, ArgMeta, _} = TypedArg,
            ArgType = maps:get(type, ArgMeta),
            infer_argument_types(Rest, Env, [ArgType | AccTypes], [TypedArg | AccAST]);
        {error, Error} ->
            {error, Error}
    end.

%% Register macro signature when macro is defined
register_macro_signature(MacroName, Args, ReturnType, Env) ->
    #{macros := Macros} = Env,
    NewMacros = maps:update_with(MacroName,
        fun(Signatures) -> [{MacroName, Args, ReturnType} | Signatures] end,
        [{MacroName, Args, ReturnType}],
        Macros),
    Env#{macros := NewMacros}.

%% Register infix macro signature in type environment
register_infix_macro_signature(Op, Args, ReturnType, Env) ->
    InfixMacros = maps:get(infix_macros, Env, #{}),
    NewInfixMacros = maps:update_with(Op,
        fun(Signatures) -> [{Op, Args, ReturnType} | Signatures] end,
        [{Op, Args, ReturnType}],
        InfixMacros),
    Env#{infix_macros := NewInfixMacros}.

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
unify_types({'fun', Args1, Return1}, {'fun', Args2, Return2}) ->
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
unify_types({list, Element1}, {list, Element2}) ->
    unify_types(Element1, Element2);
unify_types({tuple, Elements1}, {tuple, Elements2}) ->
    unify_tuple_elements(Elements1, Elements2);
unify_types(any, _) ->
    {ok, #{}};
unify_types(_, any) ->
    {ok, #{}};
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
        {'fun', Args, Return} ->
            {'fun', apply_substitution(Args, Substitution), apply_substitution(Return, Substitution)};
        {list, Element} ->
            {list, apply_substitution(Element, Substitution)};
        {tuple, Elements} ->
            {tuple, [apply_substitution(E, Substitution) || E <- Elements]};
        _ ->
            Type
    end.

%% Update node type in meta
update_node_type({Type, Meta, Args}, NewType) ->
    {Type, Meta#{type => NewType}, Args}.

%% Update AST types recursively
update_ast_types([]) ->
    [];
update_ast_types([Node | Rest]) ->
    [update_node_type_recursive(Node) | update_ast_types(Rest)];
update_ast_types(SingleNode) ->
    % Handle single node (from parser minimal)
    update_node_type_recursive(SingleNode).

update_node_type_recursive({Type, Meta, Args}) when is_list(Args) ->
    {Type, Meta, [update_node_type_recursive(Arg) || Arg <- Args]};
update_node_type_recursive({Type, Meta, Args}) ->
    {Type, Meta, Args};
update_node_type_recursive({Type, Line, Col, Value}) ->
    % Handle parser minimal structure {Type, Line, Col, Value}
    {Type, Line, Col, Value};
update_node_type_recursive(Other) ->
    % Handle non-AST nodes (atoms, integers, etc.)
    Other.

%% Create new type environment (no built-in signatures)
new_type_env() ->
    #{
        variables => #{},
        macros => #{}
    }.