-module(lx_ast_transformer).
-export([transform/1, transform_expression/1]).

%% Transform Lx AST to Core Erlang AST
transform(AST) ->
    case AST of
        [] ->
            {ok, []};
        [SingleExpr] ->
            {ok, [transform_expression(SingleExpr)]};
        MultipleExprs ->
            {ok, [transform_expression(E) || E <- MultipleExprs]}
    end.

%% Transform single expression to Core Erlang
transform_expression(Expr) ->
    case Expr of
        {integer, Line, Value} ->
            {integer, Line, Value};
        {float, Line, Value} ->
            {float, Line, Value};
        {atom, Line, Value} ->
            {atom, Line, Value};
        {string, Line, Value} ->
            {string, Line, Value};
        {ident, Line, Value} ->
            {var, Line, list_to_atom(Value)};
        {list, Line, Elements} ->
            {cons, Line,
             transform_expression(hd(Elements)),
             transform_list_tail(tl(Elements))};
        {tuple, Line, Elements} ->
            {tuple, Line, [transform_expression(E) || E <- Elements]};
        {map, Line, Entries} ->
            {map, Line, [], [transform_map_entry(E) || E <- Entries]};
        {map_entry, Line, [Key, Value]} ->
            {map_field_exact, Line, transform_expression(Key), transform_expression(Value)};
        {map_entry, Line, Key, Value} ->
            {map_field_exact, Line, transform_expression(Key), transform_expression(Value)};
        {case_, Line, Args} when is_list(Args) ->
            io:format("DEBUG: Processing case with Args: ~p~n", [Args]),
            % Handle case with different structure
            case Args of
                [Expr1, Clauses1] ->
                    Result = {'case', Line,
                     transform_expression(Expr1),
                     extract_case_clauses(Clauses1)},
                    io:format("DEBUG: Case result: ~p~n", [Result]),
                    Result;
                _ ->
                    {error, {invalid_case_structure, Line, Args}}
            end;
        {clause, Line, [Pattern, Body]} ->
            % Transform clause
            {clause, Line,
             [transform_expression(Pattern)],
             [],
             [transform_expression(Body)]};
        {else_, Line, Expressions} ->
            % Transform else clause
            {clause, Line,
             [{var, Line, '_'}],
             [],
             [transform_expression_list(Expressions)]};
        {mfa, Line, [Module, Function, Args]} ->
            % Handle module:function calls
            {call, Line, {remote, Line, transform_expression(Module), transform_expression(Function)},
             [transform_expression(Arg) || Arg <- Args]};
        % Handle operators (as strings)
        {Operator, Line, [Left, Right]} when is_list(Operator) ->
            % Convert operator string to function call
            {call, Line, {remote, Line, {atom, Line, erlang}, {atom, Line, list_to_atom(Operator)}},
             [transform_expression(Left), transform_expression(Right)]};
        _ ->
            % Fallback for unsupported expressions
            {call, 0,
             {remote, 0, {atom, 0, io}, {atom, 0, format}},
             [{string, 0, "Unsupported expression"},
              {cons, 0, {term, 0, Expr}, {nil, 0}}]}
    end.

%% Transform list tail
transform_list_tail([]) ->
    {nil, 0};
transform_list_tail([Element | Rest]) ->
    {cons, 0,
     transform_expression(Element),
     transform_list_tail(Rest)}.

%% Transform expression list to Core Erlang
transform_expression_list(Exprs) ->
    case Exprs of
        [] ->
            {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, ok}}, []};
        [Expr] ->
            transform_expression(Expr);
        [Expr | Rest] ->
            % Create a sequence of expressions
            {call, 0, {remote, 0, {atom, 0, erlang}, {atom, 0, ';'}},
             [transform_expression(Expr), transform_expression_list(Rest)]}
    end.



%% Extract and flatten case clauses from nested structure
extract_case_clauses(Clauses) ->
    io:format("DEBUG: extract_case_clauses called with: ~p~n", [Clauses]),
    case Clauses of
        [] -> [];
        {block, _Line, BlockClauses} ->
            % Handle block structure
            extract_case_clauses(BlockClauses);
        [Clause | Rest] ->
            case Clause of
                {clause, Line, [Pattern, Body]} ->
                    % Check if Body contains nested clauses
                    case Body of
                        [NestedClause | _] when is_tuple(NestedClause), element(1, NestedClause) =:= clause ->
                            % This is a nested clause structure, extract all clauses
                            [create_case_clause(Line, Pattern, []) | extract_nested_clauses(Body)];
                        _ ->
                            % Normal clause - Body is a list of expressions
                            [create_case_clause(Line, Pattern, Body) | extract_case_clauses(Rest)]
                    end;
                _ ->
                    [transform_case_clause(Clause) | extract_case_clauses(Rest)]
            end
    end.

%% Extract nested clauses from body
extract_nested_clauses(Clauses) ->
    case Clauses of
        [] -> [];
        [Clause | Rest] ->
            case Clause of
                {clause, Line, [Pattern, Body]} ->
                    [create_case_clause(Line, Pattern, Body) | extract_nested_clauses(Rest)];
                _ ->
                    [transform_case_clause(Clause) | extract_nested_clauses(Rest)]
            end
    end.

%% Create a proper case clause
create_case_clause(Line, Pattern, Body) ->
    io:format("DEBUG: create_case_clause called with Pattern: ~p, Body: ~p~n", [Pattern, Body]),
    % Extract only the first expression from Body (ignore nested clauses)
    case Body of
        [FirstExpr | _] when is_tuple(FirstExpr), element(1, FirstExpr) =/= clause ->
            {clause, Line,
             [transform_expression(Pattern)],
             [],
             [transform_expression(FirstExpr)]};
        _ ->
            {clause, Line,
             [transform_expression(Pattern)],
             [],
             [{atom, Line, ok}]}
    end.



%% Transform single case clause
transform_case_clause({clause, Line, [Pattern, Body]}) ->
    io:format("DEBUG: transform_case_clause called with: ~p~n", [{clause, Line, [Pattern, Body]}]),
    % Handle nested body structure - Body is a list of expressions
    {clause, Line,
     [transform_expression(Pattern)],
     [],
     [transform_expression_list(Body)]};
transform_case_clause({else_, Line, Expressions}) ->
    {clause, Line,
     [{var, Line, '_'}],
     [],
     [transform_expression_list(Expressions)]};
transform_case_clause(Clause) ->
    % Fallback for other clause types
    Clause.

%% Transform map entry
transform_map_entry({map_entry, Line, [Key, Value]}) ->
    {map_field_exact, Line, transform_expression(Key), transform_expression(Value)};
transform_map_entry({map_entry, Line, Key, Value}) ->
    {map_field_exact, Line, transform_expression(Key), transform_expression(Value)}.