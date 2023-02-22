-module(myrlang_parser).

-export([
    parse/1
]).

parse(Source) ->
    %% TODO: enable maybe_expr
    case erl_scan:string(Source) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(add_dot_if_missing(Tokens)) of
                {ok, AST} ->
                    case check_ast(AST) of
                        ok -> {ok, AST};
                        {error, Error} -> {error, Error}
                    end;
                _ ->
                    {error, {bad_expression, Tokens}}
            end;
        _ ->
            {error, {bad_expression, Source}}
    end.

add_dot_if_missing(Tokens) ->
    case lists:last(Tokens) of
        {dot, _} ->
            Tokens;
        _ ->
            Tokens ++ [{dot, 0}]
    end.

check_ast([Expr]) ->
    check_expr(Expr);
check_ast(AST) ->
    {error, {too_many_expressions, AST}}.

check_expr(Expr) ->
    case Expr of
        {integer, _, _} ->
            ok;
        {atom, _, _} ->
            ok;
        {var, _, _} ->
            ok;
        {op, _, _, _} ->
            ok;
        {cons, _, _, _} ->
            ok;
        {tuple, _, _} ->
            ok;
        {string, _, _} ->
            ok;
        {call, _, _, _} ->
            check_call(Expr);
        {'fun', _, _} ->
            check_fun(Expr);
        {'match', _, _, _} ->
            check_match(Expr);
        _ ->
            {error, {bad_expression, Expr}}
    end.

check_call({'call', _, {atom, _, _}, Exprs}) ->
    Errors = lists:map(fun check_expr/1, Exprs),
    case
        lists:filter(
            fun
                ({error, _}) -> true;
                (_) -> false
            end,
            Errors
        )
    of
        [] ->
            ok;
        _ ->
            {error, {bad_call, Errors}}
    end;
check_call({'call', _, Function, _}) ->
    {error, {bad_call, Function}}.

check_fun({'fun', _, {clauses, [{clause, _, Params, [], Body}]}}) ->
    ParamsErrors = lists:filtermap(
        fun (Expr) -> case check_expr(Expr) of ok -> false; Error -> {true, Error} end end,
        Params
    ),
    BodyErrors = lists:filtermap(
        fun (Expr) -> case check_expr(Expr) of ok -> false; Error -> {true, Error} end end,
        Body
    ),
    case {ParamsErrors, BodyErrors} of
        {[], []} ->
            ok;
        {_, []} ->
            {error, {bad_fun_params, ParamsErrors}};
        {[], _} ->
            {error, {bad_fun_body, BodyErrors}};
        {_, _} ->
            {error, {bad_fun, ParamsErrors ++ BodyErrors}}
    end;
check_fun({'fun', _, {clauses, [Clause]}}) ->
    {error, {guards_not_allowed, Clause}};
check_fun({'fun', _, {clauses, Clauses}}) ->
    {error, {too_many_clauses, Clauses}};
check_fun(Expr) ->
    {error, {bad_fun, Expr}}.

check_match({'match', _, {var, _, _}, Expr}) ->
    case check_expr(Expr) of
        ok ->
            ok;
        {error, _} ->
            {error, {bad_match_rhs, Expr}}
    end;
check_match({'match', _, LHS, _}) ->
    {error, {bad_match_lhs, LHS}}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_ok_test_() ->
    Cases = #{
        "X" => [{var, 1, 'X'}],
        "foo" => [{atom, 1, foo}],
        "1" => [{integer, 1, 1}],
        "\"foo\"" => [{string, 1, "foo"}],
        "X = 1" => [{'match', 1, {var, 1, 'X'}, {integer, 1, 1}}],
        "foo(bar)" => [{call, 1, {atom, 1, foo}, [{atom, 1, bar}]}],
        "{1, 2}" => [{tuple, 1, [{integer, 1, 1}, {integer, 1, 2}]}],
        "[1]" => [{cons, 1, {integer, 1, 1}, {nil, 1}}],
        "fun() -> 1 end" => [{'fun', 1, {clauses, [{clause, 1, [], [], [{integer, 1, 1}]}]}}],
        "fun() -> foo(1) end" => [{'fun', 1, {clauses, [{clause, 1, [], [], [{call, 1, {atom, 1, foo}, [{integer, 1, 1}]}]}]}}],
        "fun(X) -> X end" => [{'fun', 1, {clauses, [{clause, 1, [{var, 1, 'X'}], [], [{var, 1, 'X'}]}]}}]
    },
    lists:map(
        fun({Source, ExpectedAST}) ->
            ?_assertEqual({ok, ExpectedAST}, parse(Source))
        end,
        maps:to_list(Cases)
    ).

-endif.
