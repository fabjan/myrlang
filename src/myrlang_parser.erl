-module(myrlang_parser).

-export([
    parse/1
]).

parse(Tokens) ->
    parse_expression(Tokens).

parse_expression(_) -> {error, todo}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    %% parse tokens into an expression
    Cases = #{
        [{literal, 4711}] => {literal, 4711},
        [{literal, foo}] => {literal, foo}
    },
    lists:map(
        fun({Tokens, Expected}) ->
            ?_assertEqual(Expected, parse(Tokens))
        end,
        maps:to_list(Cases)
    ).

-endif.
