-module(myrlang).

-export([
    repl/0,
    repl/1
]).

repl() ->
    repl("> ").
repl(Prompt) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        ":q\n" ->
            ok;
        Line ->
            case myrlang_parser:parse(Line) of
                {ok, AST} ->
                    io:format("~p~n", [AST]);
                {error, {bad_expression, Expr}} ->
                    io:format("Bad expression: ~p~n", [Expr])
            end,
            repl(Prompt)
    end.
