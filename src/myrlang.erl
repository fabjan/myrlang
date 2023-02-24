-module(myrlang).

-export([
    repl/0,
    repl/1
]).

repl_primitives() ->
    #{
        '+' => fun(X, Y) -> X + Y end,
        '-' => fun(X, Y) -> X - Y end,
        '*' => fun(X, Y) -> X * Y end,
        'div' => fun(X, Y) -> X div Y end,
        '==' => fun(X, Y) -> X == Y end,
        '<' => fun(X, Y) -> X < Y end,
        'not' => fun(X) -> not X end,
        map => fun(F, List) -> lists:map(F, List) end
    }.

show_help() ->
    PrimitiveNames = lists:map(
        fun(Name) -> atom_to_list(Name) end,
        maps:keys(repl_primitives())
    ),
    PrimitiveList = string:join(PrimitiveNames, " "),
    io:format(
        "Shell commands:~n"
        "    :help - show this help~n"
        "    :quit - quit the REPL~n"
        "~n"
        "Environment primitives:~n"
        "    ~s~n"
        "~n",
        [PrimitiveList]
    ).

repl() ->
    repl("> ").
repl(Prompt) ->
    io:format("~nWelcome to Myrlang!~n~n"),
    show_help(),
    repl(Prompt, myrlang_interpreter:new(repl_primitives())).
repl(Prompt, Interpreter0) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        ":quit\n" ->
            ok;
        ":help\n" ->
            show_help(),
            repl(Prompt, Interpreter0);
        Line ->
            case read_eval(Line, Interpreter0) of
                {error, {syntax_error, Error}} ->
                    io:format("Syntax error: ~p~n", [Error]),
                    repl(Prompt, Interpreter0);
                {error, {runtime_error, Error}} ->
                    io:format("Runtime error: ~p~n", [Error]),
                    repl(Prompt, Interpreter0);
                {ok, {Interpreter1, Value}} ->
                    io:format("~s~n", [show(Value)]),
                    repl(Prompt, Interpreter1)
            end
    end.

read_eval(Line, Interpreter0) ->
    case myrlang_parser:parse(Line) of
        {error, Error} ->
            {error, {syntax_error, Error}};
        {ok, [Expr]} ->
            case myrlang_interpreter:eval(Interpreter0, Expr) of
                {error, Error} ->
                    {error, {runtime_error, Error}};
                Result ->
                    Result
            end
    end.

show(Value) ->
    %% placeholder implementation, we will hide more details later
    io_lib:format("~p", [Value]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_eval_test_() ->
    Interpreter = myrlang_interpreter:new(repl_primitives()),
    Cases = #{
        "1 + 2" => 3,
        "1 + 2 * 3" => 7,
        "1 + 2 * 3 == 7" => true,
        "1 + 2 * 3 == 8" => false,
        "1 + 2 * 3 < 8" => true,
        "1 + 2 * 3 < 7" => false,
        "map(fun(X) -> X + 1 end, [1, 2, 3])" => [2, 3, 4]
    },
    lists:map(
        fun({Source, Expected}) ->
            Value = read_eval(Source, Interpreter),
            ?_assertMatch({ok, {_, Actual}}, Value),
            {ok, {_, Actual}} = Value,
            ?_assertEqual(Expected, Actual)
        end,
        maps:to_list(Cases)
    ).

-endif.
