-module(myrlang).

-export([
    main/1,
    repl/0,
    repl/1
]).

repl_primitives() ->
    #{
        '+' => fun(X, Y) -> X + Y end,
        '-' => fun(X, Y) -> X - Y end,
        '*' => fun(X, Y) -> X * Y end,
        'div' => fun(X, Y) -> X div Y end,
        'rem' => fun(X, Y) -> X rem Y end,
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

main([]) ->
    case repl() of
        ok ->
            erlang:halt(0);
        _ ->
            erlang:halt(1, [{flush, true}])
    end.

repl() ->
    repl("> ").
repl(Prompt) ->
    io:format("~nWelcome to Myrlang!~n~n"),
    show_help(),
    repl(Prompt, myrlang_interpreter:new(repl_primitives())).
repl(Prompt, Interpreter0) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        eof ->
            io:format("~n"),
            ok;
        ":quit\n" ->
            io:format("~n"),
            ok;
        ":help\n" ->
            show_help(),
            repl(Prompt, Interpreter0);
        X when not is_list(X) ->
            io:format("Unexpected input: ~p~n", [X]),
            error;
        Line ->
            case catch read_eval(Line, Interpreter0) of
                {_, {Reason, [{M, F, A, _} | _]}} ->
                    ArgStrings = lists:map(fun(X) -> io_lib:format("~p", [X]) end, A),
                    Args = string:join(ArgStrings, ", "),
                    io:format("Exception in ~p:~p(~s): ~p~n", [M, F, Args, Reason]),
                    repl(Prompt, Interpreter0);
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
        "1 + 2 * 3 - 4" => 3,
        "7 == 7" => true,
        "7 < 7" => false,
        "4711 div 42" => 112,
        "4711 rem 42" => 7,
        "not not false" => false,
        "not (1 + 2 * 3 < 7)" => true,
        "map(fun(X) -> not (X rem 2 == 1) end, [1, 2, 3])" => [false, true, false],
        "map(fun(X) -> X + 1 end, [1, 2, 3])" => [2, 3, 4]
    },
    lists:map(
        fun({Source, Expected}) ->
            case read_eval(Source, Interpreter) of
                {ok, {_, Actual}} -> ?_assertEqual(Expected, Actual);
                _ -> throw({expected_ok, Source})
            end
        end,
        maps:to_list(Cases)
    ).

-endif.
