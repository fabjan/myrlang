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
        'not' => fun(X) -> not X end
    }.

show_help() ->
    PrimitiveNames = lists:map(
        fun(Name) -> atom_to_list(Name) end,
        maps:keys(repl_primitives())
    ),
    PrimitiveList = string:join(PrimitiveNames, " "),
    io:format(
        "Available commands:~n"
        ":help - show this help~n"
        ":quit - quit the REPL~n"
        "Available primitives:~n~s~n"
        "~n",
        [PrimitiveList]
    ).

repl() ->
    repl("> ").
repl(Prompt) ->
    io:format("Welcome to myrlang REPL!~n"),
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
            case myrlang_parser:parse(Line) of
                {ok, [Expr]} ->
                    case myrlang_interpreter:eval(Interpreter0, Expr) of
                        {ok, {Interpreter, Value}} ->
                            io:format("~p~n", [Value]),
                            repl(Prompt, Interpreter);
                        {error, Error} ->
                            io:format("Error: ~p~n", [Error]),
                            repl(Prompt, Interpreter0)
                    end;
                {error, Error} ->
                    io:format("Syntax error: ~p~n", [Error]),
                    repl(Prompt, Interpreter0)
            end
    end.
