-module(myrlang_interpreter).

-export([
    new/1,
    eval/2
]).

%% Env is a map from variable names to values.
%% Primitives is a map from primitive names to functions provided by the
%% user of the interpreter.
-record(intrepreter, {
    env :: map(),
    primitives :: map()
}).

new(Primitives) ->
    #intrepreter{
        env = maps:new(),
        primitives = Primitives
    }.

eval(Interpreter, {integer, _, N}) ->
    {ok, {Interpreter, N}};
eval(Interpreter, {atom, _, X}) ->
    {ok, {Interpreter, X}};
eval(Interpreter, {string, _, S}) ->
    {ok, {Interpreter, S}};
eval(Interpreter, {nil, _}) ->
    {ok, {Interpreter, []}};
eval(Interpreter, {cons, _, Head, Tail}) ->
    make_list(Interpreter, Head, Tail);
eval(Interpreter, {tuple, _, Elements}) ->
    make_tuple(Interpreter, Elements);
eval(Interpreter, {'fun', _, {clauses, [{clause, _, Params, _, Body}]}}) ->
    make_lambda(Interpreter, Params, Body);
eval(Interpreter, {var, _, Name}) ->
    lookup_variable(Interpreter, Name);
eval(Interpreter, {call, _, {atom, _, FunctionName}, Args}) ->
    call_function(Interpreter, FunctionName, Args);
eval(Interpreter, {op, _, OpName, Operand}) ->
    call_function(Interpreter, OpName, [Operand]);
eval(Interpreter, {op, _, OpName, LHS, RHS}) ->
    call_function(Interpreter, OpName, [LHS, RHS]);
eval(Interpreter, {'match', _, {var, _, Name}, RHS}) ->
    case is_allowed_to_bind(RHS) of
        true ->
            bind_variable(Interpreter, Name, RHS);
        false ->
            {error, {illegal_bind, RHS}}
    end;
eval(_, Expr) ->
    {error, {illegal_expression, Expr}}.

make_list(Interpreter, Head, Tail) ->
    make_list(Interpreter, Head, Tail, []).
make_list(Interpreter, Head0, {nil, _}, Acc) ->
    {ok, {_, Head}} = eval(Interpreter, Head0),
    {ok, {Interpreter, lists:reverse([Head | Acc])}};
make_list(Interpreter, Head0, {cons, _, TailHead, TailTail}, Acc) ->
    {ok, {_, Head}} = eval(Interpreter, Head0),
    make_list(Interpreter, TailHead, TailTail, [Head | Acc]).

make_tuple(Interpreter, Elements0) ->
    Elements = eval_list(Interpreter, Elements0),
    {ok, {Interpreter, list_to_tuple(Elements)}}.

make_lambda(Interpreter0, [Param], Body) ->
    Lambda = fun(Arg) ->
        {ok, {Interpreter, _}} = bind_params(Interpreter0, [Param], [Arg]),
        {ok, {_, Value}} = eval_sequence(Interpreter, Body),
        Value
    end,
    {ok, {Interpreter0, Lambda}};
make_lambda(Interpreter0, [Param0, Param1], Body) ->
    Lambda = fun(Arg0, Arg1) ->
        {ok, {Interpreter, _}} = bind_params(Interpreter0, [Param0, Param1], [Arg0, Arg1]),
        {ok, {_, Value}} = eval_sequence(Interpreter, Body),
        Value
    end,
    {ok, {Interpreter0, Lambda}};
make_lambda(Interpreter0, [Param0, Param1, Param2], Body) ->
    Lambda = fun(Arg0, Arg1, Arg2) ->
        {ok, {Interpreter, _}} = bind_params(Interpreter0, [Param0, Param1, Param2], [
            Arg0, Arg1, Arg2
        ]),
        {ok, {_, Value}} = eval_sequence(Interpreter, Body),
        Value
    end,
    {ok, {Interpreter0, Lambda}}.

bind_params(Interpreter, Params, Args) ->
    lists:foldl(
        fun
            ({{var, _, Param}, Arg}, {ok, {InterpreterAcc, _}}) ->
                Env = maps:put(Param, Arg, InterpreterAcc#intrepreter.env),
                {ok, {InterpreterAcc#intrepreter{env = Env}, Arg}};
            (_, {error, Error}) ->
                {error, {bind_failed, Error}};
            (Expr, _) ->
                {error, {wont_bind, Expr}}
        end,
        {ok, {Interpreter, []}},
        lists:zip(Params, Args)
    ).

bind_variable(Interpreter, Name, RHS) ->
    {ok, {_, Value}} = eval(Interpreter, RHS),
    case maps:find(Name, Interpreter#intrepreter.env) of
        {ok, V} when V == Value ->
            {ok, {Interpreter, Value}};
        {ok, _} ->
            {error, {bad_match, Value}};
        error ->
            Env = maps:put(Name, Value, Interpreter#intrepreter.env),
            {ok, {Interpreter#intrepreter{env = Env}, Value}}
    end.

lookup_variable(Interpreter, Name) ->
    case maps:find(Name, Interpreter#intrepreter.env) of
        {ok, Value} ->
            {ok, {Interpreter, Value}};
        error ->
            {error, {undefined_variable, Name}}
    end.

call_function(Interpreter, FunctionName, Params) ->
    case maps:find(FunctionName, Interpreter#intrepreter.primitives) of
        {ok, Fun} ->
            Args = eval_list(Interpreter, Params),
            {ok, {Interpreter, erlang:apply(Fun, Args)}};
        error ->
            {error, {undefined_function, FunctionName}}
    end.

eval_list(Interpreter, Elems) ->
    lists:map(
        fun(E) ->
            {ok, {_, Arg}} = eval(Interpreter, E),
            Arg
        end,
        Elems
    ).

eval_sequence(Interpreter0, [Expr | Rest]) ->
    Acc0 = eval(Interpreter0, Expr),
    lists:foldl(fun(E, Acc) -> eval(Acc, E) end, Acc0, Rest).

is_allowed_to_bind({integer, _, _}) -> true;
is_allowed_to_bind({atom, _, _}) -> true;
is_allowed_to_bind({string, _, _}) -> true;
is_allowed_to_bind({nil, _}) -> true;
is_allowed_to_bind({cons, _, _, _}) -> true;
is_allowed_to_bind({tuple, _, _}) -> true;
is_allowed_to_bind(_) -> false.
