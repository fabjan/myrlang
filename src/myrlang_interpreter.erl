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
eval(Interpreter, {var, _, Name}) ->
    lookup_variable(Interpreter, Name);
eval(Interpreter, {call, _, {atom, _, FunctionName}, Args}) ->
    call_function(Interpreter, FunctionName, Args);
eval(Interpreter, {op, _, OpName, LHS, RHS}) ->
    call_function(Interpreter, OpName, [LHS, RHS]);
eval(Interpreter, {'match', _, {var, _, Name}, RHS}) ->
    bind_variable(Interpreter, Name, RHS);
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

bind_variable(Interpreter, Name, RHS) ->
    {ok, {_, Value}} = eval(Interpreter, RHS),
    Env = maps:put(Name, Value, Interpreter#intrepreter.env),
    {ok, {Interpreter#intrepreter{env = Env}, Value}}.

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
