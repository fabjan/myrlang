-module(myrlang_scanner).

-export([
    tokens/1
]).

tokens(Source) ->
    tokens(Source, []).

tokens("", Acc) ->
    lists:reverse(Acc);
tokens(Source, Acc) ->
    case maybe_scan_special(Source) of
        {token, Token, Rest} ->
            tokens(Rest, [Token | Acc]);
        {char, Start, _} ->
            case Start of
                %% skip whitespace
                C when C =:= 10 orelse C =:= 13 orelse C =:= 32 ->
                    tokens(tl(Source), Acc);
                %% numbers start with digits
                C when $0 =< C andalso C =< $9 ->
                    {NumberToken, Rest} = scan_integer(Source),
                    tokens(Rest, [NumberToken | Acc]);
                %% variables start with capital letters
                C when $A =< C andalso C =< $Z ->
                    {VariableToken, Rest} = scan_variable(Source),
                    tokens(Rest, [VariableToken | Acc]);
                %% symbols start with lowercase letters
                C when $a =< C andalso C =< $z ->
                    {SymbolToken, Rest} = scan_symbol(Source),
                    tokens(Rest, [SymbolToken | Acc]);
                _ ->
                    tokens(tl(Source), Acc)
            end
    end.

%% NOTE: longest first!
maybe_scan_special("end" ++ Rest) -> {token, 'end', Rest};
maybe_scan_special("fun" ++ Rest) -> {token, 'fun', Rest};
maybe_scan_special("->" ++ Rest) -> {token, arrow, Rest};
maybe_scan_special("=>" ++ Rest) -> {token, map_arrow, Rest};
maybe_scan_special("#{" ++ Rest) -> {token, open_map, Rest};
maybe_scan_special("}" ++ Rest) -> {token, close_brace, Rest};
maybe_scan_special("[" ++ Rest) -> {token, open_bracket, Rest};
maybe_scan_special("]" ++ Rest) -> {token, close_bracket, Rest};
maybe_scan_special("(" ++ Rest) -> {token, open_paren, Rest};
maybe_scan_special(")" ++ Rest) -> {token, close_paren, Rest};
maybe_scan_special("," ++ Rest) -> {token, comma, Rest};
maybe_scan_special("+" ++ Rest) -> {token, {op, plus}, Rest};
maybe_scan_special("-" ++ Rest) -> {token, {op, minus}, Rest};
maybe_scan_special("*" ++ Rest) -> {token, {op, times}, Rest};
maybe_scan_special("/" ++ Rest) -> {token, {op, solidus}, Rest};
maybe_scan_special([C | Rest]) -> {char, C, Rest}.

scan_integer(Source) ->
    {Digits, Rest} = lists:splitwith(fun(C) -> $0 =< C andalso C =< $9 end, Source),
    NumberToken = {literal, list_to_integer(Digits)},
    {NumberToken, Rest}.

scan_symbol(Source) ->
    {SymbolChars, Rest} = lists:splitwith(fun(C) -> $a =< C andalso C =< $z end, Source),
    SymbolToken = {literal, list_to_atom(SymbolChars)},
    {SymbolToken, Rest}.

scan_variable(Source) ->
    {VariableChars, Rest} = lists:splitwith(
        fun(C) -> $A =< C andalso C =< $Z orelse $a =< C andalso C =< $z end, Source
    ),
    VariableToken = {variable, VariableChars},
    {VariableToken, Rest}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokens_test_() ->
    Cases = #{
        "1" => [{literal, 1}],
        "1 + 2" => [{literal, 1}, {op, plus}, {literal, 2}],
        "1 + 2 * 3" => [{literal, 1}, {op, plus}, {literal, 2}, {op, times}, {literal, 3}],
        "a" => [{literal, a}],
        "a b" => [{literal, a}, {literal, b}],
        "a(b, c)" => [{literal, a}, open_paren, {literal, b}, comma, {literal, c}, close_paren],
        "[a, b, c]" => [open_bracket, {literal, a}, comma, {literal, b}, comma, {literal, c}, close_bracket],
        "#{a => b, c => d}" => [open_map, {literal, a}, map_arrow, {literal, b}, comma, {literal, c}, map_arrow, {literal, d}, close_brace],
        "fun (X) -> X + 1 end" => [
            'fun',
            open_paren,
            {variable, "X"},
            close_paren,
            arrow,
            {variable, "X"},
            {op, plus},
            {literal, 1},
            'end'
        ]
    },
    lists:map(
        fun({Source, Expected}) ->
            ?_assertEqual(Expected, tokens(Source))
        end,
        maps:to_list(Cases)
    ).

-endif.
