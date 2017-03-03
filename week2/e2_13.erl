-module(e2_13).

-export([nub/1, tail_nub/1]).

% M#{H=>[]} may not work in older releases than R19
% use maps:put(H, [], M) instead
nub(L) ->
    nub(L, #{}).

nub([], _) -> [];
nub([H|T], M) ->
    case M of
        #{H:=_} -> nub(T, M);
        _ -> [H|nub(T, M#{H=>[]})]
    end.

% performance of both seems same in memory and speed
tail_nub(L) ->
    tail_nub(L, #{}, []).

tail_nub([], _, Acc) -> lists:reverse(Acc);
tail_nub([H|T], M, Acc) ->
    case M of
        #{H:=_} -> tail_nub(T, M, Acc);
        _ -> tail_nub(T, M#{H=>[]}, [H|Acc])
    end.
