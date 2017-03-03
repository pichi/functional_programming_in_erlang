-module(e2_13).

-export([nub/1, tail_nub/1]).

nub(L) ->
    nub(L, #{}).

nub([], _) -> [];
nub([H|T], M) ->
    case M of
        #{H:=_} -> nub(T, M);
        _ -> [H|nub(T, M#{H=>[]})]
    end.
% Pre R19 version
%    case maps:is_key(H, M) of
%        true  -> nub(T, M);
%        false -> [H|nub(T, maps:put(H, [], M))]
%    end.

% performance of both seems same in memory and speed
tail_nub(L) ->
    tail_nub(L, #{}, []).

tail_nub([], _, Acc) -> lists:reverse(Acc);
tail_nub([H|T], M, Acc) ->
    case M of
        #{H:=_} -> tail_nub(T, M, Acc);
        _ -> tail_nub(T, M#{H=>[]}, [H|Acc])
    end.
% Pre R19 version
%    case maps:is_key(H, M) of
%        true  -> tail_nub(T, M, Acc);
%        false -> tail_nub(T, maps:put(H, [], M), [H|Acc])
%    end.
