-module(e2_13).

-export([nub/1, tail_nub/1]).

-export([nub_nomap/1, nub_simple/1, bun/1]).

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

% O(N*logM) version without using map (M is number of unique values)
nub_nomap(L) ->
    [X ||
     {_, X} <- lists:sort(
                 lists:ukeysort(
                   2, lists:zip(
                        lists:seq(1, length(L)),
                        L)))].

% O(N*M)
nub_simple(L) ->
    nub_simple(L, []).

nub_simple([], Acc) -> lists:reverse(Acc);
nub_simple([H|T], Acc) ->
    nub_simple(T, case lists:member(H, Acc) of
                      true  -> Acc;
                      false -> [H|Acc]
                  end).

% O(N^2) super simple

bun([]) -> [];
bun([H|T]) ->
    case lists:member(H, T) of
        true  -> bun(T);
        false -> [H|bun(T)]
    end.
