-module(e2_9).

-export([double/1, evens/1, tailevens/1]).

-export([median/1, modes/1]).

-export([test/0]).

double([])    -> [];
double([H|T]) -> [2*H|double(T)].

evens([])                       -> [];
evens([H|T]) when H rem 2 =:= 0 -> [H|evens(T)];
evens([_|T])                    ->    evens(T).

% tailevens/1 is here as example how write tail recursive list processing
% function. Note that length of list reversed using lists:reverse/1 would
% be same as stack depth in body recursive evens/1 because third clause of
% evens/1 is tail recursive so speed difference will be insignificant if
% any.
tailevens(L) ->
    tailevens(L, []).

tailevens([],    Acc)                    -> lists:reverse(Acc);
tailevens([H|T], Acc) when H rem 2 =:= 0 -> tailevens(T, [H|Acc]);
tailevens([_|T], Acc)                    -> tailevens(T,    Acc ).

median([]) -> error(badarg);
median(L)  ->
    case lists:sort(L) of
        SL when length(L) rem 2 =:= 0 -> median2(SL, length(L) div 2);
        SL                            -> median1(SL, length(L) div 2)
    end.

median1([H|_], 0) -> H;
median1([_|T], N) -> median1(T, N-1).

median2([A, B|_], 1) -> (A+B)/2;
median2([_   |T], N) -> median2(T, N-1).

% most efficient modes implementation using maps
modes(L) ->
    H = histogram(L),
    M = max_frequency(H),
    if M < 2 -> []; true -> modes(M, H) end.

histogram(L) ->
    histogram(L, #{}).

histogram([],    M) -> maps:to_list(M);
histogram([H|T], M) -> histogram(T, maps:put(H, maps:get(H, M, 0)+1, M)).

max_frequency(L) ->
    max_frequency(L, 0).

max_frequency([],          M)             -> M;
max_frequency([{_, M}|T], OM) when M > OM -> max_frequency(T, M);
max_frequency([ _    |T],  M)             -> max_frequency(T, M).

modes(_, [])         -> [];
modes(M, [{H, M}|T]) -> [H|modes(M, T)];
modes(M, [ _    |T]) ->    modes(M, T).

% Tests

test() ->
    [2, 4, 8, 6] = double([1, 2, 4, 3]),
    L = lists:seq(1,9),
    R = [2, 4, 6, 8] = evens(L),
    R = tailevens(L),

    3 = median([5,2,1,3,4]),
    3.5 = median([5,2,6,1,3,4]),
    [] = modes(L),
    [6] = modes([1, 3, 6, 6, 6, 6, 7, 7, 12, 12, 17]),
    [1, 4] = lists:sort(modes([1,1,2,4,4])),
    ok.
