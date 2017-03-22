-module(e3_5).

-export([doubleAll/1, evens/1, product/1]).

-export([zip/2, zip_with/3]).

-export([zip2/2, zip_with2/3]).

-export([test/0]).

doubleAll(L) ->
    lists:map(fun double/1, L).

double(X) -> 2*X.

evens(L) ->
    lists:filter(fun even/1, L).

even(X) ->
    X rem 2 =:= 0.

product(L) ->
    lists:foldl(fun erlang:'*'/2, 1, L).

zip([H1|T1], [H2|T2]) ->
    [{H1, H2} | zip(T1, T2)];
zip([], _) -> [];
zip(_, []) -> [].

zip_with(F, [H1|T1], [H2|T2]) ->
    [F(H1, H2) | zip_with(F, T1, T2)];
zip_with(F, L1, L2)
  when is_function(F, 2), L1 =:= [] orelse L2 =:= [] ->
    [].

zip2(L1, L2) ->
    zip_with(fun zipper/2, L1, L2).

zipper(X, Y) -> {X, Y}.

zip_with2(F, L1, L2) ->
    G = fun({X, Y}) -> F(X, Y) end,
    lists:map(G, zip(L1, L2)).

test() ->
    [2, 4, 8, 6] = doubleAll([1, 2, 4, 3]),
    L = lists:seq(1,9),
    [2, 4, 6, 8] = evens(L),
    362880 = product(L),

    [ {1,2}, {3,4} ] = zip([1,3,5,7], [2,4]),
    [ 3, 7 ] = zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),

    [ {1,2}, {3,4} ] = zip2([1,3,5,7], [2,4]),
    [ 3, 7 ] = zip_with2(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]),

    ok.
