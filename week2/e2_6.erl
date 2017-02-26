-module(e2_6).

-export([sum/1, prod/1, max/1]).
-export([sum_direct/1, prod_direct/1, max_direct/1]).

sum(L) ->
    sum(L, 0).

sum([], S) -> S;
sum([H|T], S) ->
    sum(T, S+H).

prod(L) ->
    prod(L, 1).

prod([], P) -> P;
prod([H|T], P) ->
    prod(T, P*H).


max([H|T]) ->
    max_(T, H).

max_([], M) -> M;
max_([H|T], M) ->
    max_(T, max(H, M)).

% Direct a.k.a body recursive versions

sum_direct([]) -> 0;
sum_direct([H|T]) ->
    H + sum_direct(T).

prod_direct([]) -> 1;
prod_direct([H|T]) ->
    H * prod_direct(T).

max_direct([H]) -> H;
max_direct([H|T]) ->
    max(H, max_direct(T)).
