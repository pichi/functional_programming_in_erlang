-module(e3_11).

-export([compose/1, twice/2, iterate/1]).

-export([test/0]).

inc() -> fun inc/1.

inc(X) -> X+1.

add(N) ->
    (iterate(N))(inc()).

twice(F, X) ->
    ((iterate(2))(F))(X).

iterate(N) ->
    fun(F) -> compose(lists:duplicate(N, F)) end.

compose(L) ->
    lists:foldl(fun compose/2, identity(), L).

compose(F, G) ->
    fun(X) -> F(G(X)) end.

identity(X) -> X.

identity() -> fun identity/1.

test() ->
    F = add(1),
    G = fun(X) -> X*2 end,
    2 = (compose([F, G]))(0),
    1 = (compose([G, F]))(0),
    M3 = fun(X) -> X*3 end,
    18 = twice(M3, 2),
    ok.
