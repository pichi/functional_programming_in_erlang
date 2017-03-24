-module(rps).

-export([possibilities/0, beats/1]).

-export([result/2]).

-export([tournament/2]).

-export([choose/0, choose/1, freq/1]).

possibilities() -> [rock, paper, scissors].

beats(rock)     -> scissors;
beats(paper)    -> rock;
beats(scissors) -> paper.

result(X, Y) ->
    case {beats(X), beats(Y)} of
        {Y, _} -> win;
        {_, X} -> lose;
        _      -> draw
    end.

points(X, Y) ->
    result2points(result(X, Y)).

result2points(win)  -> 1;
result2points(draw) -> 0;
result2points(lose) -> -1.

tournament(L1, L2) ->
    lists:sum(lists:zipwith(fun points/2, L1, L2)).

choose() ->
    choose([1, 1, 1]).

choose({R, P, C}) -> choose([R, P, C]);
choose([_,_,_] = L) ->
    Sum = lists:sum(L),
    choose(rand:uniform(), [X/Sum || X <- L], possibilities()).

choose(X, [P|_], [H|_]) when X =< P -> H;
choose(X, [P|Ps], [_|T]) ->
    choose(X-P, Ps, T).

freq(L) ->
    freq(L, 0, 0, 0).

freq([], R, P, S) -> {R, P, S};
freq([H|T], R, P, S) ->
    case H of
        rock     -> freq(T, R+1, P, S);
        paper    -> freq(T, R, P+1, S);
        scissors -> freq(T, R, P, S+1)
    end.
