-module(rps).

-export([possibilities/0, beats/1]).

-export([result/2]).

-export([tournament/2]).

-export([choose/0, choose/1, freq/1]).

-export([play/1, get_move/0]).

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

points2result(0) -> draw;
points2result(X) when X > 0 -> win;
points2result(_) -> lose.

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

play(Strategy) ->
    play(Strategy, [], 0).

play(Strategy, Moves, Score) ->
    Move = Strategy(Moves),
    case get_move() of
        quit ->
            io:format("You ~p with final score ~b.~n",
                      [points2result(Score), Score]);
        YourMove ->
            R = result(YourMove, Move),
            S = Score + result2points(R),
            io:format("You ~p (~p x ~p, ~b so far)~n",
                      [R, YourMove, Move, S]),
            play(Strategy, [{YourMove, Move}|Moves], S)
    end.

get_move() ->
    get_move(io:get_line("Your move (r,p,s,q): ")).

get_move([$r|_]) -> rock;
get_move([$R|_]) -> rock;
get_move([$p|_]) -> paper;
get_move([$P|_]) -> paper;
get_move([$s|_]) -> scissors;
get_move([$S|_]) -> scissors;
get_move([$q|_]) -> quit;
get_move([$Q|_]) -> quit;
get_move(eof)    -> quit;
get_move({error, _}) -> quit;
get_move(_)      -> get_move().
