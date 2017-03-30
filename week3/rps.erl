-module(rps).

-export([possibilities/0, beats/1]).

-export([result/2]).

-export([tournament/2]).

-export([choose/0, choose/1, freq/1]).

-export([play/0, play/1, get_move/0]).

-export([rock/1, last/1, mk_inhuman/1, inhuman/2, markov/1]).

-export([init_markov_db/2]).

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

play() -> play(markov).

play(markov) -> play(fun markov/1);
play(rock) -> play(fun rock/1);
play(last) -> play(fun last/1);
play(inhuman) -> play(mk_inhuman(3));
play(Strategy) when is_function(Strategy, 1) ->
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
    get_move(io:get_line("Your move (r,p,s,q,?): ")).

get_move([$r|_]) -> rock;
get_move([$R|_]) -> rock;
get_move([$p|_]) -> paper;
get_move([$P|_]) -> paper;
get_move([$s|_]) -> scissors;
get_move([$S|_]) -> scissors;
get_move([$q|_]) -> quit;
get_move([$Q|_]) -> quit;
get_move([$?|_]) ->
    io:format("Play r[ock], p[aper], s[cissors] or q[uit].~n", []),
    get_move();
get_move(eof)    -> quit;
get_move({error, _}) -> quit;
get_move(_)      -> get_move().

rock(_) -> rock.

last([]) -> choose();
last([{M,_}|_]) -> M.

mk_inhuman(P) ->
    fun(Ms) -> inhuman(Ms, P) end.

inhuman([], _P) -> choose();
inhuman([{M, N}|_], P) ->
    case result(M, N) of
        win  ->             % player won so probably stick with it
            [C] = [X || X <- possibilities(), beats(X) =:= M ],
            prefer(C, P);
        draw -> choose();
        lose -> prefer(M,P)   % player lose, probably changes to beat me
    end.

prefer(M, P) ->
    choose([case X of M -> P; _ -> 1 end || X <- possibilities() ]).

choose() ->
    choose([1, 1, 1]).

choose({R, P, C}) -> choose([R, P, C]);
choose([_,_,_] = L) ->
    choose(L, possibilities()).

choose(Ps, Ms) ->
    Sum = lists:sum(Ps),
    choose(rand:uniform(), [X/Sum || X <- Ps], Ms).

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

-define(K, 1).

-define(MD_SRV, markov_db).
-define(MD_FILE, "rps_markov.dat").
-define(MD_RETRY, 3).
-define(MC_LEN, 3).

markov(Ms) ->
    observe(Ms),
    {R, P, S} = get_markov(Ms),
    io:format("Expect: ~p~n", [{R, P, S}]),
    choose({S+?K, R+?K, P+?K}).

ensure_markov_db() ->
    ensure_markov_db(?MD_RETRY).

observe(Ms) ->
    ensure_markov_db(),
    ?MD_SRV ! {observe, lists:sublist(Ms,1+?MC_LEN)}.

get_markov(Ms) ->
    Pid = ensure_markov_db(),
    MRef = monitor(process, Pid),
    Pid ! {get, self(), MRef, Ms},
    receive
        {ok, MRef, Result} -> demonitor(MRef, [flush]), Result;
        {'DOWN', MRef, process, Pid, Info} -> error(Info)
    after
        1000 ->
            demonitor(MRef, [flush]),
            error(timeout)
    end.

ensure_markov_db(N) ->
    case whereis(?MD_SRV) of
        undefined ->
            case start_markov_db() of
                ok -> ok;
                _ when N > 0 ->
                    ensure_markov_db(N-1);
                Error ->
                    exit({markov_db, Error})
            end;
        Pid -> Pid
    end.

start_markov_db() ->
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(?MODULE, init_markov_db, [self(), Ref]),
    receive
        {init, Ref} -> demonitor(MRef), ok;
        {'DOWN', MRef, process, Pid, Info} -> {error, Info}
    after
        1000 ->
            exit(Pid, kill),
            demonitor(MRef),
            timeout
    end.

init_markov_db(Parent, Ref) ->
    true = register(?MD_SRV, self()),
    {ok, FH} = file:open(?MD_FILE, [read, write, binary]),
    Parent ! {init, Ref},
    loop_markov_db(FH).

loop_markov_db(FH) ->
    receive
        {observe, Ms} ->
            write_observ(FH, Ms),
            loop_markov_db(FH);
        {get, Pid, Ref, Ms} ->
            Pid ! {ok, Ref, read_observ(FH, Ms)},
            loop_markov_db(FH);
        quit -> ok
    end.

pos(rock)     -> 0;
pos(paper)    -> 1;
pos(scissors) -> 2.

encode({A, _})   -> pos(A).
%encode({A, B})   -> 3*pos(A) + pos(B).

position(Ms) ->
    position(lists:sublist(Ms, ?MC_LEN), 0).

position([], P) -> P;
position([H|T], P) ->
    position(T, 3*(P+1) + encode(H)).
%   position(T, 9*(P+1) + encode(H)).

read_observ(FH, Ms) ->
    case file:pread(FH, 3*position(Ms), 3) of
        {ok, <<R, P, S>>} -> {R, P, S};
        eof -> {0, 0, 0}
    end.

write_observ(_, []) -> ok;
write_observ(FH, [{M,_}|Ms]) ->
    Ps = read_observ(FH, Ms),
    Pos = pos(M)+1,
    {R, P, S} = normalise(setelement(Pos, Ps, element(Pos, Ps) + 1)),
    ok = file:pwrite(FH, 3*position(Ms), <<R, P, S>>).

normalise({R, P, S}) when R > 255; P > 255; S > 255 ->
    {n(R), n(P), n(S)};
normalise(Ps) -> Ps.

n(X) when X > 0 -> X-1;
n(_) -> 0.
