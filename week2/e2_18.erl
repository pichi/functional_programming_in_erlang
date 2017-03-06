-module(e2_18).

-export([join/2, concat/1, member/2]).

-export([sort/1, qsort/1, insert_sort/1]).

-export([test/0]).

join([],     T) -> T;
join([X|Xs], T) ->
    [X|join(Xs, T)].

concat([]) -> [];
concat([H|T]) ->
    join(H, concat(T)).

member(_, [])    -> false;
member(X, [H|T]) ->
    X =:= H orelse member(X, T).

sort(L) ->
    merge([[X] || X <- L]).

merge([]) -> [];
merge([X]) -> X;
merge(L) ->
    merge(merge_pairs(L)).

merge_pairs([]     ) -> [];
merge_pairs([_] = L) -> L;
merge_pairs([A,B|T]) ->
    [merge(A, B)|merge_pairs(T)].

merge([], L) -> L;
merge([_|_]=L, []) -> L;
merge([H|T], [X|_]=L) when H < X ->
    [H | merge(T, L)];
merge([_|_]=L, [H|T]) ->
    [H | merge(L, T)].

qsort([]) -> [];
qsort([H|T]) ->
    {L, E, G} = split(H, T, [], [H], []),
    qsort(L) ++ E ++ qsort(G).

split(_, [], L, E, G) -> {L, E, G};
split(P, [H|T], L, E, G) when H < P ->
    split(P, T, [H|L], E, G);
split(P, [H|T], L, E, G) when H > P ->
    split(P, T, L, E, [H|G]);
split(P, [H|T], L, E, G) ->
    split(P, T, L, [H|E], G).

insert_sort([]) -> [];
insert_sort([H|T]) ->
    insert(H, insert_sort(T)).

insert(X, [H|T]) when X > H ->
    [H|insert(X, T)];
insert(X, L) ->
    [X | L].

test() ->
    "goodbye" = concat(["goo","d","","by","e"]),
    true  = member( 2,[2,0,0,1]),
    false = member(20,[2,0,0,1]),
    [1,2,3,4,5] = sort([4,3,5,1,2]),
    [1,2,3,4,5] = qsort([4,3,5,1,2]),
    [1,2,3,4,5] = insert_sort([4,3,5,1,2]),
    ok.
