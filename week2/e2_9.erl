-module(e2_9).

-export([double/1, evens/1, tailevens/1]).

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

% Tests

test() ->
    [2, 4, 8, 6] = double([1, 2, 4, 3]),
    L = lists:seq(1,9),
    R = [2, 4, 6, 8] = evens(L),
    R = tailevens(L),
    ok.
