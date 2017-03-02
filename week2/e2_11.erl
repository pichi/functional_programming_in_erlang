-module(e2_11).

-export([take/2]).

-export([test/0]).

-spec take(N :: non_neg_integer(), L :: [T]) -> [T].

take(0, _    ) -> [];
take(_, []   ) -> [];
take(N, [H|T])
  when is_integer(N), N > 0 ->
    [H|take(N-1, T)].

test() ->
    []      = take(0, "hello"),
    "hell"  = take(4,"hello"),
    "hello" = take(5,"hello"),
    "hello" = take(9,"hello"),
    ok.
