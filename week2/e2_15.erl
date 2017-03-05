-module(e2_15).

-export([palindrome/1]).

-export([test/0]).

palindrome(L) ->
    L2 = normalise(L, []),
    L2 =:= lists:reverse(L2).

normalise([], Acc) -> Acc;
normalise([H|T], Acc) when H >= $a, H =< $z ->
    normalise(T, [H|Acc]);
normalise([H|T], Acc) when H >= $A, H =< $Z ->
    normalise(T, [H+($a-$A)|Acc]);
normalise([_|T], Acc) ->
    normalise(T, Acc).

test() ->
    true = palindrome("Able was I ere I saw Elba"),
    false = palindrome("Able wxs I ere I saw Elba"),
    true = palindrome("A man, a plan, a canal - Panama!"),
    false = palindrome("A an, a plan, a canal - Panama!"),
    true = palindrome("Madam, I'm Adam"),
    true = palindrome("Never odd or even"),
    true = palindrome("Doc, note: I dissent. A fast never prevents a fatness.
                      I diet on cod"),
    true = palindrome("T. Eliot, top bard, notes putrid tang emanating, is
                      sad; I'd assign it a name: gnat dirt upset on drab pot
                      toilet."),
    ok.
