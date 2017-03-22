-module(index).

-export([file/1]).

-export([get_file_contents/1, show_file_contents/1]).

-export([words/1, lines/1]).

% Index file
file(Name) ->
    lines(get_file_contents(Name)).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok, File} = file:open(Name, [read]),
    get_all_lines(File,[]).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File, Partial) ->
    case io:get_line(File, "") of
        eof ->
            file:close(File),
            lists:reverse(Partial);
        Line ->
            {Strip,_} = lists:split(length(Line)-1,Line),
            get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
show_file_contents([]) ->
    ok.

% Index lines as returned from get_file_contents/1
lines(Ls) ->
    lines(Ls, #{}, 1).

lines([], I, _) ->
    [ {K, lists:reverse(V)}                         % reverse occurrences, see line/3
      || {K, V} <- lists:sort(maps:to_list(I)) ];   % sort index alphabetically
lines([L|Ls], I, N) ->
    lines(Ls, line(words(L), I, N), N+1).

% Add line of words into index
% List of occurrences is build up in reverse so last entry is head of value
% in map
line([], I, _) -> I;
line([W|Ws], I, N) ->
    line(Ws,
         case maps:get(W, I, []) of
             [{_, N}|_] -> I;               % next occurrence on same line
             [{X, Y}|T] when N =:= Y+1 ->   % could extended last entry (previous line)
                 maps:put(W, [{X, N}|T], I);
             T -> maps:put(W, [{N,N}|T], I) % otherwise new entry
         end,
         N).

% get list of words from line
words(L) ->
    words(L, []).

% simple state parser, word starts with letter
words([H|T], Ws)
  when H >= $a, H =< $z;
       H >= $A, H =< $Z ->
    word(T, Ws, [H]);
words([_|T], Ws) -> % ignore all non-letters outside word
    words(T, Ws);
words([], Ws) -> Ws.

% accept apostrophe inside word
word([H|T], Ws, W)
  when H >= $a, H =< $z;
       H >= $A, H =< $Z;
       H =:= $' ->
    word(T, Ws, [H|W]);
word(Rest, Ws, W) -> % W is reversed here
    words(Rest, add_word(normalise_word(W), Ws)).

% we can filter what is considered word, e.g. accept only words with length
% more than 3
add_word(W, Ws) when length(W) > 3 -> [W|Ws];
add_word(_, Ws) -> Ws.

% make index case insensitive by transforming to lower case
% normalise_word also reverses input, see second clause of word/3
normalise_word(W) ->
    normalise_word(W, []).

normalise_word([H|T], W) ->
    normalise_word(T, [if H >= $A, H =< $Z -> H + ($a - $A);
                          true -> H end | W]);
normalise_word([], W) -> W.
