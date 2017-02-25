-module(week1).

% Shapes

-export([area/1, perimeter/1, enclose/1]).

% Bits

-export([bits/1, bits2/1]).

% Tests

-export([test/0]).

% Shapes

-type point()  :: {X :: number(), Y :: number()}.
-type circle() :: {'circle', Center :: point(), Readius :: number()}.
-type rectangle() ::
    {'rectangle', Center :: point(), Height :: number(), Width :: number()}.
-type triangle() ::
    {'triangle', A :: point(), B :: point(), C :: point()}.

-type shape() :: circle() | rectangle() | triangle().

-export_type([shape/0]).

-spec area(Shape :: shape()) -> number().
area({circle, {_X, _Y}, R}) ->
    math:pi() * R * R;
area({rectangle, {_X, _Y}, H, W}) ->
    H * W;
area({triangle, A, B, C}) ->
    LA = length(A, B),
    LB = length(B, C),
    LC = length(A, C),
    S = (LA + LB + LC)/2,
    math:sqrt(S * (S - LA) * (S - LB) * (S - LC)).

-spec perimeter(Shape :: shape()) -> number().
perimeter({circle, {_X, _Y}, R}) ->
    2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) ->
    2 * (H + W);
perimeter({triangle, A, B, C}) ->
    length(A, B) + length(B, C) + length(A, C).

-spec enclose(Shape :: shape()) -> rectangle().
enclose({circle, {_X, _Y} = C, R}) ->
    {rectangle, C, 2 * R, 2 * R};
enclose({rectangle, {_X, _Y}, _H, _W} = R) ->
    R;
enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
    Xmin = min(X1, X2, X3),
    Xmax = max(X1, X2, X3),
    Ymin = min(Y1, Y2, Y3),
    Ymax = max(Y1, Y2, Y3),
    {rectangle,
     {avg(Xmin, Xmax), avg(Ymin, Ymax)}, Ymax - Ymin, Xmax - Xmin}.

% Bits

-spec bits(X :: non_neg_integer()) -> non_neg_integer().
bits(X) when is_integer(X), X >= 0 ->
    bits(X, 0).

% slower because allocates memory on the stack
-spec bits2(X :: non_neg_integer()) -> non_neg_integer().
bits2(0) -> 0;
bits2(X) when is_integer(X), X > 0 ->
    (X band 1) + bits2(X bsr 1).

%% Internal functions

bits(0, A) -> A;
bits(X, A) ->
    bits(X band (X - 1), A + 1). % X band (X - 1) removes leftmost one
%    bits(X bsr 1, A + (X band 1)). % bit shift and bit masking solution
%    bits(X div 2, A + (X rem 2)). % dividing by 2 works as shift

length({X1, Y1}, {X2, Y2}) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    math:sqrt(DX * DX + DY * DY).

min(A, B, C) ->
    min(A, min(B, C)).

max(A, B, C) ->
    max(A, max(B, C)).

avg(A, B) ->
    (A + B) / 2.

%% Tests

test() ->
    C = {circle, {1.0, 2.0}, 2.5},
    R = {rectangle, {2.0, 1.0}, 2.0, 3.0},
    T = {triangle, {1.0, 1.0}, {3.0, 0.0}, {2.0, 2.0}},

    [[19.634954084936208,6.0,1.4999999999999998],
     [15.707963267948966,10.0,5.8863495173726745],
     [{rectangle,{1.0,2.0},5.0,5.0},
      {rectangle,{2.0,1.0},2.0,3.0},
      {rectangle,{2.0,1.0},2.0,2.0}]] =
    [ [ ?MODULE:F(X) || X <- [C, R, T] ]
      || F <- [area, perimeter, enclose] ],

    1 = bits(8),
    3 = bits(7),

    1 = bits2(8),
    3 = bits2(7),

    ok.
