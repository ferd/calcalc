-module(calcalc_search).
-export([invert_angular/4, binary_search/4,
         next/2, final/2]).
-import(calcalc_math, [mod/2]).
-import(calcalc_astro, [deg/1]).

%% Find inverse of angular function `F' at point `Y' within interval
%% `[A..B]'.
invert_angular(F, Y, A, B) ->
    Accuracy = 1/100000,
    calcalc_search:binary_search(
        A, B,
        fun(_Low, _High, X) -> mod(F(X)-Y, 360) < deg(180) end,
        fun(Low, High) -> High - Low < Accuracy end
    ).

%% Bisection search for `AltDay' in [`Lo'..`Hi'] such that
%% `End' holds. `Test' determines when to go left.
-spec binary_search(Low, High, Test, End) -> Day when
    Low :: Day,
    High :: Day,
    Day :: number(),
    Test :: fun((Low, High, AltDay::Day) -> Day),
    End :: fun((Low, High) -> boolean()).
binary_search(Lo, Hi, Test, End) ->
    case End(Lo, Hi) of
        true ->
            (Lo + Hi) / 2;
        false ->
            X = (Lo + Hi) / 2,
            Left = Test(Lo, Hi, X),
            {NewLo, NewHi} = if Left -> {Lo, X}; not Left -> {X, Hi}  end,
            binary_search(NewLo, NewHi, Test, End)
    end.

%% First integer greater or equal to `Index' where `Cond/1' is `true'.
-spec next(number(), fun((number()) -> boolean())) -> number().
next(Index, Cond) ->
    case Cond(Index) of
        true -> Index;
        false -> next(Index+1, Cond)
    end.

%% Last integer greater or equal to `Index' where `Cond/1' is `true'.
-spec final(number(), fun((number()) -> boolean())) -> number().
final(Index, Cond) ->
    case Cond(Index) of
        true -> final(Index+1, Cond);
        false -> Index-1
    end.
