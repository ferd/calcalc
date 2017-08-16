-module(calcalc_math).
-compile({no_auto_import, [min/2,max/2,ceil/1,floor/1]}).
-export([sum/3, min/2, max/2,
         lcm/2, gcd/2, mod/2, amod/2, signum/1,
         floor/1, ceil/1,
         deg/1, poly/2, sigma/2, final/2, next/2]).

-spec sum(F::fun((I) -> number()),
          I :: number(),
          Pred::fun((I) -> number())) -> number().
sum(F, I, P) ->
    case P(I) == 0 of
        true -> 0;
        false -> F(I) + sum(F, I+1, P)
    end.

%% @doc `min(I, Pred)' returns the first `I' value for which
%% `Pred(I)' returns `true', and searches by increments of `+1'.
-spec min(I, Pred::fun((I) -> boolean())) -> number() when
    I :: number().
min(I, P) ->
    case P(I) of
        true -> I;
        false -> min(I+1, P)
    end.

%% @doc `max(I, Pred)' returns the last `I' value for which
%% `Pred(I)' returns `true', and searches by increments of `+1'.
%% As soon as `Pred(I)' returns `false', `I-1' is returned.
-spec max(I, Pred::fun((I) -> boolean())) -> number() when
    I :: number().
max(I, P) ->
    case P(I) of
        true -> max(I+1, P);
        false -> I-1
    end.

-spec lcm(integer(), integer()) -> number().
lcm(X, Y) -> (X*Y) / gcd(X, Y).

-spec gcd(integer(), integer()) -> integer().
gcd(X, 0) -> X;
gcd(X, Y) -> gcd(Y, mod(X, Y)).

%% The modulus implementation in the book differs from the
%% one in the stdlib
%% -- (a, b] -> {a,b}
%mod(X, {A,B}) when B /= 0 -> B - mod(B-X, B-A);
mod(X, Y) when Y /= 0 -> X - Y * floor(X / Y).

%% adjusted modulus
amod(X, Y) when Y /= 0 ->
    Y + mod(X, -Y).

-spec signum(number()) -> -1 | 0 | 1.
signum(X) when X > 0 -> 1;
signum(X) when X < 0 -> -1;
signum(_) -> 0.

floor(X) when X >= 0 ->
    trunc(X);
floor(X) when X < 0 ->
    T = trunc(X),
    if X - T == 0 -> T
     ; X - T /= 0 -> T - 1
    end.

ceil(X) -> -floor(-X).

deg(X) -> X.

-spec poly(number(), [number()]) -> number().
poly(_, []) -> 0;
poly(X, [A|As]) -> A + X*poly(X, As).

-spec sigma([[number(), ...]], fun()) -> number().
sigma(ListOfLists, Fun) ->
    case lists:all(fun(L) -> L =:= [] end, ListOfLists) of
        true ->
            0;
        false ->
            Current = [hd(L) || L <- ListOfLists],
            Next = [tl(L) || L <- ListOfLists],
            Fun(Current) + sigma(Next, Fun)
    end.

final(K, Pred) ->
    NewK = K+1,
    case not Pred(NewK) of
        true -> K;
        false -> final(NewK, Pred)
    end.

next(K, Pred) ->
    case Pred(K) of
        true -> K;
        false -> next(K+1, Pred)
    end.
