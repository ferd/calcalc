-module(calcalc_math).
-export([sum/3,
         lcm/2, gcd/2, mod/2, amod/2, signum/1,
         floor/1, ceil/1,
         deg/1]).

-spec sum(F::fun((I) -> number()),
          I :: number(),
          Pred::fun((I) -> number())) -> number().
sum(F, I, P) ->
    case P(I) == 0 of
        true -> 0;
        false -> F(I) + sum(F, I+1, P)
    end.

-spec lcm(integer(), integer()) -> integer().
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
