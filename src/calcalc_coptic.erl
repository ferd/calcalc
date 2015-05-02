-module(calcalc_coptic).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

%% Fixed 103605 => 384 C.E. august 29 (julian)
-spec epoch() -> integer().
epoch() -> calcalc:fixed(103605).

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

thoout() -> 1.
paope() -> 2.
athor() -> 3.
koiak() -> 4.
tobe() -> 5.
meshir() -> 6.
paremotep() -> 7.
parmoute() -> 8.
pashons() -> 9.
paone() -> 10.
epep() -> 11.
mesore() -> 12.
epagomene() -> 13.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Year, month := M, day := D}) ->
    epoch() - 1 + 365 * (Year-1) + floor(Year/4)
    + 30 * (M-1) + D.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Year = floor((4 * (Date - epoch()) + 1463) / 1461),
    Month = 1 + floor(
        (Date - to_fixed(#{cal => ?CAL, year => Year, month => 1, day => 1}))
         / 30),
    Day = Date + 1 - to_fixed(#{cal => ?CAL, year => Year, month => Month, day => 1}),
    #{cal => ?CAL, year => Year, month => Month, day => Day}.

-spec is_leap_year(calcalc:date() | integer()) -> boolean().
is_leap_year(#{cal := ?CAL, year := Y}) ->
    is_leap_year(Y);
is_leap_year(Year) when is_integer(Year) ->
    mod(Year, 4) =:= 3.
