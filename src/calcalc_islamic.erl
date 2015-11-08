%% This calendar is arithmetic and not fully realistic; many
%% islamic calendars depend on proclamation of new moons by
%% religious authorities.
%% Days actually start on prior evenings, at sunset
-module(calcalc_islamic).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

%% July 16 622 C.E. (Julian)
-spec epoch() -> integer().
epoch() -> calcalc:fixed(227015).

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

muharram() -> 1.
safar() -> 2.
rabi_al_awwal() -> 3.
rabi_al_thani() -> 4.
jumada_al_awwal() -> 5.
jumada_al_thani() -> 6.
rajab() -> 7.
shaaban() -> 8.
ramadan() -> 9.
shawwal() -> 10.
dhu_al_qadah() -> 11.
dhu_al_hijjah() -> 12.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, month := M, day := D}) ->
    epoch() - 1
    + (Y-1) * 354 % avg lunar year: 354 + 11/30
    + floor((3+11*Y)/30)
    + 29 * (M-1) + floor(M/2) % 29 or 30 days months
    + D.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Y = floor((30 * (Date - epoch()) + 10646)/10631),
    PriorDays = Date - to_fixed(#{cal => ?CAL, year => Y, month => 1, day => 1}),
    M = floor((11 * PriorDays + 330)/325),
    D = 1 + Date - to_fixed(#{cal => ?CAL, year => Y, month => M, day => 1}),
    #{cal => ?CAL, year => Y, month => M, day => D}.

is_leap_year(#{cal := ?CAL, year := Year}) ->
    is_leap_year(Year);
is_leap_year(Year) when is_integer(Year) ->
    mod(14 + 11*Year, 30) < 11.
