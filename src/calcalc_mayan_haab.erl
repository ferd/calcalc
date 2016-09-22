-module(calcalc_mayan_haab).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc:fixed_from_jd(584283) - ordinal(18, 8).

-spec date(map()) -> calcalc:date().
date(#{month := M, day := D}) ->
    #{cal => ?CAL, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

%% Haab months
pop() -> 1.
uo() -> 2.
zip() -> 3.
zots() -> 4.
tzec() -> 5.
xul() -> 6.
yaxkin() -> 7.
mol() -> 8.
chen() -> 9.
yax() -> 10.
zac() -> 11.
ceh() -> 12.
mac() -> 13.
kankin() -> 14.
muan() -> 15.
pax() -> 16.
kayab() -> 17.
cumku() -> 18.
uayeb() -> 19.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, month := _, day := _}) ->
    error(mayan_haab_has_no_year_and_cannot_be_converted).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Count = mod(Date - epoch(), 365),
    Day = mod(Count, 20),
    Month = floor(Count / 20) + 1,
    #{cal => ?CAL, month => Month, day => Day}.

on_or_before(#{cal := ?CAL, month := Month, day := Day}, Date) ->
    Date - mod(Date - epoch() - ordinal(Month, Day), 365).

ordinal(#{cal := ?CAL, month := M, day := D}) ->
    ordinal(M, D).

ordinal(Month, Day) ->
    (Month - 1) * 20 + Day.

