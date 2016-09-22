-module(calcalc_aztec_xihuitl).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() ->
    calcalc_julian:to_fixed(calcalc_julian:date(
        #{year => 1521, month => calcalc_julian:august(), day => 13}
    )) - ordinal(11,2).

-spec date(map()) -> calcalc:date().
date(#{month := M, day := D}) ->
    #{cal => ?CAL, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

%% Haab months
izcalli() -> 1.
atlcahualo() -> 2.
tlacaxipehualiztli() -> 3.
tozoztontli() -> 4.
huei_tozoztli() -> 5.
toxcatl() -> 6.
etzalcualiztli() -> 7.
tecuilhuitontli() -> 8.
huei_tecuilhuitl() -> 9.
tlaxochimaco() -> 10.
xocotlhuetzi() -> 11.
ochpaniztli() -> 12.
teotleco() -> 13.
tepeilhuitl() -> 14.
quecholli() -> 15.
panquetzaliztli() -> 16.
atemoztli() -> 17.
tititl() -> 18.
nemontemi() -> 19.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, month := _, day := _}) ->
    error(aztec_xihuitl_has_no_year_and_cannot_be_converted).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Count = mod(Date - epoch(), 365),
    Day = mod(Count, 20) + 1,
    Month = floor(Count/20) + 1,
    #{cal => ?CAL, month => Month, day => Day}.

on_or_before(#{cal := ?CAL, month := Month, day := Day}, Date) ->
    Date - mod(Date - epoch() - ordinal(Month, Day), 365).

ordinal(#{cal := ?CAL, month := M, day := D}) ->
    ordinal(M, D).

ordinal(Month, Day) ->
    (Month - 1) * 20 + Day - 1.

