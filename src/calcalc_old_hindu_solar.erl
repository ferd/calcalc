-module(calcalc_old_hindu_solar).

-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> -1132959.

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, month := M, day := D}) ->
    ceil(epoch() +
         Y * arya_solar_year() +
         (M - 1) * arya_solar_month() +
         D - hour(30)).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Sun = day_count(Date) + hour(6),
    Year = floor(Sun / arya_solar_year()),
    Month = mod(floor(Sun / arya_solar_month()), 12) + 1,
    Day = floor(mod(Sun, arya_solar_month())) + 1,
    #{cal => ?CAL, year => Year, month => Month, day => Day}.

jovian_year(Date) ->
    amod(27 + floor(day_count(Date) / (1 / 12 * arya_jovian_period())), 60).

day_count(Date) ->
    Date - epoch().

arya_solar_month() ->
    1/12 * arya_solar_year().

arya_solar_year() ->
    1577917500 / 4320000.

arya_jovian_period() ->
    1577917500 / 364224.

hour(X) ->
    X / 24.

