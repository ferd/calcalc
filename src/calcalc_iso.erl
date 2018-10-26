-module(calcalc_iso).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc_gregorian:epoch(). % fixed(1)

-spec date(map()) -> calcalc:date().
date(#{year := Y, week := W, day := D}) ->
    #{cal => ?CAL, year => Y, week => W, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, week := W, day := D}) ->
    calcalc_day_of_week:nth_kday(
        W, calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(calcalc_gregorian:date(
            #{year => Y-1, month => calcalc_gregorian:december(), day => 28}
        ))
    ) + D.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Approx = calcalc_gregorian:year_from_fixed(Date-3),
    Year = case Date >= to_fixed(date_from_year(Approx+1)) of
        true -> Approx+1;
        false -> Approx
    end,
    Week = 1 + floor((Date - to_fixed(date_from_year(Year)))/7),
    Day = amod(Date - calcalc:fixed(0), 7),
    #{cal => ?CAL, year => Year, week => Week, day => Day}.

iso_long_year(Year) ->
    Jan1 = calcalc_day_of_week:from_fixed(calcalc_gregorian:new_year(Year)),
    Dec31 = calcalc_day_of_week:from_fixed(calcalc_gregorian:year_end(Year)),
    Jan1 =:= calcalc_day_of_week:thursday()
    orelse
    Dec31 =:= calcalc_day_of_week:thursday().

date_from_year(Year) ->
    #{cal => ?CAL, year => Year, week => 1, day => 1}.
