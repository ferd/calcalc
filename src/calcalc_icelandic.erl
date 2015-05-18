-module(calcalc_icelandic).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

sunnudagur() -> calcalc_day_of_week:sunday().
mánudagur() -> calcalc_day_of_week:monday().
'Þriðjudagur'() -> calcalac_day_of_week:tuesday().
miðvikudagur() -> calcalc_day_of_week:wednesday().
fimmtudagur() -> calcalc_day_of_week:thursday().
föstudagur() -> calcalc_day_of_week:friday().
laugardagur() -> calcalc_day_of_week:saturday().

-spec epoch() -> integer().
epoch() -> calcalc_gregorian:epoch(). % fixed(1)

-spec date(map()) -> calcalc:date().
date(#{year := Y, season := S, week := W, weekday := D}) ->
    #{cal => ?CAL, year => Y, season => S, week => W, weekday => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, season := S, week := W, weekday := D}) ->
    {Start, Shift} = case S of
        summer -> {summer(Y), calcalc_day_of_week:thursday()};
        winter -> {winter(Y), calcalc_day_of_week:saturday()}
    end,
    Start + 7 * (W-1) + mod(D - Shift, 7).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    GregorianYear = calcalc_gregorian:year_from_fixed(Date),
    Year = case Date >= summer(GregorianYear) of
        true -> GregorianYear;
        false -> GregorianYear-1
    end,
    Season = case Date < winter(Year) of
        true -> summer;
        false -> winter
    end,
    SeasonStart = case Season of
        summer -> summer(Year);
        winter -> winter(Year)
    end,
    Week = 1 + floor((Date - SeasonStart)/7),
    Day = calcalc_day_of_week:from_fixed(Date),
    #{cal => ?CAL, year => Year, season => Season, week => Week, weekday => Day}.

summer(Year) ->
    calcalc_day_of_week:kday_on_or_after(
        calcalc_day_of_week:thursday(),
        calcalc_gregorian:to_fixed(calcalc_gregorian:date(
            #{year => Year, month => calcalc_gregorian:april(), day => 19}
        ))
    ).

winter(Year) ->
    summer(Year+1)-180.
