-module(calcalc_old_hindu_lunisolar).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc_old_hindu_solar:epoch().

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D, leap_month := LM}) ->
    #{cal => ?CAL, year => Y, month => M, day => D, leap_month => LM}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, month := M, day := D, leap_month := LM}) ->
    SolarMonth = calcalc_old_hindu_solar:arya_solar_month(),
    LunarMonth = arya_lunar_month(),
    Mina = (12 * Y - 1) * SolarMonth,
    NewYear = LunarMonth * (floor(Mina/LunarMonth) + 1),
    ceil(epoch() + NewYear + LunarMonth
         * case (not LM) andalso
                 ceil((NewYear - Mina) / (SolarMonth - LunarMonth)) =< M of
               true -> M;
               false -> M-1
           end
         + (D - 1) * arya_lunar_day() - calcalc_old_hindu_solar:hour(6)).


-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    SolarMonth = calcalc_old_hindu_solar:arya_solar_month(),
    LunarMonth = arya_lunar_month(),
    Sun = calcalc_old_hindu_solar:day_count(Date)+calcalc_old_hindu_solar:hour(6),
    NewMoon = Sun - mod(Sun, LunarMonth),
    LeapMod = mod(NewMoon, SolarMonth),
    Leap = (SolarMonth - LunarMonth) >= LeapMod
         andalso LeapMod > 0,
    Month = mod(ceil(NewMoon/SolarMonth), 12) + 1,
    Day = mod(floor(Sun/arya_lunar_day()), 30) + 1,
    Year = ceil((NewMoon + SolarMonth)/
                calcalc_old_hindu_solar:arya_solar_year()) - 1,
    #{cal => ?CAL, year => Year, month => Month, day => Day,
      leap_month => Leap}.

is_leap_year(Year) ->
    mod(Year * calcalc_old_hindu_solar:arya_solar_year()
        - calcalc_old_hindu_solar:arya_solar_month(),
        arya_lunar_month()) >= (23902504679 / 1282400064).

%% in months
avg_year_length() ->
    calcalc_old_hindu_solar:arya_solar_year() / arya_lunar_month().

year_delta() ->
    2 - calcalc_old_hindu_solar:arya_solar_month() / arya_lunar_month().

arya_lunar_day() ->
    (1 / 30) * arya_lunar_month().

arya_lunar_month() ->
    1577917500 / 53433336.
