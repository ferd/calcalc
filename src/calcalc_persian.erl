-module(calcalc_persian).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1]).
-import(calcalc_astro, [angle/3, deg/1, mt/1, hr/1]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> 226896. % julian 622/03/19

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?MODULE, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

farvadin() -> 1.
ordibehesht() -> 2.
xordad() -> 3.
tir() -> 4.
mordad() -> 5.
shahrivar() -> 6.
mehr() -> 7.
aban() -> 8.
azar() -> 9.
dey() -> 10.
bahman() -> 11.
esfand() -> 12.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, month := M, day := D}) ->
    NewY = new_year_on_or_before(
        epoch() + 180 + floor(
           calcalc_astro:mean_tropical_year() *
           if 0 < Y -> Y-1
            ; 0 >= Y -> Y
           end
        )
    ),
    NewY - 1 +
    if M =< 7 -> 31*(M-1)
     ; M > 7 -> 30*(M-1)+6
    end + D.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    NewYear = new_year_on_or_before(Date),
    Y = round((NewYear-epoch())/calcalc_astro:mean_tropical_year()) + 1,
    Year = if Y < 0 -> Y
            ; Y >= 0 -> Y-1
           end,
    DayOfYear = 1+Date - to_fixed(date(#{year => Year, month => 1, day => 1})),
    Month = if DayOfYear =< 186 -> ceil((1/31) * DayOfYear)
             ; DayOfYear > 186 -> ceil((1/30) * (DayOfYear-6))
            end,
    Day = Date - to_fixed(date(#{year => Year, month => Month, day => 1})) + 1,
    #{cal => ?MODULE, year => Year, month => Month, day => Day}.

new_year_on_or_before(Date) ->
    Approx = calcalc_astro:estimate_prior_solar_longitude(
        calcalc_astro:spring(),
        calcalc_loc:tehran_midday(Date)
    ),
    calcalc_math:next(floor(Approx)-1, fun(Day) ->
        calcalc_astro:solar_longitude(calcalc_loc:tehran_midday(Day))
        =< calcalc_astro:spring() + deg(2)
    end).
