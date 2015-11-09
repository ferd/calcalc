%% Days actually start on prior evenings, at sunset
-module(calcalc_hebrew).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

%% September 7 -3760 gregorian
-spec epoch() -> integer().
epoch() -> calcalc:fixed(-1373427).

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

nisan() -> 1. % 30 days
iyyar() -> 2. % 29 days
sivan() -> 3. % 30 days
tammuz() -> 4. % 29 days
av() -> 5. % 30 days
elul() -> 6. % 29 days
tishri() -> 7. % 30 days
marheshvan() -> 8. % 29 or 30 days
kislev() -> 9. % 29 or 30 days
tevet() -> 10. % 29 days
shevat() -> 11. % 30 days
adar() -> 12.  % 29 days, 30 on leap years
adarii() -> 13. % 29 days, leap years only

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Y, month := M, day := D}) ->
    F = fun(Month) -> last_day_of_month(Y, Month) end,
    PredM = fun(Month) -> (1+last_month(Y)) - Month end, % Month =< last_month, == 0
    Pred = fun(Month) -> M - Month end, % Month < M, == 0
    new_year(Y) + D - 1 +
    case M < tishri() of
        true ->
            sum(F, tishri(), PredM) + sum(F, nisan(), Pred);
        false ->
            sum(F, tishri(), Pred)
    end.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Approx = floor((98496/35975351) * (Date - epoch())) + 1,
    Year = calcalc_math:max(Approx-1, fun(Year) -> new_year(Year) =< Date end),
    Start = case Date < to_fixed(date(#{year => Year, month => nisan(), day => 1})) of
        true -> tishri();
        false -> nisan()
    end,
    Month = calcalc_math:min(Start,
                fun(M) ->
                    Date =< to_fixed(date(#{year => Year, month => M,
                                            day => last_day_of_month(Year, M)}))
                end),
    Day = 1 + Date - to_fixed(date(#{year => Year, month => Month, day => 1})),
    #{cal => ?CAL, year => Year, month => Month, day => Day}.

%%%
is_leap_year(#{cal := ?CAL, year := Year}) ->
    is_leap_year(Year);
is_leap_year(Year) when is_integer(Year) ->
    mod(7 * Year + 1, 19) < 7.

last_month(#{cal := ?CAL, year := Year}) ->
    last_month(Year);
last_month(Year) when is_integer(Year) ->
    case is_leap_year(Year) of
        true -> adarii();
        false -> adar()
    end.

is_sabbatical_year(#{cal := ?CAL, year := Year}) ->
    is_sabbatical_year(Year);
is_sabbatical_year(Year) when is_integer(Year) ->
    mod(Year, 7) =:= 0.

%% molad is the approximation of the mean lunar conjunction (when
%% the earth, moon, and sun, in that order, are in line -- AKA the
%% new moon) -- the margin of error from the actual conjunction
%% should be ~ ±16h
molad(#{cal := ?CAL, year := Y, month := M}) ->
    molad(Y, M).

molad(Year, M) ->
    Y = case M < tishri() of
            true -> Year+1;
            false -> Year
        end,
    MonthsElapsed = M - tishri() + floor((1/19) * (235 * Y - 234)),
    epoch() - 876/25920 + MonthsElapsed * (29 + 12 + 793/25920).

new_year(#{cal := ?CAL, year := Y}) ->
    new_year(Y);
new_year(Year) when is_integer(Year) ->
    epoch() + elapsed_days(Year) + year_length_correction(Year).

elapsed_days(#{cal := ?CAL, year := Y}) ->
    elapsed_days(Y);
elapsed_days(Year) when is_integer(Year) ->
    Months = floor((1/19) * (235*Year - 234)),
    Parts = 12084 + 13753 * Months,
    Days = 29 * Months + floor(Parts / 25920),
    case mod(3 * (Days+1), 7) < 3 of
        true -> Days+1;
        false -> Days
    end.

year_length_correction(#{cal := ?CAL, year := Y}) ->
    year_length_correction(Y);
year_length_correction(Year) when is_integer(Year) ->
    NY0 = elapsed_days(Year-1),
    NY1 = elapsed_days(Year),
    NY2 = elapsed_days(Year+1),
    if NY2 - NY1 =:= 365 -> 2
     ; NY1 - NY0 =:= 382 -> 1
     ; true -> 0
    end.

last_day_of_month(#{cal := ?CAL, year := Y, month := M}) ->
    last_day_of_month(Y, M).

last_day_of_month(Year, Month) ->
    case lists:member(Month, [iyyar(), tammuz(), elul(), tevet(), adarii()])
         orelse (Month =:= adar() andalso not is_leap_year(Year))
         orelse (Month =:= marheshvan() andalso not is_long_marheshvan(Year))
         orelse (Month =:= kislev() andalso is_short_kislev(Year)) of
        true -> 29;
        false -> 30
    end.

is_long_marheshvan(Year) ->
    Days = days_in_year(Year),
    Days =:= 355 orelse Days =:= 385.

is_short_kislev(Year) ->
    Days = days_in_year(Year),
    Days =:= 353 orelse Days =:= 383.

days_in_year(Year) ->
    new_year(Year+1) - new_year(Year).

