-module(calcalc_mayan_long).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc:fixed_from_jd(584283).

-spec date(map()) -> calcalc:date().
date(#{baktun := Baktun, katun := Katun, tun := Tun,
       uinal := Uinal, kin := Kin}) ->
    #{cal => ?CAL, baktun => Baktun, katun => Katun, tun => Tun,
      uinal => Uinal, kin => Kin}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, baktun := Baktun, katun := Katun, tun := Tun,
           uinal := Uinal, kin := Kin}) ->
    epoch() + Baktun * 144000 + Katun * 7200 + Tun * 360 + Uinal * 20 + Kin.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    LongCount = Date - epoch(),
    Baktun = floor(LongCount / 144000),
    DayOfBaktun = mod(LongCount, 144000),
    Katun = floor(DayOfBaktun / 7200),
    DayOfKatun = mod(DayOfBaktun, 7200),
    Tun = floor(DayOfKatun / 360),
    DayOfTun = mod(DayOfKatun, 360),
    Uinal = floor(DayOfTun / 20),
    Kin = mod(DayOfTun, 20),
    #{cal => ?CAL, baktun => Baktun, katun => Katun, tun => Tun,
      uinal => Uinal, kin => Kin}.

%% Calculate the calendar round (haab + tzolkin), forming a cycle
%% of ~52 solar years. We seek the latest date on or before `Date'
%% that falls on a specific date of the calendar round.
round_on_or_before(Haab, Tzolkin, Date) ->
    HaabCount = calcalc_mayan_haab:ordinal(Haab) + calcalc_mayan_haab:epoch(),
    TzolkinCount = calcalc_mayan_tzolkin:ordinal(Tzolkin) + calcalc_mayan_tzolkin:epoch(),
    Diff = TzolkinCount - HaabCount,
    case mod(Diff, 5) of
        0 ->
            Date - mod(Date - HaabCount - 365 * Diff, 18980);
        _ ->
            undefined
    end.
