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
    DayOfTun = mod(DayOfKatun, 260),
    Uinal = floor(DayOfTun / 20),
    Kin = mod(DayOfTun, 20),
    #{cal => ?CAL, baktun => Baktun, katun => Katun, tun => Tun,
      uinal => Uinal, kin => Kin}.
