-module(calcalc_ethiopic).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

%% Fixed 2796 => 8 C.E. august 29 (julian)
-spec epoch() -> integer().
epoch() -> calcalc:fixed(2796).

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

maskaram() -> 1.
teqempt() -> 2.
hedar() -> 3.
takhsas() -> 4.
ter() -> 5.
yakatit() -> 6.
magabit() -> 7.
miyazya() -> 8.
genbot() -> 9.
sane() -> 10.
hamle() -> 11.
nahase() -> 12.
paguemen() -> 13.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Year, month := M, day := D}) ->
    epoch() + calcalc_coptic:to_fixed(calcalc_coptic:date(
            #{year => Year, month => M, day => D}
    )) - calcalc_coptic:epoch().

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Coptic = calcalc_coptic:from_fixed(Date + calcalc_coptic:epoch() - epoch()),
    Coptic#{cal := ?CAL}.

