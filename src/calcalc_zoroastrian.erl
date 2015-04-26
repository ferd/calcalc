-module(calcalc_zoroastrian).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> 230638.

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(Date = #{cal := ?CAL}) ->
    EDate = Date#{cal := calcalc_egyptian},
    epoch() + calcalc_egyptian:to_fixed(EDate) - calcalc_egyptian:epoch().

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    D = calcalc_egyptian:from_fixed(Date + calcalc_egyptian:epoch() - epoch()),
    D#{cal := ?CAL}.

