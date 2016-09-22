-module(calcalc_aztec_xiuhmolpilli).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() ->
    %% actually not used; built from other calendars.
    calcalc_julian:to_fixed(calcalc_julian:date(
        #{year => 1521, month => calcalc_julian:august(), day => 13}
    )).

-spec date(map()) -> calcalc:date().
date(#{number := Num, name := Name}) ->
    #{cal => ?CAL, number => Num, name => Name}.

cipactli() -> 1.
ehecatl() -> 2.
calli() -> 3.
cuetzpallin() -> 4.
coatl() -> 5.
miquiztli() -> 6.
mazatl() -> 7.
tochtli() -> 8.
atl() -> 9.
itzcuintli() -> 10.
ozomatli() -> 11.
malinalli() -> 12.
acatl() -> 13.
ocelotl() -> 14.
quauhtli() -> 15.
cozcaquauhtli() -> 16.
ollin() -> 17.
tecpatl() -> 18.
quiahuitl() -> 19.
xochitl() -> 20.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, number := _, name := _}) ->
    error(aztec_xiuhmolpilli_has_no_year_and_cannot_be_converted).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    X = calcalc_aztec_xihuitl:on_or_before(
          calcalc_aztec_xihuitl:date(#{month => 18, day => 20}),
          Date + 364
    ),
    #{month := Month} = calcalc_aztec_xihuitl:from_fixed(Date),
    case Month of
        19 ->
            undefined;
        _ ->
            date(calcalc_aztec_tonalphualli:from_fixed(X))
    end.

on_or_before(Xihuitl, Tonalphualli, Date) ->
    XihuitlCount = calcalc_aztec_xihuitl:ordinal(Xihuitl) + calcalc_aztec_xihuitl:epoch(),
    TonalpohualliCount = calcalc_aztec_tonalpohualli:ordinal(Tonalphualli) + calcalc_aztec_tonalpohualli:epoch(),
    Diff = TonalpohualliCount - XihuitlCount,
    case mod(Diff, 5) of
        0 ->
            Date - mod(Date - XihuitlCount - 365 * Diff, 18980);
        _ ->
            undefined
    end.
