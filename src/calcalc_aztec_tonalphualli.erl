-module(calcalc_aztec_tonalphualli).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() ->
    calcalc_julian:to_fixed(calcalc_julian:date(
        #{year => 1521, month => calcalc_julian:august(), day => 13}
    )) - ordinal(1,5).

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
to_fixed(#{cal := ?CAL, month := _, day := _}) ->
    error(aztec_tonalphualli_has_no_year_and_cannot_be_converted).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Count = Date - epoch() + 1,
    Num = amod(Count, 13),
    Name = amod(Count, 20),
    #{cal => ?CAL, number => Num, name => Name}.

on_or_before(#{cal := ?CAL, number := Num, name := Name}, Date) ->
    Date - mod(Date - epoch() - ordinal(Num, Name), 260).

year_bearer(Date) ->
    case calcalc_mayan_haab:from_fixed(Date) of
        #{month := 19} ->
            undefined;
        _ ->
            calcalc_mayan_haab:on_or_before(
              calcalc_mayan_haab:date(#{month => 1, day => 0}),
              Date
            )
    end.

ordinal(#{cal := ?CAL, number := Num, name := Name}) ->
    ordinal(Num, Name).

ordinal(Num, Name) ->
    mod(Num - 1 + 39 * (Num - Name), 260).

