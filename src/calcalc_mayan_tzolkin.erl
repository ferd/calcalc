-module(calcalc_mayan_tzolkin).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).

-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc:fixed_from_jd(584283) - ordinal(4, 20).

-spec date(map()) -> calcalc:date().
date(#{number := Num, name := Name}) ->
    #{cal => ?CAL, number => Num, name => Name}.

imix() -> 1.
ik() -> 2.
akbal() -> 3.
kan() -> 4.
chicchan() -> 5.
cimi() -> 6.
manik() -> 7.
lamat() -> 8.
muluc() -> 9.
oc() -> 10.
chuen() -> 11.
eb() -> 12.
ben() -> 13.
ix() -> 14.
men() -> 15.
cib() -> 16.
caban() -> 17.
etznab() -> 18.
cauac() -> 19.
ahau() -> 20.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, month := _, day := _}) ->
    error(mayan_tzolkin_has_no_year_and_cannot_be_converted).

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
              Date + 364
            )
    end.

ordinal(#{cal := ?CAL, number := Num, name := Name}) ->
    ordinal(Num, Name).

ordinal(Num, Name) ->
    mod(Num - 1 + 39 * (Num - Name), 260).

