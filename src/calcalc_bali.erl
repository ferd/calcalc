-module(calcalc_bali).
-export([epoch/0, date/1, is_valid/1, to_fixed/1,
         from_fixed/1]).
-compile(export_all).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc:fixed_from_jd(146).

-spec date(map()) -> calcalc:date().
date(#{luang := Luang, dwiwara := Dwiwara, triwara := Triwara,
       caturwara := Caturwara, pancawara := Pancawara, sadwara := Sandwara,
       saptawara := Saptawara, asatawara := Asatawara, sangawara := Sangawara,
       dasawara := Dasawara}) ->
    #{cal => ?CAL,
      luang => Luang, dwiwara => Dwiwara, triwara => Triwara,
      caturwara => Caturwara, pancawara => Pancawara, sadwara => Sandwara,
      saptawara => Saptawara, asatawara => Asatawara, sangawara => Sangawara,
      dasawara => Dasawara}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL}) ->
    error(bali_has_no_year_and_cannot_be_converted).

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    #{cal => ?CAL,
      luang => luang(Date),
      dwiwara => dwiwara(Date),
      triwara => triwara(Date),
      caturwara => caturwara(Date),
      pancawara => pancawara(Date),
      sadwara => sadwara(Date),
      saptawara => saptawara(Date),
      asatawara => asatawara(Date),
      sangawara => sangawara(Date),
      dasawara => dasawara(Date)}.

day(Date) -> mod(Date - epoch(), 210).

triwara(Date) ->
    mod(day(Date), 3) + 1.

sadwara(Date) ->
    mod(day(Date), 6) + 1.

saptawara(Date) ->
    mod(day(Date), 7) + 1.

pancawara(Date) ->
    amod(day(Date) + 2, 5).

week(Date) ->
    floor((1/7) * day(Date)) + 1.

dasawara(Date) ->
    I = pancawara(Date), % the book substracts 1, but nth is 0-indexed there
    J = saptawara(Date),
    mod(1 + element(I, {5,9,7,4,8}) + element(J, {5,4,3,7,8,6,9}), 10).

dwiwara(Date) ->
    amod(dasawara(Date), 2).

luang(Date) ->
    0 == mod(dasawara(Date), 2).

sangawara(Date) ->
    mod(max(0, day(Date)-3), 9) + 1.

asatawara(Date) ->
    mod(max(6, 4 + mod(day(Date) - 70, 210)), 8) + 1.

caturwara(Date) ->
    amod(asatawara(Date), 4).

on_or_before(#{cal := ?CAL, pancawara := A5, sadwara := A6, saptawara := B7},
             Date) ->
    B35 = mod(A5 + 14 + 15 * (B7 - A5), 35),
    Days = A6 + 36 * (B35 - A6),
    Delta = day(0),
    Date - mod(Date + Delta - Days, 210).

