-module(calcalc_julian).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

%% Fixed 1 => Jan 3, 1 C.E.
-spec epoch() -> integer().
epoch() -> calcalc:fixed(-1).

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?CAL, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

january() -> 1.
february() -> 2.
march() -> 3.
april() -> 4.
may() -> 5.
june() -> 6.
july() -> 7.
august() -> 8.
september() -> 9.
october() -> 10.
november() -> 11.
december() -> 12.

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := Year, month := M, day := D}) ->
    Y = if Year < 0 -> Year+1
         ; Year >= 0 -> Year
        end,
    IsLeap = is_leap_year(Year),
    epoch() - 1 % days before start of calendar
    + 365 * (Y-1) % non-leap days between 0 and last day of preceding year
    + floor((Y-1) / 4) % preceding leap days
    + floor((367 * M - 362)/12) % days in current preceding months of year
    + D % days, with correction for the assumption that february has 30 days
    + if M =< 2            -> 0  % not february yet, no correction
       ; IsLeap            -> -1 % 30 days vs. actually 29
       ; M > 2, not IsLeap -> -2 % 28 days instead of 30
      end.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Approx = floor((4 * (Date - epoch()) + 1464)/1461),
    Year = if Approx =< 0 -> Approx-1 % no year 0
            ; Approx > 0  -> Approx
           end,
    PriorDays = Date - new_year(Year),
    March1st = to_fixed(#{cal => ?CAL, year => Year, month => march(), day => 1}),
    IsLeap = is_leap_year(Year),
    Correction = if Date < March1st -> 0
                  ; IsLeap -> 1
                  ; Date >= March1st, not IsLeap -> 2
                 end,
    Month = floor((12 * (PriorDays + Correction) + 373)/367),
    Day = Date + 1 - to_fixed(#{cal => ?CAL, year => Year, month => Month, day => 1}),
    #{cal => ?CAL, year => Year, month => Month, day => Day}.

-spec day_number(calcalc:date()) -> 1..366.
day_number(Date=#{year := Y}) ->
    date_difference(#{cal => ?CAL, year => Y-1, month => december(), day => 31}, Date).

-spec days_remaining(calcalc:date()) -> 0..365.
days_remaining(Date=#{cal := ?CAL, year := Y}) ->
    date_difference(Date, #{cal => ?CAL, year => Y, month => december(), day => 31}).

-spec date_difference(calcalc:date(), calcalc:date()) -> calcalc:fixed().
date_difference(Date1=#{}, Date2=#{}) ->
    to_fixed(Date2) - to_fixed(Date1).

-spec is_leap_year(calcalc:date() | integer()) -> boolean().
is_leap_year(#{cal := ?CAL, year := Year}) ->
    is_leap_year(Year);
is_leap_year(Year) when is_integer(Year) ->
    mod(Year, 4) =:=  if Year > 0 -> 0; Year =< 0 -> 3 end.

new_year(Year) ->
    to_fixed(#{cal => ?CAL, year => Year, month => january(), day => 1}).

year_end(Year) ->
    to_fixed(#{cal => ?CAL, year => Year, month => december(), day => 31}).

bce(N) -> -N.
ce(N) -> N.
