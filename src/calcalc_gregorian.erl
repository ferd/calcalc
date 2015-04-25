-module(calcalc_gregorian).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc:fixed(1).

-spec date({integer(), integer(), integer()}) -> calcalc:date().
date({Y,M,D}) -> #date{year=Y, month=M, day=D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #date{}) ->
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
to_fixed(#date{cal=?MODULE, year=Y, month=M, day=D}) ->
    IsLeap = is_leap_year(Y),
    epoch() - 1 % start at 0
    + 365 * (Y-1) % non-leap days between 0 and last day of preceding year
    + floor((Y-1) / 4) - floor((Y-1) / 100) + floor((Y-1) / 400) % preceding leap days
    + floor((367 * M - 362)/12) % days in current preceding months of year
    + D % days, with correction for the assumption that february has 30 days
    + if M =< 2            -> 0  % not february yet, no correction
       ; IsLeap            -> -1 % 30 days vs. actually 29
       ; M > 2, not IsLeap -> -2 % 28 days instead of 30
      end.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Year = year_from_fixed(Date),
    PriorDays = Date - new_year(Year),
    March1st = to_fixed(#date{year=Year, month=march(), day=1}),
    IsLeap = is_leap_year(Year),
    Correction = if Date < March1st -> 0
                  ; IsLeap -> 1
                  ; Date >= March1st, not IsLeap -> 2
                 end,
    Month = floor((12 * (PriorDays + Correction) + 373)/367),
    Day = 1 + Date - to_fixed(#date{year=Year, month=Month, day=1}),
    #date{year = Year, month=Month, day=Day}.

-spec day_number(calcalc:date()) -> 1..366.
day_number(Date=#date{year=Y}) ->
    date_difference(#date{year=Y-1, month=december(), day=31}, Date).

-spec days_remaining(calcalc:date()) -> 0..365.
days_remaining(Date=#date{cal=?MODULE, year=Y}) ->
    date_difference(Date, #date{year=Y, month=december(), day=31}).

-spec date_difference(calcalc:date(), calcalc:date()) -> calcalc:fixed().
date_difference(Date1=#date{}, Date2=#date{}) ->
    to_fixed(Date2) - to_fixed(Date1).

-spec year_from_fixed(calcalc:fixed()) -> integer().
year_from_fixed(Date) ->
    D0 = Date - epoch(),
    N400 = floor(D0/146097), % 400-years cycles from date to epoch
    D1 = mod(D0, 146097), % prior days not in N400
    N100 = floor(D1/36524), % 100-year cycles not in N400
    D2 = mod(D1, 36524), % prior days not in N400 nor N100
    N4 = floor(D2/1461), % 4-year cycle not in N400 nor N100
    D3 = mod(D2, 1461), % Prior days not in N400, N100, nor N4
    N1 = floor(D3/365), % Years not in N400, N100, nor N4
    Year = 400 * N400 + 100 * N100 + 4 * N4 + N1,
    if N100 == 4; N1 == 4 -> Year
    ; true -> Year + 1
    end.

is_leap_year(Year) ->
    mod(Year, 4) =:= 0 andalso not lists:member(mod(Year, 400), [100,200,300]).

year_range(Year) ->
    {new_year(Year), year_end(Year)}.

new_year(Year) ->
    to_fixed(#date{year=Year, month=january(), day=1}).

year_end(Year) ->
    to_fixed(#date{year=Year, month=december(), day=31}).

