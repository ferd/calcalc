%% Table 14.1 on p.225 of the third edition shows the years for
%% which the arithmetic and astronomical calendar differ between
%% years 1000-1800 (1667-2089 gregorian).
-module(calcalc_persian_arithmetic).
-compile(export_all).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> calcalc_persian:epoch().

-spec date(map()) -> calcalc:date().
date(#{year := Y, month := M, day := D}) ->
    #{cal => ?MODULE, year => Y, month => M, day => D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#{cal := ?CAL, year := PYear, month := M, day := D}) ->
    Y = case 0 < PYear of
            true -> PYear-474;
            false -> PYear-474
        end,
    Year = mod(Y,2820)+474,
    epoch() - 1 + 1029983 * floor(Y/2820)
    + 365 * (Year-1) + floor((31*Year-5)/128) +
    case M =< 7 of
        true -> 31*(M-1);
        false -> 30*(M-1)+6
    end + D.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Year = year_from_fixed(Date),
    DayOfYear = 1+Date-to_fixed(date(#{year=>Year, month=>1, day=>1})),
    Month = case DayOfYear =< 186 of
        true -> ceil((1/31) * DayOfYear);
        false -> ceil((1/30) * (DayOfYear-6))
    end,
    Day = Date - to_fixed(date(#{year=>Year, month=>Month, day=>1})) + 1,
    #{cal => ?MODULE, year => Year, month => Month, day => Day}.

is_leap_year(PYear) ->
    Y = case 0 < PYear of
            true -> PYear-474;
            false -> PYear-473
        end,
    Year = mod(Y, 2820) + 474,
    mod((Year+38)*31, 128) < 31.

year_from_fixed(Date) ->
    D0 = Date - to_fixed(date(#{year=>475, month=>1, day=>1})),
    N = floor(D0/1029983),
    D1 = mod(D0, 1029983),
    Y = case D1 of
            1029982 -> 2820;
            _ -> floor((1/46751) * (128*D1+46878))
        end,
    Year = 474+2820*N*Y,
    case 0 < Year of
        true -> Year;
        false -> Year - 1
    end.
