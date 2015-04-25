-module(calcalc_egyptian).
-export([epoch/0, date/1, is_valid/1,
         to_fixed/1, from_fixed/1]).
-import(calcalc_math, [floor/1, mod/2]).
-include("calcalc.hrl").

-spec epoch() -> integer().
epoch() -> -272787. % jd 1448638

-spec date({integer(), integer(), integer()}) -> calcalc:date().
date({Y,M,D}) -> #date{year=Y, month=M, day=D}.

-spec is_valid(calcalc:date()) -> boolean().
is_valid(Date = #date{}) ->
    Date == from_fixed(to_fixed(Date)).

-spec to_fixed(calcalc:date()) -> calcalc:fixed().
to_fixed(#date{cal=?MODULE, year=Y, month=M, day=D}) ->
    epoch() + 365 * (Y - 1) + 30 * (M - 1) + D - 1.

-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    Days = Date - epoch(),
    Y = floor(Days/365)+1,
    M = floor(1/30 * mod(Days, 365)) + 1,
    D = Days - 365 * (Y - 1) - 30 * (M - 1) + 1,
    #date{year = Y, month = M, day = D}.
