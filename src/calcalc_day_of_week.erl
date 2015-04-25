-module(calcalc_day_of_week).
-compile(export_all).
-import(calcalc_math, [mod/2]).

sunday() -> 0.
monday() -> 1.
tuesday() -> 2.
wednesday() -> 3.
thursday() -> 4.
friday() -> 5.
saturday() -> 6.

from_fixed(Date) ->
    mod(Date - calcalc:fixed(0) - sunday(), 7).

first_kday(K, Date) -> nth_kday(1, K, Date).
last_kday(K, Date) -> nth_kday(-1, K, Date).

%% N=0 is undefined
-spec nth_kday(pos_integer()|neg_integer(), integer(), calcalc:fixed()) -> calcalc:fixed().
nth_kday(N, K, Date) when N > 0 ->
    7*N + kday_before(K, Date);
nth_kday(N, K, Date) when N < 0 ->
    7*N + kday_after(K, Date).

%% Kth day of the week on or before fixed date `Date'
-spec kday_on_or_before(integer(), calcalc:fixed()) -> calcalc:fixed().
kday_on_or_before(K, Date) ->
    Date - from_fixed(Date-K).

-spec kday_on_or_after(integer(), calcalc:fixed()) -> calcalc:fixed().
kday_on_or_after(K, Date) ->
    kday_on_or_before(K, Date+6).

-spec kday_nearest(integer(), calcalc:fixed()) -> calcalc:fixed().
kday_nearest(K, Date) ->
    kday_on_or_before(K, Date+3).

-spec kday_before(integer(), calcalc:fixed()) -> calcalc:fixed().
kday_before(K, Date) ->
    kday_on_or_before(K, Date-1).

-spec kday_after(integer(), calcalc:fixed()) -> calcalc:fixed().
kday_after(K, Date) ->
    kday_on_or_before(K, Date+7).


