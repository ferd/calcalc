-module(calcalc_roman).
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
date(#{year := Y, month := M, event := E, count := C, leap := L}) ->
    #{cal => ?CAL, year => Y, month => M, event => E,
      count => C, leap => L}.

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
to_fixed(#{cal := ?CAL, year := Y, month := M,
           event := Event, count := Count, leap := Leap}) ->
    case Event of
        kalends ->
            calcalc_julian:to_fixed(calcalc_julian:date(
                #{year => Y, month => M, day => 1}
            ));
        nones ->
            calcalc_julian:to_fixed(calcalc_julian:date(
                #{year => Y, month => M, day => nones(M)}
            ));
        ides ->
            calcalc_julian:to_fixed(calcalc_julian:date(
                #{year => Y, month => M, day => ides(M)}
            ))
    end % special day as a base
    - Count
    + case is_leap_year(Y) andalso M =:= march()
           andalso Event =:= kalends andalso
           16 >= Count andalso Count >= 6 of
        true -> 0;
        false -> 1
    end
    + if Leap =/= false -> 1
       ; true -> 0 % leap has a type of 'false' or some tuple
    end.


-spec from_fixed(calcalc:fixed()) -> calcalc:date().
from_fixed(Date) ->
    #{year := Y, month := M, day := D} = Julian = calcalc_julian:from_fixed(Date),
    Nones = nones(M),
    Ides = ides(M),
    February = february(),
    IsLeap = calcalc_julian:is_leap_year(Julian),
    if D =:= 1 ->
         #{cal => ?CAL, year => Y, month => M, event => kalends,
           count => 1, leap => false}
     ; D =< Nones ->
         #{cal => ?CAL, year => Y, month => M, event => nones,
           count => Nones - D + 1, leap => false}
     ; D =< Ides ->
         #{cal => ?CAL, year => Y, month => M, event => ides,
           count => Ides - D + 1, leap => false}
     ; M =/= February; not IsLeap ->
         M2 = amod(M+1, 12),
         Y2 = if M2 =/= 1 -> Y
               ; Y =/= -1 -> Y+1
               ; M =:= 1, Y =:= -1 -> 1
              end,
         Kalends = to_fixed(
                #{cal => ?CAL, year => Y2, month => M2, event => kalends,
                  count => 1, leap => false}
         ),
         #{cal => ?CAL, year => Y2, month => M2, event => kalends,
           count => Kalends - Date + 1, leap => false}
     ; D < 25 ->
         #{cal => ?CAL, year => Y, month => march(), event => kalends,
           count => 30 - D, leap => false}
     ; true ->
         #{cal => ?CAL, year => Y, month => march(), event => kalends,
           count => 31 - D, leap => {day,25}}
    end.

ides(Month) ->
    case lists:member(Month, [march(), may(), july(), october()]) of
        true -> 15;
        false -> 13
    end.

nones(Month) ->
    ides(Month) - 8.

is_leap_year(Year) ->
    mod(Year, 4) =:=  if Year > 0 -> 0; Year =< 0 -> 3 end.

rome_foundation() -> bce(753).

julian_year_from_auc(Year) when is_integer(Year) ->
    RomeFounded = rome_foundation(),
    case 1 =< Year andalso Year =< -RomeFounded of
        true -> Year + RomeFounded - 1;
        false -> Year + RomeFounded
    end.

auc_year_from_julian(Year) when is_integer(Year) ->
    RomeFounded = rome_foundation(),
    case RomeFounded =< Year andalso Year =< -1 of
        true -> Year - RomeFounded + 1;
        false -> Year - RomeFounded
    end.

bce(N) -> -N.
ce(N) -> N.

