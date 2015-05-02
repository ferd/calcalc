-module(calcalc_holiday).
-compile(export_all).
-include("calcalc.hrl").

us_independence_day(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_gregorian:to_fixed(
        #{cal => calcalc_gregorian,
          year => Y, month => calcalc_gregorian:july(), day => 4}
    ).

us_labor_day(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:monday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:september(), day => 1}
        )
    ).

us_memorial_day(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:last_kday(
        calcalc_day_of_week:monday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:may(), day => 31}
        )
    ).

us_election_day(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:tuesday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:november(), day => 2}
        )
    ).

%% As of 2007
us_daylight_saving_start(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:nth_kday(
        2,
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:march(), day => 1}
        )
    ).

%% As of 2007
us_daylight_saving_end(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:november(), day => 1}
        )
    ).

christmas(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_gregorian:to_fixed(
        #{cal => calcalc_gregorian,
          year => Y, month => calcalc_gregorian:december(), day => 25}
    ).


advent(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:kday_nearest(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:november(), day => 30}
        )
    ).

epiphany(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_gregorian:to_fixed(
        #{cal => calcalc_gregorian,
          year => Y, month => calcalc_gregorian:january(), day => 6}
    ).

%% Some countries celebrate the epiphany on the first sunday after
%% Gregorian January 1st
epiphany_sunday(#{cal := calcalc_gregorian, year := Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian,
              year => Y, month => calcalc_gregorian:january(), day => 2}
        )
    ).

unlucky_fridays_in_range(A=#{cal := calcalc_gregorian},
                         B=#{cal := calcalc_gregorian}) ->
    unlucky_fridays_in_range(calcalc_gregorian:to_fixed(A),
                             calcalc_gregorian:to_fixed(B));
%% Should hide this clause
unlucky_fridays_in_range(FA, FB) when is_integer(FA), is_integer(FB) ->
    Fri = calcalc_day_of_week:kday_on_or_after(calcalc_day_of_week:friday(), FA),
    Date = #{day := D} = calcalc_gregorian:from_fixed(Fri),
    case {Fri >= FA andalso Fri =< FB, D} of
        {true, 13} ->
            [Date | unlucky_fridays_in_range(Fri+1, FB)];
        {true, _} ->
            unlucky_fridays_in_range(Fri+1, FB);
        {false, _} ->
            []
    end.

%% Julian holidays
eastern_orthodox_christmas(#{cal := calcalc_gregorian, year := Y}) ->
    julian_in_gregorian(calcalc_julian:december(), 25, Y).

eastern_orthodox_epiphany(#{cal := calcalc_gregorian, year := Y}) ->
    julian_in_gregorian(calcalc_julian:january(), 6, Y).

eastern_orthodox_annunciation(#{cal := calcalc_gregorian, year := Y}) ->
    julian_in_gregorian(calcalc_julian:march(), 25, Y).

eastern_orthodox_transfiguration(#{cal := calcalc_gregorian, year := Y}) ->
    julian_in_gregorian(calcalc_julian:august(), 6, Y).

%% Allows to find Julian date calendar events into a gregorian year.
%% It may return duplicate dates because far into the future (say year
%% 41104) some julian dates will happen twice within the same
%% gregorian year.
julian_in_gregorian(JulianMonth, JulianDay, GregorianYear) ->
    Jan1 = calcalc_gregorian:new_year(GregorianYear),
    #{year := Y} = calcalc_julian:from_fixed(Jan1),
    Y1 = if Y =:= -1 -> 1
          ; Y =/= -1 -> Y+1
         end,
    Date1 = calcalc_julian:to_fixed(calcalc_julian:date(
                #{year => Y, month => JulianMonth, day => JulianDay})),
    Date2 = calcalc_julian:to_fixed(calcalc_julian:date(
                #{year => Y1, month => JulianMonth, day => JulianDay})),
    list_range([Date1,Date2], calcalc_gregorian:year_range(GregorianYear)).

list_range([], _) -> [];
list_range([H|T], Range) ->
    case in_range(H, Range) of
        true -> [H | list_range(T, Range)];
        false -> list_range(T, Range)
    end.

in_range(X, {A, B}) -> A =< X andalso X =< B.
