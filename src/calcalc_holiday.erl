-module(calcalc_holiday).
-compile(export_all).
-include("calcalc.hrl").

us_independence_day(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_gregorian:to_fixed(
        #date{cal=calcalc_gregorian, year=Y, month=calcalc_gregorian:july(), day=4}
    ).

us_labor_day(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:monday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:september(), day=1}
        )
    ).

us_memorial_day(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:last_kday(
        calcalc_day_of_week:monday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:may(), day=31}
        )
    ).

us_election_day(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:tuesday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:november(), day=2}
        )
    ).

%% As of 2007
us_daylight_saving_start(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:nth_kday(
        2,
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:march(), day=1}
        )
    ).

%% As of 2007
us_daylight_saving_end(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:november(), day=1}
        )
    ).

christmas(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_gregorian:to_fixed(
        #date{cal=calcalc_gregorian, year=Y, month=calcalc_gregorian:december(), day=25}
    ).

advent(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:kday_nearest(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:november(), day=30}
        )
    ).

epiphany(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_gregorian:to_fixed(
        #date{cal=calcalc_gregorian, year=Y, month=calcalc_gregorian:january(), day=6}
    ).

%% Some countries celebrate the epiphany on the first sunday after
%% Gregorian January 1st
epiphany_sunday(#date{cal=calcalc_gregorian, year=Y}) ->
    calcalc_day_of_week:first_kday(
        calcalc_day_of_week:sunday(),
        calcalc_gregorian:to_fixed(
            #date{cal=calcalc_gregorian,
                  year=Y, month=calcalc_gregorian:january(), day=2}
        )
    ).

unlucky_fridays_in_range(A=#date{cal=calcalc_gregorian},
                         B=#date{cal=calcalc_gregorian}) ->
    unlucky_fridays_in_range(calcalc_gregorian:to_fixed(A),
                             calcalc_gregorian:to_fixed(B));
%% Should hide this clause
unlucky_fridays_in_range(FA, FB) when is_integer(FA), is_integer(FB) ->
    Fri = calcalc_day_of_week:kday_on_or_after(calcalc_day_of_week:friday(), FA),
    Date=#date{day=D} = calcalc_gregorian:from_fixed(Fri),
    case {Fri >= FA andalso Fri =< FB, D} of
        {true, 13} ->
            [Date | unlucky_fridays_in_range(Fri+1, FB)];
        {true, _} ->
            unlucky_fridays_in_range(Fri+1, FB);
        {false, _} ->
            []
    end.

