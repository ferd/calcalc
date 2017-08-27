-module(calcalc_loc).
-compile(export_all).
-import(calcalc_astro, [angle/3, deg/1, mt/1, hr/1]).

%%% Basic locations

mecca() ->
    #{latitude => angle(21,25,24), longitude => angle(39,49,24),
      elevation => mt(298), zone => hr(3)}.

jerusalem() ->
    #{latitude => deg(31.8), longitude => deg(35.2),
      elevation => mt(800), zone => hr(2)}.

urbana() ->
    #{latitude => deg(40.1), longitude => deg(-88.2),
      elevation => mt(225), zone => hr(-6)}.

greenwich() ->
    #{latitude => deg(51.4777815), longitude => deg(0),
      elevation => mt(46.9), zone => hr(0)}.

tehran() ->
    #{latitude => deg(35.68), longitude => deg(51.42),
      elevation => mt(1100), zone => hr(3.5)}.

%%% Utils
%% Tested at about 1s of precision off from the book;
urbana_winter(#{cal := calcalc_gregorian, year := Y}) ->
    urbana_winter(Y);
urbana_winter(Y) when is_integer(Y) ->
    NewYear = calcalc_gregorian:date(
        #{year => Y, month => calcalc_gregorian:january(), day => 1}
    ),
    calcalc_astro:standard_from_universal(
      calcalc_astro:solar_longitude_after(
       calcalc_astro:winter(),
       calcalc_gregorian:to_fixed(NewYear)
      ),
      urbana()
    ).

urbana_sunset(GDate = #{cal := calcalc_gregorian}) ->
    D = calcalc_gregorian:to_fixed(GDate),
    calcalc_astro:time_from_moment(calcalc_astro:sunset(D, urbana())).

tehran_midday(Date) ->
    calcalc_astro:universal_from_standard(
      calcalc_astro:midday(Date, tehran()),
      tehran()
    ).
