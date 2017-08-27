-module(calcalc_holiday).
-compile(export_all).
-include("calcalc.hrl").
-import(calcalc_math, [mod/2, floor/1]).

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

%% non-Finnish Orthodox Easter
orthodox_easter(#{cal := calcalc_gregorian, year := Y}) ->
    %% Epacts are approximation of lunar phases
    ShiftedEpact = mod(14 + 11 * mod(Y, 19), 30),
    JulianYear = if Y > 0 -> Y;
                    Y =< 0 -> Y-1
                 end,
    PaschalMoon = calcalc_julian:to_fixed(
            #{cal => calcalc_julian, year => JulianYear,
              month => calcalc_julian:april(), day => 19}
    ) - ShiftedEpact,
    calcalc_day_of_week:kday_after(calcalc_day_of_week:sunday(), PaschalMoon).

%% gregorian easter
easter(#{cal := calcalc_gregorian, year := Y}) ->
    Century = floor(Y/100)+1,
    ShiftedEpact = mod(14 + 11 * mod(Y, 19) - floor(0.75 * Century)
                       + floor((5 + 8 * Century)/25), 30),
    YMod19 = mod(Y, 19),
    AdjustedEpact = case ShiftedEpact of
        0 -> 1;
        1 when 10 < YMod19 -> 1;
        _ -> ShiftedEpact
    end,
    PaschalMoon = calcalc_gregorian:to_fixed(
            #{cal => calcalc_gregorian, year => Y,
              month => calcalc_gregorian:april(), day => 19}
    ) - AdjustedEpact,
    calcalc_day_of_week:kday_after(calcalc_day_of_week:sunday(), PaschalMoon).

%% Coptic holidays
coptic_christmas(#{cal := calcalc_gregorian, year := Y}) ->
    coptic_in_gregorian(calcalc_coptic:koiak(), 29, Y).

coptic_epiphany(#{cal := calcalc_gregorian, year := Y}) ->
    coptic_in_gregorian(calcalc_coptic:tobe(), 11, Y).

%% Islamic holidays
%% Up to 3 islamic years may end up present within a gregorian year.
%%
%% The entire month of ramadan counts here, but we return the first day.
%% Do note that islamic days/holidays start at sunset the prior evening.
%% The calculation is again not fully accurate because it may depend
%% on proclamation of religous authorities.
ramadan(#{cal := calcalc_gregorian, year := Y}) ->
    islamic_in_gregorian(calcalc_islamic:ramadan(), 1, Y).

mawlid_an_nabi(#{cal := calcalc_gregorian, year := Y}) ->
    islamic_in_gregorian(calcalc_islamic:rabi_al_awwal(), 12, Y).


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

%% Allows to find Coptic date calendar events into a gregorian year.
coptic_in_gregorian(CopticMonth, CopticDay, GregorianYear) ->
    Jan1 = calcalc_gregorian:new_year(GregorianYear),
    #{year := Y} = calcalc_coptic:from_fixed(Jan1),
    Date1 = calcalc_coptic:to_fixed(calcalc_coptic:date(
                #{year => Y, month => CopticMonth, day => CopticDay})),
    Date2 = calcalc_coptic:to_fixed(calcalc_coptic:date(
                #{year => Y+1, month => CopticMonth, day => CopticDay})),
    list_range([Date1,Date2], calcalc_gregorian:year_range(GregorianYear)).

%% Allows to find islamic date calendar events into a gregorian year.
islamic_in_gregorian(IslamicMonth, IslamicDay, GregorianYear) ->
    Jan1 = calcalc_gregorian:new_year(GregorianYear),
    #{year := Y} = calcalc_islamic:from_fixed(Jan1),
    Date1 = calcalc_islamic:to_fixed(calcalc_islamic:date(
                #{year => Y, month => IslamicMonth, day => IslamicDay})),
    Date2 = calcalc_islamic:to_fixed(calcalc_islamic:date(
                #{year => Y+1, month => IslamicMonth, day => IslamicDay})),
    Date3 = calcalc_islamic:to_fixed(calcalc_islamic:date(
                #{year => Y+2, month => IslamicMonth, day => IslamicDay})),
    list_range([Date1,Date2,Date3], calcalc_gregorian:year_range(GregorianYear)).

%% Hebrew
yom_kippur(#{cal := calcalc_gregorian, year := Y}) ->
    HY = 1 + Y - calcalc_gregorian:year_from_fixed(calcalc_hebrew:epoch()),
    calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                              month => calcalc_hebrew:tishri(), day => 10}).

passover(#{cal := calcalc_gregorian, year := Y}) ->
    HY = Y - calcalc_gregorian:year_from_fixed(calcalc_hebrew:epoch()),
    {calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                               month => calcalc_hebrew:nisan(), day => 15}),
     calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                               month => calcalc_hebrew:nisan(), day => 21})}.

purim(#{cal := calcalc_gregorian, year := Y}) ->
    HY = Y - calcalc_gregorian:year_from_fixed(calcalc_hebrew:epoch()),
    LastMonth = calcalc_hebrew:last_month(HY),
    calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                              month => LastMonth, day => 14}).

ta_anit_esther(GregorianDate) ->
    PurimDate = purim(GregorianDate),
    case {calcalc_day_of_week:from_fixed(PurimDate),
          calcalc_day_of_week:sunday()} of
        {X, X} -> PurimDate-3;
        _ -> PurimDate-1
    end.

tishah_be_av(#{cal := calcalc_gregorian, year := Y}) ->
    HY = Y - calcalc_gregorian:year_from_fixed(calcalc_hebrew:epoch()),
    AV9 = calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                                    month => calcalc_hebrew:av(), day => 9}),
    case {calcalc_day_of_week:from_fixed(AV9),
          calcalc_day_of_week:saturday()} of
        {X,X} -> AV9 + 1;
        _ -> AV9
    end.

yom_ha_zikkaron(#{cal := calcalc_gregorian, year := Y}) ->
    HY = Y - calcalc_gregorian:year_from_fixed(calcalc_hebrew:epoch()),
    Iyyar4 = calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                                       month => calcalc_hebrew:iyyar(), day => 4}),
    Iyyar4DoW = calcalc_day_of_week:from_fixed(Iyyar4),
    Thursday = calcalc_day_of_week:thursday(),
    Friday = calcalc_day_of_week:friday(),
    Sunday = calcalc_day_of_week:sunday(),
    if Iyyar4DoW =:= Thursday; Iyyar4DoW =:= Friday ->
        calcalc_day_of_week:kday_before(calcalc_day_of_week:wednesday(), Iyyar4);
       Iyyar4DoW =:= Sunday ->
        Iyyar4DoW + 1;
       true ->
        Iyyar4
    end.

%% beginning of sh'ela (request for rain) outside Israel, follows
%% the structure of the coptic calendar. In israel, it starts
%% on Marheshvan 7.
sh_ela(#{cal := calcalc_gregorian, year := Y}) ->
    hd(coptic_in_gregorian(calcalc_coptic:athor(), 26, Y)).

birkath_ha_hama(#{cal := calcalc_gregorian, year := Y}) ->
    Dates = coptic_in_gregorian(calcalc_coptic:paremotep(), 30, Y),
    case Dates of
        [] ->
            [];
        [Date|_] ->
            #{year := CY} = calcalc_coptic:from_fixed(Date),
            case mod(CY, 28) of
                17 -> Dates;
                _ -> []
            end
    end.

%% Calculates the hebrew birthday of someone for a given hebrew year --
%% for birthdays, gregorian and hebrew years don't coincide properly, so
%% multiple values may be returned when calculating with a gregorian date.
-spec hebrew_birthday(BirthDate, CurrentHebrewYear | GregorianYear) -> Date when
    BirthDate :: calcalc:date(),
    CurrentHebrewYear :: calcalc:date(),
    GregorianYear :: calcalc:date(),
    Date :: calcalc:fixed().
hebrew_birthday(#{cal := calcalc_hebrew, year := BY, month := BM, day := BD},
                #{cal := calcalc_hebrew, year := HY}) ->
    case {BM, calcalc_hebrew:last_month(BY)} of
        {X,X} ->
            calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                                      month => calcalc_hebrew:last_month(HY),
                                      day => BD});
        _ ->
            calcalc_hebrew:to_fixed(#{cal => calcalc_hebrew, year => HY,
                                      month => BM, day => 1}) + BD - 1
    end;
hebrew_birthday(BirthDate,
                #{cal := calcalc_gregorian, year := GregorianYear}) ->
    Jan1 = calcalc_gregorian:new_year(GregorianYear),
    HebrewDate = #{year := HY} = calcalc_hebrew:from_fixed(Jan1),
    Date1 = hebrew_birthday(BirthDate, HebrewDate),
    Date2 = hebrew_birthday(BirthDate, HebrewDate#{year => HY+1}),
    list_range([Date1, Date2], calcalc_gregorian:year_range(GregorianYear)).

%% Bali
kajeng_keliwon(#{cal := calcalc_gregorian, year := GregorianYear}) ->
    Year = calcalc_gregorian:year_range(GregorianYear),
    Delta = calcalc_bali:day(0),
    positions_in_range(9, 15, Delta, Year).

tumpek(#{cal := calcalc_gregorian, year := GregorianYear}) ->
    Year = calcalc_gregorian:year_range(GregorianYear),
    Delta = calcalc_bali:day(0),
    positions_in_range(14, 35, Delta, Year).

%% Persian
naw_ruz(#{cal := calcalc_gregorian, year := GYear}) -> % new year!
    PYear = 1+GYear-calcalc_gregorian:year_from_fixed(calcalc_persian:epoch()),
    Y = case PYear =< 0 of
            true -> PYear - 1;
            false -> PYear
        end,
    calcalc_persian:to_fixed(
      calcalc_persian:date(#{year => Y, month => 1, day => 1})
    ).

%%%%%%%%%%%%%
%%% Utils %%%
%%%%%%%%%%%%%
list_range([], _) -> [];
list_range([H|T], Range) ->
    case in_range(H, Range) of
        true -> [H | list_range(T, Range)];
        false -> list_range(T, Range)
    end.

in_range(X, {A, B}) -> A =< X andalso X =< B.

positions_in_range(N, C, Delta, {A,B}) ->
    Pos = A + mod(N - A - Delta - 1, C),
    if Pos > B  -> []
    ;  Pos =< B -> [Pos | positions_in_range(N, C, Delta, {Pos+1, B})]
    end.

