-module(calcalc_astro).
-import(calcalc_math, [floor/1, ceil/1, amod/2, mod/2, poly/2]).
-import(math, [pi/0]).
-compile({no_auto_import,[ceil/1, floor/1]}).
-compile(export_all).
-compile({inline, [deg/1, mt/1, hr/1, angle/3]}).

-type angle() :: number().
-type meters() :: number().
-type hours() :: number().
-type moment() :: float().
-type season() :: angle().
-type phase() :: angle().
-type location() ::  #{latitude := angle(), longitude := angle(),
                       elevation := meters(), zone := hours()}.

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

-spec direction(location(), location()) -> angle().
direction(_Locale = #{latitude := Phi, longitude := Psi},
          _Focus = #{latitude := PhiPrime, longitude := PsiPrime}) ->
    Y = sin(PsiPrime - Psi),
    X = cos(Phi) * tan(PhiPrime)
      - sin(Phi) * cos(Psi - PsiPrime),
    Deg90 = deg(90),
    Degm90 = deg(-90),
    if (X == 0 andalso Y == 0) orelse (PhiPrime == Deg90) -> deg(0)
     ; PhiPrime == Degm90 -> deg(180)
     ; true -> atan(Y, X)
    end.

%%%%%%%%%%%%%%%%%%
%%% TIME UTILS %%%
%%%%%%%%%%%%%%%%%%
zone_from_longitude(#{longitude := Angle}) ->
    zone_from_longitude(Angle);
zone_from_longitude(Angle) when is_number(Angle) ->
    Angle / deg(360).

-spec universal_from_local(moment(), location()) -> moment().
universal_from_local(T, #{longitude := Longitude}) ->
    T - zone_from_longitude(Longitude).

local_from_universal(T, #{longitude := Longitude}) ->
    T + zone_from_longitude(Longitude).

standard_from_universal(T, #{zone := Zone}) ->
    T + Zone.

universal_from_standard(T, #{zone := Zone}) ->
    T - Zone.

standard_from_local(T, Locale) ->
    standard_from_universal(universal_from_local(T, Locale), Locale).

ephemeris_correction(T) ->
    %% Page 178, double-check them numbers.
    Year = calcalc_gregorian:year_from_fixed(floor(T)),
    Jan = calcalc_gregorian:january(),
    Jul = calcalc_gregorian:july(),
    C = (1/36525) * calcalc_gregorian:date_difference(
        calcalc_gregorian:date(#{year => 1900, month => Jan, day => 1}),
        calcalc_gregorian:date(#{year => Year, month => Jul, day => 1})
    ),
    X = hr(12) + calcalc_gregorian:date_difference(
        calcalc_gregorian:date(#{year => 1810, month => Jan, day => 1}),
        calcalc_gregorian:date(#{year => Year, month => Jan, day => 1})
    ),
    if 1988 =< Year, Year =< 2019 ->
        (1/86400) * (Year-1933);
       1900 =< Year, Year =< 1987 ->
        poly(C, [-0.00002, 0.000297, 0.025184,
                 -0.181133, 0.553040, -0.861938,
                 0.677066, -0.212591]);
       1800 =< Year, Year =< 1899 ->
        poly(C, [-0.000009, 0.003844, 0.083563,
                 0.865736, 4.867575, 15.845535,
                 31.332267, 38.291999, 28.316289,
                 11.636204, 2.043794]);
       1700 =< Year, Year =< 1799 ->
        (1/86400) * poly(Year - 1700,
                         [8.118780842, -0.005092142,
                          0.003336121, -0.0000266484]);
       1600 =< Year, Year =< 1699 ->
        (1/86400) * poly(Year - 1600, [196.58333, -4.0675, 0.0219167]);
       true ->
        (1/86400) * ((X*X)/41048480 - 15)
    end.

dynamical_from_universal(T) ->
    T + ephemeris_correction(T).

universal_from_dynamical(T) ->
    T - ephemeris_correction(T).

julian_centuries(T) ->
    (1/36525) * (dynamical_from_universal(T) - j2000()).

j2000() ->
    hr(12) + calcalc_gregorian:new_year(2000).

equation_of_time(T) ->
    C = julian_centuries(T),
    Lambda = poly(C, [deg(X) || X <- [280.46645, 36000.76983,
                                      0.0003032]]),
    Anomaly = poly(C, [deg(X) || X <- [357.52910, 35999.05030,
                                       -0.0001559, -0.00000048]]),
    Eccentricity = poly(C, [0.016708617, -0.000042037,
                            -0.0000001236]),
    Epsilon = obliquity(T),
    Y = math:pow(tan(Epsilon/2), 2),
    Equation = (1 / (2*pi()))
             * (Y * sin(2*Lambda)
               + (-2 * Eccentricity * sin(Anomaly))
               + (4 * Eccentricity * Y * sin(Anomaly)
                 * cos(2*Lambda))
               + (-0.5 * Y * Y * sin(4*Lambda))
               + (-1.25 * Eccentricity * Eccentricity
                 * sin(2*Anomaly))),
    signum(Equation) * min(abs(Equation), hr(12)).

apparent_from_local(T, Locale) ->
    T + equation_of_time(universal_from_local(T, Locale)).

local_from_apparent(T, Locale) ->
    T - equation_of_time(universal_from_local(T, Locale)).

midnight(Date, Locale) ->
    standard_from_local(local_from_apparent(Date, Locale), Locale).

midday(Date, Locale) ->
    standard_from_local(local_from_apparent(Date + hr(12), Locale), Locale).

sidereal_from_moment(T) ->
    C =  (T-j2000()) / 36525,
    mod(poly(C, [deg(X) || X <- [280.46061837,
                                 36525 * 360.98564736629,
                                 0.000387933, -1/38710000]]),
        360).

%%%%%%%%%%%%
%%% DATE %%%
%%%%%%%%%%%%

obliquity(T) ->
    C = julian_centuries(T),
    angle(23, 26, 21.448) + poly(C, [angle(0,0,-46.8150),
                                     angle(0,0,-0.00059),
                                     angle(0,0,0.001813)]).

-spec declination(moment(), angle(), angle()) -> angle().
declination(T, Beta, Lambda) ->
    Epsilon = obliquity(T),
    asin(sin(Beta) * cos(Epsilon) + cos(Beta) * sin(Epsilon) * sin(Lambda)).

-spec right_ascension(moment(), angle(), angle()) -> angle().
right_ascension(T, Beta, Lambda) ->
    Epsilon = obliquity(T),
    atan(sin(Lambda) * cos(Epsilon) - tan(Beta) * sin(Epsilon),
         cos(Lambda)).

mean_tropical_year() -> 365.242189.

mean_sidereal_year() -> 365.25636.

-spec solar_longitude(moment()) -> season().
solar_longitude(T) ->
    C = julian_centuries(T),
    Coefficients = [403406, 195207, 119433, 112392, 3891, 2819, 1721,
                    660, 350, 334, 314, 268, 242, 234, 158, 132, 129, 114,
                    99, 93, 86, 78, 72, 68, 64, 46, 38, 37, 32, 29, 28, 27, 27,
                    25, 24, 21, 21, 20, 18, 17, 14, 13, 13, 13, 12, 10, 10, 10,
                    10],
    Multipliers = [0.9287892, 35999.1376958, 35999.4089666,
                   35998.7287385, 71998.20261, 71998.4403,
                   36000.35726, 71997.4812, 32964.4678,
                   -19.4410, 445267.1117, 45036.8840, 3.1008,
                   22518.4434, -19.9739, 65928.9345,
                   9038.0293, 3034.7684, 33718.148, 3034.448,
                   -2280.773, 29929.992, 31556.493, 149.588,
                   9037.750, 107997.405, -4444.176, 151.771,
                   67555.316, 31556.080, -4561.540,
                   107996.706, 1221.655, 62894.167,
                   31437.369, 14578.298, -31931.757,
                   34777.243, 1221.999, 62894.511,
                   -4442.039, 107997.909, 119.066, 16859.071,
                   -4.578, 26895.292, -39.127, 12297.536,
                   90073.778],
    Addends = [270.54861, 340.19128, 63.91854, 331.26220,
               317.843, 86.631, 240.052, 310.26, 247.23,
               260.87, 297.82, 343.14, 166.79, 81.53,
               3.50, 132.75, 182.95, 162.03, 29.8,
               266.4, 249.2, 157.6, 257.8, 185.1, 69.9,
               8.0, 197.1, 250.4, 65.3, 162.7, 341.5,
               291.6, 98.5, 146.7, 110.0, 5.2, 342.6,
               230.9, 256.1, 45.3, 242.9, 115.2, 151.8,
               285.3, 53.3, 126.6, 205.7, 85.9,
               146.1],
    Lambda = deg(282.7771834) + deg(36000.76953744) * C
           + deg(0.000005729577951308232)
           * calcalc_math:sigma(
               [Coefficients, Addends, Multipliers],
               fun([X,Y,Z]) -> X * sin(Y + Z * C) end
             ),
    mod(Lambda + aberration(T) + nutation(T), 360).

-spec aberration(moment()) -> angle().
aberration(T) ->
    C = julian_centuries(T),
    deg(0.0000974) * cos(deg(177.63) + deg(35999.01848)*C) - deg(0.005575).

-spec nutation(moment()) -> angle().
nutation(T) ->
    C = julian_centuries(T),
    A = poly(C, [deg(X) || X <- [124.90, -1934.134, 0.002063]]),
    B = poly(C, [deg(X) || X <- [201.11, 72001.5377, 0.00057]]),
    deg(-0.004778) * sin(A) + deg(-0.0003667) * sin(B).

-spec solar_longitude_after(season(), moment()) -> moment().
solar_longitude_after(Lambda, T) ->
    Rate = mean_tropical_year() / deg(360),
    Tau = T + Rate * mod(Lambda - solar_longitude(T), 360), % estimate within 5 days
    A = max(T, Tau-5), % at or after T
    B = Tau+5,
    invert_angular(fun solar_longitude/1, Lambda, A, B).

spring() -> deg(0).
summer() -> deg(90).
autumn() -> deg(180).
winter() -> deg(270).

%% Tested at about 1s of precision off from the book;
%% TODO: see if the problem is in bad copy of some values
%%       or floating point number variations between Erlang and CL.
urbana_winter(#{cal := calcalc_gregorian, year := Y}) ->
    urbana_winter(Y);
urbana_winter(Y) when is_integer(Y) ->
    NewYear = calcalc_gregorian:date(
        #{year => Y, month => calcalc_gregorian:january(), day => 1}
    ),
    standard_from_universal(
      solar_longitude_after( winter(), calcalc_gregorian:to_fixed(NewYear)),
      urbana()
    ).

precession(T) ->
    C = julian_centuries(T),
    Eta = mod(poly(C, [secs(47.0029), secs(-0.03302), secs(0.000060)]), 360),
    P1 = mod(poly(C, [deg(174.876384), secs(-869.8089), secs(0.03536)]), 360),
    P2 = mod(poly(C, [secs(5029.0966), secs(1.11113), secs(0.000006)]), 360),
    A = cos(Eta) * sin(P1),
    B = cos(P1),
    Arg = atan(A,B),
    mod(P2+P1-Arg, 360).

sidereal_solar_longitude(T) ->
    mod(solar_longitude(T) - precession(T) + sidereal_start(), 360).

sidereal_start() ->
    deg(156.13605090692624).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Solar Calendars %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec estimate_prior_solar_longitude(season(), moment()) -> moment().
estimate_prior_solar_longitude(Lambda, T) ->
    Rate = mean_tropical_year() / deg(360),
    Tau = T - Rate * mod(solar_longitude(T) - Lambda, 360),
    Delta = mod(solar_longitude(Tau) - Lambda + deg(180), 360) - deg(180),
    min(T, Tau - Rate * Delta).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Month Calculations %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
mean_synodic_month() -> 29.530588861. % as per errata

-spec nth_new_moon(integer()) -> moment().
nth_new_moon(N) ->
    %% Rates corrected from the errata
    N0 = 24724, % month,s since RD 0 until j2000
    K = N - N0, % months since j2000
    C = K / 1236.85,
    Approx = j2000()
           + poly(C, [5.09766, mean_synodic_month() * 1236.85,
                      0.0001437, -0.000000150, 0.00000000073]),
    E = poly(C, [1, -0.002516, -0.0000074]),
    SolarAnomaly = poly(C, [deg(X) || X <- [2.5534,
                                            1236.85 * 29.10535670,
                                            -0.000-0014
                                            -0.00000011]]),
    LunarAnomaly = poly(C, [deg(X) || X <- [201.5643,
                                            385.81693528 * 1236.85,
                                            0.0107582,
                                            0.00001238,
                                            -0.000000058]]),
    MoonArgument = poly(C, [deg(X) || X <- [160.7108,
                                            390.67050284 * 1236.85,
                                            -0.0016118,
                                            -0.00000227,
                                            0.000000011]]),
    Omega = poly(C, [deg(X) || X <- [124.7746,
                                     -1.56375588 * 1236.85,
                                     0.0020672, 0.00000215]]),
    SineCoeff = [-0.40720, 0.17241, 0.01608, 0.01039, 0.00739,
                 -0.00514, 0.00208, -0.00111, -0.00057, 0.00056,
                 -0.00042, 0.00042, 0.00038, -0.00024, -0.00007,
                 0.00004, 0.00004, 0.00003, 0.00003, -0.00003,
                 0.00003, -0.00002, -0.00002, 0.00002],
    EFactor = [0, 1, 0, 0, 1, 1, 2, 0, 0, 1, 0, 1,
               1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    SolarCoeff = [0, 1, 0, 0, -1, 1, 2, 0, 0, 1, 0, 1,
                  1, -1, 2, 0, 3, 1, 0, 1, -1, -1, 1, 0],
    LunarCoeff = [1, 0, 2, 0, 1, 1, 0, 1, 1, 2, 3, 0,
                  0, 2, 1, 2, 0, 1, 2, 1, 1, 1, 3, 4],
    MoonCoeff = [0, 0, 0, 2, 0, 0, 0, -2, 2, 0, 0, 2,
                 -2, 0, 0, -2, 0, -2, 2, 2, 2, -2, 0, 0],
    Correction = deg(-0.00017) * sin(Omega)
               + calcalc_math:sigma(
                   [SineCoeff, EFactor, SolarCoeff, LunarCoeff, MoonCoeff],
                   fun([V,W,X,Y,Z]) ->
                       V * math:pow(E, W)
                       * sin(X*SolarAnomaly + Y*LunarAnomaly + Z*MoonArgument)
                   end),
    Extra = deg(0.000325)
          * sin(poly(C, [deg(X) || X <- [299.77, 132.8475848, -0.009173]])),
    AddConst = [251.88, 251.83, 349.42, 84.66, 141.74, 207.14, 154.84,
                34.52, 207.19, 291.34, 161.72, 239.56, 331.55],
    AddCoeff = [0.016321, 26.651886, 36.412478, 18.206239, 53.303771,
                2.453732, 7.306860, 27.261239, 0.121824, 1.844379,
                24.198154, 25.513099, 3.592518],
    AddFactor = [0.000165, 0.000164, 0.000126, 0.000110, 0.000062, 0.000060,
                 0.000056, 0.000047, 0.000042, 0.000040, 0.000037, 0.000035,
                 0.000023],
    Additional = calcalc_math:sigma([AddConst, AddCoeff, AddFactor],
                                    fun(I,J,L) -> L * sin(I + J * K) end),
    universal_from_dynamical(Approx + Correction + Extra + Additional).

new_moon_before(T) ->
    T0 = nth_new_moon(0),
    Phi = lunar_phase(T),
    N = round((T-T0) / mean_synodic_month() - Phi/deg(360)), 
    nth_new_moon(calcalc_math:final(1-N, fun(K) -> nth_new_moon(K) < T end)).

new_moon_at_or_after(T) ->
    T0 = nth_new_moon(0),
    Phi = lunar_phase(T),
    N = round((T-T0) / mean_synodic_month() - Phi/deg(360)), 
    nth_new_moon(calcalc_math:next(N, fun(K) -> nth_new_moon(K) >= T end)).

-spec lunar_phase(moment()) -> phase().
lunar_longitude(T) ->
    C = julian_centuries(T),
    L = mean_lunar_longitude(C),
    D = lunar_elongation(C),
    M = solar_anomaly(C),
    MPrime = lunar_anomaly(C),
    F = moon_node(C),
    E = poly(C, [1, -0.002516, -0.0000074]),
    ArgsLunarElong = [0,2,2,0,0,0,2,2,2,2,0,1,0,2,0,0,4,0,4,2,2,1,
                      1,2,2,4,2,0,2,2,1,2,0,0,2,2,2,4,0,3,2,4,0,2,
                      2,2,4,0,4,1,2,0,1,3,4,2,0,1,2],
    ArgsSolarAnom = [0,0,0,0,1,0,0,-1,0,-1,1,0,1,0,0,0,0,0,0,1,1,
                     0,1,-1,0,0,0,1,0,-1,0,-2,1,2,-2,0,0,-1,0,0,1,
                     -1,2,2,1,-1,0,0,-1,0,1,0,1,0,0,-1,2,1,0],
    ArgsLunarAnom = [1,-1,0,2,0,0,-2,-1,1,0,-1,0,1,0,1,1,-1,3,-2,
                     -1,0,-1,0,1,2,0,-3,-2,-1,-2,1,0,2,0,-1,1,0,
                     -1,2,-1,1,-2,-1,-1,-2,0,1,4,0,-2,0,2,1,-2,-3,
                     2,1,-1,3],
    ArgsMoonNode = [0,0,0,0,0,2,0,0,0,0,0,0,0,-2,2,-2,0,0,0,0,0,
                    0,0,0,0,0,0,0,2,0,0,0,0,0,0,-2,2,0,2,0,0,0,0,
                    0,0,-2,0,0,0,0,-2,-2,0,0,0,0,0,0,0],
    SineCoeff = [6288774, 1274027, 658314, 213618, -185116, -114332,
                 58793, 57066, 53322, 45758, -40923, -34720, -30383,
                 15327, -12528, 10980, 10675, 10034, 8548, -7888,
                 -6766, -5163, 4987, 4036, 3994, 3861, 3665, -2689,
                 -2602, 2390, -2348, 2236, -2120, -2069, 2048, -1773,
                 -1595, 1215, -1110, -892, -810, 759, -713, -700, 691,
                 596, 549, 537, 520, -487, -399, -381, 351, -340, 330,
                 327, -323, 299, 294],
    Correction = deg(1/1000000) * calcalc_math:sigma(
        [SineCoeff, ArgsLunarElong, ArgsSolarAnom, ArgsLunarAnom, ArgsMoonNode],
        fun([V,W,X,Y,Z]) ->
            V * math:pow(E, abs(X)) * sin(W*D + X*M + Y*MPrime + Z*F)
        end
    ),
    Venus = deg(3958/1000000) * sin(deg(119.75) + C * deg(131.849)),
    Jupiter = deg(318/1000000) * sin(deg(53.09) + C * deg(479264.29)),
    FlatEarth = deg(1962/1000000) * sin(L - F),
    mod(L + Correction + Venus + Jupiter + FlatEarth + nutation(T), 360).

mean_lunar_longitude(C) ->
    degrees(poly(C, [deg(X) || X <- [218.3164477, 481267.88123421,
                                     -0.0015786, 1/538841, -1/65194000]])).

lunar_elongation(C) ->
    degrees(poly(C, [deg(X) || X <- [297.8501921, 445267.1114034,
                                     -0.0018819, 1/545868, -1/113065000]])).

solar_anomaly(C) ->
    degrees(poly(C, [deg(X) || X <- [357.5291092, 35999.0502909,
                                     -0.0001536, 1/24490000]])).

lunar_anomaly(C) ->
    degrees(poly(C, [deg(X) || X <- [134.9633964, 477198.8675055,
                                     0.0087414, 1/69699, -1/14712000]])).

moon_node(C) ->
    degrees(poly(C, [deg(X) || X <- [93.2720950, 483202.0175233,
                                     -0.0036539, -1/3526000, 1/863310000]])).

%% From errata
lunar_node(D) ->
    mod(moon_node(julian_centuries(D)) + deg(90), deg(180)) + deg(90).

sidereal_lunar_longitude(T) ->
    mod(lunar_longitude(T) - precession(T) + sidereal_start(), 360).
%%

lunar_phase(T) ->
    Phi = mod(lunar_longitude(T) - solar_longitude(T), 360),
    T0 = nth_new_moon(0),
    N = round((T-T0)/mean_synodic_month()),
    PhiPrime = deg(360) * mod((T - nth_new_moon(N)) / mean_synodic_month(), 1),
    case abs(Phi - PhiPrime) > deg(180) of
        true -> PhiPrime;
        false -> Phi
    end.

-spec lunar_phase_at_or_before(phase(), moment()) -> moment().
lunar_phase_at_or_before(Phi, T) ->
    Tau = T - mean_synodic_month() * (1/deg(360)) * mod(lunar_phase(T)-Phi,360),
    A = Tau - 2,
    B = min(T, Tau+2),
    invert_angular(fun lunar_phase/1, Phi, A, B).

lunar_phase_at_or_after(Phi, T) ->
    Tau = T + mean_synodic_month() * (1/deg(360)) * mod(Phi-lunar_phase(T),360),
    A = max(T, Tau-2),
    B = Tau+2,
    invert_angular(fun lunar_phase/1, Phi, A, B).

new() -> deg(0).
full() -> deg(180).
first_quarter() -> deg(90).
last_quarter() -> deg(270).

-spec lunar_latitude(moment()) -> angle().
lunar_latitude(T) ->
    C = julian_centuries(T),
    L = mean_lunar_longitude(C),
    D = lunar_elongation(C),
    M = solar_anomaly(C),
    MPrime = lunar_anomaly(C),
    F = moon_node(C),
    E = poly(C, [1, -0.002516, -0.0000074]),
    LunarElongation = [0,0,0,2,2,2,2,0,2,0,2,2,2,2,2,2,2,0,4,0,0,0,
                       1,0,0,0,1,0,4,4,0,4,2,2,2,2,0,2,2,2,2,4,2,2,
                       0,2,1,1,0,2,1,2,0,4,4,1,4,1,4,2],
    SolarAnomaly = [0,0,0,0,0,0,0,0,0,0,-1,0,0,1,-1,-1,-1,1,0,1,
                    0,1,0,1,1,1,0,0,0,0,0,0,0,0,-1,0,0,0,0,1,1,
                    0,-1,-2,0,1,1,1,1,1,0,-1,1,0,-1,0,0,0,-1,-2],
    LunarAnomaly = [0,1,1,0,-1,-1,0,2,1,2,0,-2,1,0,-1,0,-1,-1,-1,
                    0,0,-1,0,1,1,0,0,3,0,-1,1,-2,0,2,1,-2,3,2,-3,
                    -1,0,0,1,0,1,1,0,0,-2,-1,1,-2,2,-2,-1,1,1,-2,
                    0,0],
    MoonNode = [1,1,-1,-1,1,-1,1,1,-1,-1,-1,-1,1,-1,1,1,-1,-1,
                -1,1,3,1,1,1,-1,-1,-1,1,-1,1,-3,1,-3,-1,-1,1,
                -1,1,-1,1,1,1,1,-1,3,-1,-1,1,-1,-1,1,-1,1,-1,
                -1,-1,-1,-1,-1,1],
    SineCoeff = [5128122, 280602, 277693, 173237, 55413, 46271, 32573,
                 17198, 9266, 8822, 8216, 4324, 4200, -3359, 2463, 2211,
                 2065, -1870, 1828, -1794, -1749, -1565, -1491, -1475,
                 -1410, -1344, -1335, 1107, 1021, 833, 777, 671, 607,
                 596, 491, -451, 439, 422, 421, -366, -351, 331, 315,
                 302, -283, -229, 223, 223, -220, -220, -185, 181,
                 -177, 176, 166, -164, 132, -119, 115, 107],
    Beta = 1/1000000 * calcalc_math:sigma(
        [SineCoeff, LunarElongation, SolarAnomaly, LunarAnomaly, MoonNode],
        fun([V,W,X,Y,Z]) ->
            V * math:pow(E,abs(X)) * sin(W*D + X*M + Y*MPrime + Z*F)
        end
    ),
    Venus = deg(175/1000000)
          * sin(deg(119.75) + C*deg(131.849) + F)
          * sin(deg(119.75) + C*deg(131.849) - F),
    FlatEarth = deg(-2235/1000000) * sin(L)
              + deg(127/1000000) * sin(L - MPrime)
              + deg(-115/1000000) * sin(L + MPrime),
    Extra = deg(382/1000000) * sin(deg(313.45) + C*deg(481266.484)),
    Beta + Venus + FlatEarth + Extra.

-spec lunar_altitude(moment(), location()) -> angle().
lunar_altitude(T, #{latitude := Phi, longitude := Psi}) ->
    Lambda = lunar_longitude(T),
    Beta = lunar_latitude(T),
    Alpha = right_ascension(T, Beta, Lambda),
    Delta = declination(T, Beta, Lambda),
    Theta = sidereal_from_moment(T),
    H = mod(Theta + Psi - Alpha, 360),
    Altitude = asin(sin(Phi) * sin(Delta) + cos(Phi) * cos(Delta) * cos(H)),
    mod(Altitude + deg(180), 360) - deg(180).

-spec lunar_distance(moment()) -> meters().
lunar_distance(T) ->
    C = julian_centuries(T),
    D = lunar_elongation(C),
    M = solar_anomaly(C),
    MPrime = lunar_anomaly(C),
    F = moon_node(C),
    E = poly(C, [1, -0.002516, -0.0000074]),
    LunarElongation = [0,2,2,0,0,0,2,2,2,2,0,1,0,2,0,0,4,0,4,2,2,1,
                       1,2,2,4,2,0,2,2,1,2,0,0,2,2,2,4,0,3,2,4,0,2,
                       2,2,4,0,4,1,2,0,1,3,4,2,0,1,2,2],
    SolarAnomaly = [0,0,0,0,1,0,0,-1,0,-1,1,0,1,0,0,0,0,0,0,1,1,
                    0,1,-1,0,0,0,1,0,-1,0,-2,1,2,-2,0,0,-1,0,0,1,
                    -1,2,2,1,-1,0,0,-1,0,1,0,1,0,0,-1,2,1,0,0],
    LunarAnomaly = [1,-1,0,2,0,0,-2,-1,1,0,-1,0,1,0,1,1,-1,3,-2,
                    -1,0,-1,0,1,2,0,-3,-2,-1,-2,1,0,2,0,-1,1,0,
                    -1,2,-1,1,-2,-1,-1,-2,0,1,4,0,-2,0,2,1,-2,-3,
                    2,1,-1,3,-1],
    MoonNode = [0,0,0,0,0,2,0,0,0,0,0,0,0,-2,2,-2,0,0,0,0,0,
                0,0,0,0,0,0,0,2,0,0,0,0,0,0,-2,2,0,2,0,0,0,0,
                0,0,-2,0,0,0,0,-2,-2,0,0,0,0,0,0,0,-2],
    CosCoeff = [-20905355,-3699111,-2955968,-569925,48888,-3149,
                246158,-152138,-170733,-204586,-129620,108743,
                104755,10321,0,79661,-34782,-23210,-21636,24208,
                30824,-8379,-16675,-12831,-10445,-11650,14403,
                -7003,0,10056,6322,-9884,5751,0,-4950,4130,0,
                -3958,0,3258,2616,-1897,-2117,2354,0,0,-1423,
                -1117,-1571,-1739,0,-4421,0,0,0,0,1165,0,0,
                8752],
    Correction = calcalc_math:sigma(
        [CosCoeff, LunarElongation, SolarAnomaly, LunarAnomaly, MoonNode],
        fun([V,W,X,Y,Z]) ->
            V * math:pow(E, abs(X)) * cos(W*D + X*M + Y*MPrime + Z*F)
        end),
    mt(385000560) + Correction.

lunar_parallax(T, Locale) ->
    Geo = lunar_altitude(T, Locale),
    Delta = lunar_distance(T),
    Alt = mt(6738140)/Delta,
    Arg = Alt * cos(Geo),
    asin(Arg).

topocentric_lunar_altitude(T, Locale) ->
    lunar_altitude(T, Locale) - lunar_parallax(T, Locale).

%%%%%%%%%%%%%%%%%%%
%%% Time of day %%%
%%%%%%%%%%%%%%%%%%%

%% in book, `Horizon' = `early?', where `true == east',
%% and `false == west'.
-spec approx_moment_of_depression(moment(), location(), angle(), east|west) -> moment().
approx_moment_of_depression(T, Locale, Alpha, Horizon) ->
    Try = sin_offset(T, Locale, Alpha),
    Date = fixed_from_moment(T),
    Alt = if Alpha >= 0, Horizon == east -> Date
           ; Alpha >= 0, Horizon == west -> Date+1
           ; Alpha < 0 -> Date+hr(12)
          end,
    Value = case abs(Try) > 1 of
                true -> sin_offset(Alt, Locale, Alpha);
                false -> Try
            end,
    case abs(Value) =< 1 of
        true ->
            local_from_apparent(
              Date + hr(12)
              + case Horizon of
                  east -> -1;
                  west -> 1
                end * (mod(hr(12) + asin(Value)/deg(360), 1) - hr(6)),
              Locale);
        false ->
            undefined
    end.

-spec sin_offset(moment(), location(), angle()) -> number().
sin_offset(T, Locale = #{latitude := Phi}, Alpha) ->
    T2 = universal_from_local(T, Locale),
    Delta = declination(T, deg(0), solar_longitude(T2)),
    tan(Phi) * tan(Delta) + sin(Alpha)/(cos(Delta)*cos(Phi)).

moment_of_depression(Approx, Locale, Alpha, Horizon) ->
    T = approx_moment_of_depression(Approx, Locale, Alpha, Horizon),
    Secs = sec(30),
    if T == undefined -> undefined
     ; abs(Approx-T) < Secs -> T
     ; true -> moment_of_depression(T, Locale, Alpha, Horizon)
    end.

morning() -> east.
evening() -> west.

dawn(Date, Locale, Alpha) ->
    case moment_of_depression(Date+hr(6), Locale, Alpha, morning()) of
        undefined -> undefined;
        Result -> standard_from_local(Result, Locale)
    end.

dusk(Date, Locale, Alpha) ->
    case moment_of_depression(Date+hr(18), Locale, Alpha, evening()) of
        undefined -> undefined;
        Result -> standard_from_local(Result, Locale)
    end.

sunrise(Date, Locale) ->
    Alpha = find_horizon(Locale),
    dawn(Date, Locale, Alpha).

sunset(Date, Locale) ->
    Alpha = find_horizon(Locale),
    dusk(Date, Locale, Alpha).

find_horizon(#{elevation := E}) ->
    H = max(mt(0), E),
    R = mt(6.372e6),
    Dip = acos(R/(R+H)),
    angle(0,50,0) + Dip + secs(19) * math:sqrt(H).

urbana_sunset(GDate = #{cal := calcalc_gregorian}) ->
    D = calcalc_gregorian:to_fixed(GDate),
    time_from_moment(sunset(D, urbana())).


standard_from_sundial(T, Locale) ->
    Date = fixed_from_moment(T),
    Hour = 24 * mod(T, 1),
    H = if 6 =< Hour, Hour =< 18 -> daytime_temporal_hour(Date, Locale)
         ; Hour < 6 -> nighttime_temporal_hour(Date-1, Locale)
         ; Hour > 18 -> nighttime_temporal_hour(Date, Locale)
        end,
    if H == undefined -> undefined
     ; 6 =< Hour, Hour =< 18 -> sunrise(Date, Locale) + (Hour-6) * H
     ; Hour < 6 -> sunset(Date-1, Locale) + (Hour+6) * H
     ; Hour > 18 -> sunset(Date, Locale) + (Hour - 18) * H
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Holidays and special times %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% move to another place or something
jewish_sabbath_ends(Date, Locale) ->
    dusk(Date, Locale, angle(7,5,0)).

jewish_dusk(Date, Locale) ->
    dusk(Date, Locale, angle(4,40,0)).

daytime_temporal_hour(Date, Locale) ->
    case {sunrise(Date, Locale), sunset(Date, Locale)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {Sunrise, Sunset} -> (1/12) * (Sunset - Sunrise)
    end.

nighttime_temporal_hour(Date, Locale) ->
    case {sunrise(Date+1, Locale), sunset(Date, Locale)} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {Sunrise, Sunset} -> (1/12) * (Sunrise - Sunset)
    end.

jewish_morning_end(Date, Locale) ->
    standard_from_sundial(Date+hr(10), Locale).

asr(Date, Locale = #{latitude := Phi}) ->
    Noon = universal_from_standard(midday(Date, Locale), Locale),
    Delta = declination(Noon, deg(0), solar_longitude(Noon)),
    Altitude = Delta - Phi - deg(90),
    H = atan(tan(Altitude), 2*tan(Altitude)+1), % Shafii removes the *2
    dusk(Date, Locale, -H).

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%

-spec fixed_from_moment(moment()) -> calcalc:fixed().
fixed_from_moment(Moment) -> floor(Moment).

%% Time of day
-spec time_from_moment(moment()) -> calcalc:time().
time_from_moment(T) -> mod(T,1).

degrees(Theta) ->
    mod(Theta, 360).

radians_from_degrees(Theta) ->
    degrees(Theta) * pi() * (1/180).

degrees_from_radians(Theta) ->
    degrees((Theta / pi()) / (1/180)).

radians_to_degrees(Theta) ->
    degrees_from_radians(Theta).

degrees_to_radians(Theta) ->
    radians_from_degrees(Theta).

sin(Theta) ->
    math:sin(radians_from_degrees(Theta)).

cos(Theta) ->
    math:cos(radians_from_degrees(Theta)).

tan(Theta) ->
    math:tan(radians_from_degrees(Theta)).

atan(Y, X) ->
    mod(if X == 0 andalso Y /= 0 -> signum(Y) * deg(90.0);
           X == 0 andalso Y == 0 -> undefined;
           X >= 0 -> degrees_from_radians(math:atan(Y/X));
           X < 0  -> degrees_from_radians(math:atan(Y/X)) + deg(180.0)
        end, 360).

asin(X) ->
    degrees_from_radians(math:asin(X)).

acos(X) ->
    degrees_from_radians(math:acos(X)).

signum(N) when N == 0 -> 0;
signum(N) when N < 0 -> -1;
signum(N) when N > 0 -> 1.

hr(X) -> X / 24.
sec(X) -> ((X / 24) / 60) / 60.
mt(X) -> X.  % meters
deg(X) -> X. % degrees
secs(X) -> X / 3600.
angle(D, M, S) -> D + (M + S/60)/60.
degrees_minutes_seconds(D, M, S) -> {D, M, S}.

-spec invert_angular(fun((number()) -> angle()), number(), number(), number())
       -> number().
invert_angular(F, Y, A, B) ->
    %% returns X such that A =< X =< B for which f(X) = Y
    %% where |X-X0| < Tolerance
    Tolerance = 1/100000,
    Phi = fun(L,U) -> U-L < Tolerance end,
    Psi = fun(X) -> mod(F(X) - Y, 360) < deg(180) end,
    bisection_search(A, B, Phi, Psi).

bisection_search(U,V,Phi,Psi) ->
    X = (V+U)/2,
    case Phi(U,V) of
        true ->
            X;
        false ->
            case Psi(X) of
                true -> bisection_search(U,X,Phi,Psi);
                false -> bisection_search(X,V,Phi,Psi)
            end
    end.
