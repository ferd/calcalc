-module(calcalc).
-compile(export_all).
-import(calcalc_math, [sum/3,
                       lcm/2, gcd/2, mod/2, amod/2, signum/1,
                       floor/1, ceil/1,
                       deg/1]).

-define(JULIAN_DAY_EPOCH, -1721424.5).
-define(MODIFIED_JULIAN_DAY_EPOCH, 678576).
-define(bahai, 'bahá\'í').

-include("calcalc.hrl").

-type calendar() :: hebrew | mayan | hindu | chinese
                  | egyptian | tibetan | julian | roman | gregorian
                  | iso | ethiopic | coptic | armenian | persian
                  | islamic | zoroastran | french_revolutionary
                  | ?bahai.
-type julian_day() :: number().
-type modified_julian_day() :: number().
-type moment() :: float().
-type time() :: float().
-type fixed() :: integer().
-type date() :: map().
-type clock() :: #clock{}.
-type angle() :: #angle{}.
-type degrees() :: number().
-export_types([julian_day/0, modified_julian_day/0,
               moment/0, time/0, fixed/0,
               date/0, clock/0, angle/0, degrees/0]).

-inline([jd_epoch/0, mjd_epoch/0,
         deg/1]).
%% API exports
-export([]).


%%====================================================================
%% Behaviour
%%====================================================================
-callback epoch() -> integer().

-callback is_valid(date()) -> boolean().

-callback to_fixed(date()) -> fixed().

-callback from_fixed(fixed()) -> date().
%%====================================================================
%% API functions
%%====================================================================

%%====================================================================
%% Internal functions
%%====================================================================
-spec degrees_from_angle(angle()) -> degrees().
degrees_from_angle(#angle{degree=D, arcmin=M, arcsec=S}) ->
    D + (M + (S/60))/60.

-spec angle_from_degrees(degrees()) -> angle().
angle_from_degrees(Deg) ->
    Sign = if Deg < 0 -> -1
            ; Deg >= 0 -> 1
           end,
    #angle{degree = Sign * floor(abs(Deg)),
           arcmin = Sign * floor(60 * mod(abs(Deg), 1)),
           arcsec = Sign * mod(abs(Deg)*60*60, 60)}.

%% clock_from_moment/1 rounded to the nearest second:
%% clock_from_moment((round(Time * 24*60*60) / (24*60*60))
-spec clock_from_moment(moment()) -> clock().
clock_from_moment(Time) ->
    #clock{hour = floor(Time*24),
           min  = floor(mod(Time*24*60, 60)),
           sec  = mod(Time*24*60*60, 60)}.

-spec time_from_clock(clock()) -> time().
time_from_clock(#clock{hour=H, min=M, sec=S}) ->
    (1/24) * (H + (M + (S/60))/60).

jd_epoch() -> ?JULIAN_DAY_EPOCH.
mjd_epoch() -> ?MODIFIED_JULIAN_DAY_EPOCH.

-spec moment_from_jd(julian_day()) -> moment().
moment_from_jd(JD) -> JD + jd_epoch().

-spec jd_from_moment(moment()) -> julian_day().
jd_from_moment(T) -> T - jd_epoch().


%% Assume epoch = 0;
%% formula is then T - Epoch;
%% therefore identity function
-spec fixed(integer()) -> fixed().
fixed(T) -> T.

-spec fixed_from_mjd(modified_julian_day()) -> fixed().
fixed_from_mjd(Mjd) -> Mjd + mjd_epoch().

-spec mjd_from_fixed(fixed()) -> modified_julian_day().
mjd_from_fixed(Date) -> Date - mjd_epoch().

-spec fixed_from_jd(julian_day()) -> fixed().
fixed_from_jd(Jd) -> floor(moment_from_jd(Jd)).

-spec jd_from_fixed(fixed()) -> julian_day().
jd_from_fixed(Fixed) -> jd_from_moment(Fixed).

-spec fixed_from_moment(moment()) -> calcalc:fixed().
fixed_from_moment(Moment) -> calcalc_astro:fixed_from_moment(Moment).

%% Time of day
-spec time_from_moment(moment()) -> calcalc:time().
time_from_moment(T) -> calcalc_astro:time_from_moment(T,1).

-spec to(fixed | calendar(), date() | fixed()) -> date().
%% To fixed mode
to(fixed, D = #{cal := Mod}) -> Mod:to_fixed(D);
%% Calendar conversion mode
to(Cal, D = #{cal := Mod}) -> (mod(Cal)):from_fixed(Mod:to_fixed(D));
%% From fixed mode
to(Cal, Fixed) when is_integer(Fixed) -> (mod(Cal)):from_fixed(Fixed).

-spec date(calendar(), #{}) -> date().
date(Cal, D=#{}) -> (mod(Cal)):date(D).

-spec epoch(calendar()) -> integer().
epoch(Cal) -> (mod(Cal)):epoch().
%epoch(hebrew) -> -1373427;
%epoch(mayan) -> -1137142;
%epoch(hindu) -> -1132959;
%epoch(chinese) -> -963099;
%epoch(tibetan) -> -46410;
%epoch(french_revolutionary) -> 654415;

-spec is_valid(date()) -> boolean().
is_valid(Date=#{cal := Cal}) -> Cal:is_valid(Date);
is_valid(_) -> false.

-spec mod(calendar()) -> module().
mod(hebrew) -> calcalc_hebrew;
mod(mayan) -> calcalc_mayan_long; % long count calendar alias
mod(mayan_long) -> calcalc_mayan_long; % long count calendar
mod(old_hindu_solar) -> calcalc_old_hindu_solar;
mod(old_hindu_lunisolar) -> calcalc_old_hindu_lunisolar;
mod(hindu) -> calcalc_hindu;
mod(chinese) -> calcalc_chinese;
mod(egyptian) -> calcalc_egyptian;
mod(tibetan) -> calcalc_tibetan;
mod(julian) -> calcalc_julian;
mod(roman) -> calcalc_roman;
mod(gregorian) -> calcalc_gregorian;
mod(icelandic) -> calcalc_icelandic;
mod(iso) -> calcalc_iso;
mod(ethiopic) -> calcalc_ethiopic;
mod(coptic) -> calcalc_coptic;
mod(armenian) -> calcalc_armenian;
mod(persian) -> calcalc_persian;
mod(persian_arithmetic) -> calcalc_persian_arithmetic;
mod(islamic) -> calcalc_islamic;
mod(zoroastrian) -> calcalc_zoroastrian;
mod(french_revolutionary) -> calcalc_french_revolutionary;
mod(bahai) -> calcalc_bahai;
mod(?bahai) -> calcalc_bahai;
mod(bahai_old) -> calcalc_bahai_old;
%% Year-less calendars
mod(bali) -> calcalc_bali;
mod(mayan_haab) -> calcalc_mayan_haab; % (Month, Day) calendar
mod(mayan_tzolkin) -> calcalc_mayan_tzolkin; % (Number, Name) calendar
mod(aztec_xiuhmolpilli) -> calcalc_aztec_xiuhmolpilli; % joined aztec, seems more common, but sometimes has the name swapped with xihuitl
mod(aztec_xihuitl) -> calcalc_aztec_xihuitl; % (Month, Day) calendar
mod(aztec_tonalpohualli) -> calcalc_aztec_tonalpohualli. % (Number, Name) calendar

