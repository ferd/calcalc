-ifndef(CAL).
-define(CAL, ?MODULE).
-endif.

-record(clock, {day :: integer(),
                hour :: integer(),
                min :: integer(),
                sec :: integer()}).
-record(angle, {degree :: calcalc:degrees(),
                arcmin,
                arcsec}).

-compile({no_auto_import,[ceil/1, floor/1]}).
