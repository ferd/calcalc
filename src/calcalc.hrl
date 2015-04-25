-record(date, {cal = ?MODULE :: module(),
               year :: integer(),
               month :: integer(),
               day :: integer()}).
-record(clock, {day :: integer(),
                hour :: integer(),
                min :: integer(),
                sec :: integer()}).
-record(angle, {degree :: calcalc:degrees(),
                arcmin,
                arcsec}).
