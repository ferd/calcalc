-module(prop_calcalc_circular).
-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").
-compile(export_all).

prop_hebrew() -> circular(hebrew).
prop_mayan() -> circular(mayan).
prop_mayan_long() -> circular(mayan_long).
prop_old_hindu_solar() -> circular(old_hindu_solar).
prop_old_hindu_lunisolar() -> circular(old_hindu_lunisolar).
%prop_hindu() -> circular(hindu).
%prop_chinese() -> circular(chinese).
prop_egyptian() -> circular(egyptian).
%prop_tibetan() -> circular(tibetan).
prop_julian() -> circular(julian).
prop_roman() -> circular(roman).
prop_gregorian() -> circular(gregorian).
prop_icelandic() -> circular(icelandic).
prop_iso() -> circular(iso).
prop_ethiopic() -> circular(ethiopic).
prop_coptic() -> circular(coptic).
prop_armenian() -> circular(armenian).
prop_persian() -> circular(persian).
prop_persian_arithmetic() -> circular(persian_arithmetic).
prop_islamic() -> circular(islamic).
prop_zoroastrian() -> circular(zoroastrian).
%prop_french_revolutionary() -> circular(french_revolutionary).
prop_bahai() -> circular(bahai).
prop_bahai_old() -> circular(bahai_old).

circular(Cal) ->
    ?FORALL(Fixed, integer(),
            Fixed =:= calcalc:to(fixed, calcalc:to(Cal, Fixed))).
