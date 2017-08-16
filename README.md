calcalc
=====

`[CALendrical CALCulations]`

This library implements a converted version of the Common Lisp code and algorithms described in the third edition of *Calendrical Calculations* by Nachum Dershowitz and Edward M. Reingold.

This library is made public under the following conditions:

- The code can be used for personal use
- The code can be used for demonstrations purpose
- Non-profit reuse with attribution is fine
- [Commercial use of the algorithms should be licensed](http://www.cs.tau.ac.il/~nachum/calendar-book/third-edition/CIIT.html) and are not allowed from this library.

The permissions above are granted **as long as attribution is given to the authors of the original algorithms, Nachum Dershowitz and Edward M. Reingold**.

This code has been ported to Erlang by Fred Hebert.

Build
-----

    $ rebar3 compile

Development
-----------

Development of the library is still ongoing. So far, the basic non-astronomical calendars have been ported, but the astronomical ones have not.

The code is subject to reorganization and modifications. It is my intent to rework and unify the interface sooner or later.
