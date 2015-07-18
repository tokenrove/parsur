Parser combinators in Ur
------------------------

Inspired by [discussion on the Ur/Web mailing list circa
July 2014](http://comments.gmane.org/gmane.comp.lang.ur/1712)
but built around more efficient string pointer passing (where
possible) rather than lists of characters.

I was working on a library of parser combinators for Ur/Web since my
own manual recursive descent things were evolving into something like
that anyway.  I also considered retargeting mlyacc or menhir to emit
Ur/Web code, but this seemed like the easiest approach.

Status
------

The blob support still needs to be developed, some things need to be
cleaned up, and the API will certainly have breaking changes without
notice.  As the API stabilizes, documentation including performance
information should appear.

See the examples in `demos/` for ideas on using this library in your
own application.

What's here should be considered alpha quality at best.  Patches
thoughtfully considered.

License
-------

This code is released under the
[MIT license](http://opensource.org/licenses/mit-license.html) (see
the file `LICENSE`), since the LGPL's status is murky with regards
whole-program compilation as performed by urweb.
