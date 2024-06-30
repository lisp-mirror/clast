CLAST
=====

Copyright (c) 2014-2020 Marco Antoniotti and Matteo Crespi, all rights reserved.
Copyright (c) 2020-2024 Marco Antoniotti, all rights reserved.

**CLAST** is a Common Lisp library that can produce an *abstract
syntax tree* of a *form*.  Its main use is for *source code analysis*
and *transformation*, e.g., extracting the *free variables* list from
a form.

The library relies on a working implementation of the "*environment*"
functions from **CLtL1**, so, at this point, it does not work on every
available implementation.  Currently, **CLAST** works on:

- [Allegro](https://www.franz.com) at least 11.x.
- [Clozure CL](https://www.clozure.com) at least 1.12.2.
- [Lispworks](https://www.lispworks.com) at least 8.x.
- [SBCL](https://www.sbcl.org) at least 2.2.9.

Other implementations are in the works:
[CMUCL](https://cmucl.cons.org) and others will be ready soon.

See the file `COPYING` for licensing information


Installation
------------

**CLAST** should be available directly from **Quicklisp**.  In
alternative, just download the code from the repository
(or `git clone` it).  Then you just need to `load` the `.asd` file (or
the `.system` file; your choice) and proceed from there.

### Dependencies

**CLAST** depends only on `fiveam` for testing.  If you will not use
the test system to run the tests, you will not need `fiveam`.  I
suggest you `load` (or `quickload`) `fiveam` **before** loading the
**CLAST** system file, to minimize interaction with the building
process.

### Warnings

[SBCL](https://www.sbcl.org) generates some hairy warnings during
compilation.  They apear to be type-checking and optimization related
(plus the usual obnoxious `style-warnings` about stuff that should not
flagged as such).  There should not be any problems about it.



Testing
-------

You can run the tests using `fiveam`.  The top level suite is called
`:parse`.

At the time of this writing there are two expected failures in the
test suite.  Also, `sbcl` and `allegro` produce several warning about
declarations that **CLAST** generates for `defstruct` and `defclass`
*automatic* functions.  You can ignore them, but a fix would be most
than welcome.


Documentation
-------------

The documentation is produced with
[`HEΛP`](https://helambdap.sf.net).  It is accessible from the
[CLAST main site](https://clast.sf.net).


Help Needed
-----------

Please help extending and fixing this library.  You know the drill.
Porting to [ECL](https://ecls) and [CLISP](https://clisp) should be
doable.  Porting to [ABCL](https://abcl) needs... some more work.


A NOTE ON FORKING
-----------------

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.
