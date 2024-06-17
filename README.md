CLAST
=====

Copyright (c) 2014-2020 Marco Antoniotti and Matteo Crespi, all rights reserved.
Copyright (c) 2020-2024 Marco Antoniotti, all rights reserved.

CLAST is a Common Lisp library that can produce an *abstract syntax
tree* of a *form*.  Its main use is for source analysis and
transformation, e.g., extracting the *free variables* list from a
form.

The library relies on a working implementation of the "environment"
functions from CLtL1, so, at this point, it does not work on every
available implementation.

See the file COPYING for licensing information


A NOTE ON FORKING
-----------------

Of course you are free to fork the project subject to the current
licensing scheme.  However, before you do so, I ask you to consider
plain old "cooperation" by asking me to become a developer.
It helps keeping the entropy level at an acceptable level.
