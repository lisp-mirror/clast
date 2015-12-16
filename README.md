CLAST
=====
Copyright (c) 2014-2016 Marco Antoniotti, all rights reserved.

CLAST is a Common Lisp library that can produce an "abstract syntax
tree" of a "form".  Its main use is for source analysis and
transformation, e.g., extracting the "free variables" list from a
form.

The library relies on a working implementation of the "environment"
functions from CLtL1, so, at this point, it does not work on every
available implementation.

See the file COPYING for licensing information
