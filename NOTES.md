NOTES
=====

2015-01-14 ANSI operators defined as macros.
--------------------------------------------

There are several operators in the ANSI spec that are defined as
'macros'; the iteration constructs are such -- DOLIST, DOTIMES, DO,
DO*, LOOP.

It would make sense to have MACRO-APPLICATION as a superclass of,
e.g., LOOP-FORM in order to list them later as actual macros when
calling MACROS.

This would complicate the construction of the iteration form instances
and other as well.  A different implementation choice would be to work
on MAP-SUBFORMS or on the calls to WALK.


2024-06-06 New "implementation" package and new environments.
-------------------------------------------------------------

I got back to this library after a long time and noted that it does
not quite work as expected.  There are two issues:
1. The "implementation" is not quite separated (ia a package needed?)
2. The environment machinery is insufficient, because I need to
   potentially modify a "parent" environment.

Hence the current modifications in the branch `env-tree`.


2024-06-13 Handling of global environments semi-working.
--------------------------------------------------------

Fixed the gross problems, however, the handling of the global
environment is still iffy and not general.

Things do work as long as there are no "definitions", "proclamations"
or similar "usually top level" forms in subforms.


2024-06-14 Environments notes.
------------------------------

It may be easier, eventually to really just keep the "global"
environment... global.  This will require a whole reworking of the API
and of the code.

Also, in order to fix some issues with the processing of declarations
and proclamations, FTTB, it may be useful to have an "environment"
slot in the `form` instances, although this may become unduly
expensive.


2024-06-17 Need to rework environments.
---------------------------------------

The "global" environment handling is kludgy.  This is necessary, FTTB,
to smooth differences among different implementations.  But there must
be a better way, even given the constraint to keep the CLtL2
environments in place.


2024-06-18 Need to refactor code.
---------------------------------

As expected and feared, SCBL complains.  But it does have a point and
I should really refactor the code in order to have **all** the
implementation dependent code loaded before the top level one.


2024-06-24 Refactoring done and Allegro working.
------------------------------------------------

Refactoring of code mostly done (it could be improved) and Allegro
(Express 11.x) is working.
