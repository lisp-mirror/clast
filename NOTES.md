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

