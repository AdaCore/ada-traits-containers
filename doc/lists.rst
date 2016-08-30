`List` data type
================


Bounded lists
-------------

A bounded list is a list with a known maximal number of elements. The
list can be shorter, but can never contain more elements than that
maximum size.

The tradeoff for this restriction is that the list does not need to
perform any memory allocation for the list's nodes, which might be
faster and fit in tight memory constraints.
