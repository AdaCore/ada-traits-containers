Introduction
============

This library provides a number of data types and algorithms.

These algorithms, however, work with potentially any data type, not just
the specific implementations provided in this package. For instance, you
do not have to use any of the provided `Graph` data structures to use
an algorithm like `Depth First Search`. Instead, via the careful use of
generics, these algorithms can be applied to any other data structure.

The first part of this documentation will explain the design and concepts
of this library. Later chapters then take a look at each of the data
structures and algorithms. Finally, some considerations on the use of
generics in Ada will be explained.
