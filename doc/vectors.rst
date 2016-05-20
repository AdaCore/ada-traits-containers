`Vector` data type
==================

Vectors are similar to Ada arrays, but can in general grow in size.
They use contiguous storage for their elements, which improves
caching on the processor, compared to lists, and therefore are
generally significantly faster when accessing elements.

Adding an element at the end of a vector is done in constant time.
In some cases, the vector will need to increase its capacity to
make space for the new element. For efficiency reasons, it will
then allocate more memory than it needs, so that the next time an
element is appended it won't need to reallocate memory again (and
potentially copy the elements).

As opposed to lists, inserting at the beginning or the middle of
a vector might be slow since all elements potentially need to be
copied.

Vector nodes
------------

The ``Conts.Vectors.Storage`` package is a generic package that provides
the low-level implementation of vectors. It is used to instantiate an
actual vector, but not meant to be used directly by applications.

Instead, it lets you chose between various possible implementations for
the vectors:

  :file:`conts-vectors-unbounded.ads`

     This is the type of nodes that should be used for **unbounded vectors**,
     that will grow automatically as more elements are added to it. See the
     :ref:`vector_resizing_strategy` section below.

  :file:`conts-vectors-bounded.ads`

     This package provides support for **bounded vectors**. Such vectors have a
     statically known maximal capacity, and no more than this many elements can
     be added to the vector. In exchange, no dynamic memory allocation is
     performed by the vector (unless the elements themselves require it, of
     course). This might be necessary in some contexts (embedded programming
     for instance often has a restriction on memory allocations). These are
     very similar to Ada arrays in practice, but work better with the rest of
     this library since they provide the usual cursors needed by algorithms,
     for instance.

     The maximal capacity of a bounded container is given by a discrimant on
     the container type.

It is possible, although not trivial, to provide new implementations for the nodes.

To instantiate any of the packages above, several parameters are needed for the
generic:

  ``Elements``

     This is an instance of the element traits package (:ref:`element_traits`),
     which describes which elements are stored in the vector, and how to do so.

  ``Container_Base_Type``

     A vector is always implemented as a :ref:`tagged type
     <tagged_and_controlled_types>`. But sometimes applications will want them
     as controlled types with automatic memory deallocation (set the
     ``Container_Base_Type`` to ``Ada.Finalization.Controlled``), whereas in
     other cases we want the container to simply be a limited type (set the
     ``Container_Base_Type`` to ``Conts.Limited_Base``).

     Other combinations are of course possible. You could chose to have all
     your containers derive from a common base type for instance.


.. _vector_resizing_strategy:

Resizing strategy
-----------------

When the *capacity* of a vector needs to be increased, it will reallocate its
internal array to make space for the new elements.  For performance reasons, it
should then allocate more memory than just one element. Standard strategies
include doubling the capacity, increasing by a fixed number of elements, and
perhaps alternate between the two depending on the current capacity of the
vector.

All these strategies try to find the best compromise between memory usage and
speed.

The default strategy used in this library is to increase the capacity by half,
but you get full control via

  ``Resizing_Strategy``

      A generic traits package that provides two operators ``Grow`` and
      ``Shrink``. Given the current capacity of a vector and the minimal
      expected size, they return the new capacity that shoud be allocated.

Generic vectors
---------------

If you need full control on the implementation of your vector packages, you
might chose to directly instantiate the package
:file:`conts-vector-generics.ads`. This package requires two pieces of
information:

   ``Index_Type``

       This is the type used to reference elements in the vector. It will
       often be an integer type (natural or positive for instance).
       It is invalid for this type to be either `Integer`, or an enumeration
       type because the package needs a way to indicate an invalid index,
       for instance when using `Last` on an empty vector.
       To use an enumeration type, you will in fact need a subtype for which
       the base type has one more element, as in::

           type Base_Enum is (Invalid, A, B, C, D, E);
           subtype Enum is Base_Enum range A .. Base_Enum'Last;

   ``Storage``

       This is the choice of the vector nodes package for the low-level
       implementation, as described above. This lets you chose between
       **bounded** and **unbounded** vectors.

The following operations are provided for vectors:

  ``Reserve_Capacity``

  ``Shrink_To_Fit``

  ``Length``

  ``Is_Empty``

  ``Element``

  ``Last_Element``

  ``Replace_Element``

  ``Append``

  ``Clear``

  ``Delete``

  ``Delete_Last``


It also provides cursors that can be used for any of the algorithms. They
however do not provide the cursor traits package (which cannot be
instantiated yet). See the :ref:`simple_vectors` section for vectors that
provide these by default.

These vectors also do not provide support for Ada2012 iterators and the
for-of loop. See the package below as well.

Here is an example creating a full vector from scratch::

    --  show an example


.. _simple_vectors:

Simple vectors
---------------

Instantiating the full package above requires several steps. Although this
is not a difficult operation, it can make the code harder to read, and is
not compatible with the standard Ada vectors.

For this, this library provides a set of packages that are easier to
instantiate:

  :file:`conts-vectors-definite_bounded.ads`

     Support for bounded containers of definite elements. Such vectors never
     need to allocate any memory, so are very efficient.

     ::

        --  show an example on how to specify the capacity

  :file:`conts-vectors-definite_unbounded.ads`

     Support for unbounded containers of definite elements. They need to
     allocate memory for the low-level array, but not for the elements
     themselves. The vector will grow as needed.

  :file:`conts-vectors-indefinite_unbounded.ads`

     Support for unbounded containers of indefinite elements. Memory is
     allocated for both the low-level array and for each of the elements.

  :file:`conts-vectors-indefinte_unbounded_ref.ads`

     Similar to the previous one, but the elements are returned as
     reference types, which might be more efficent in general.

All these packages improve on the generic vector by also providing the
necessary aspects to support Ada's ``for E of Vec`` loops, as well as
the direct indexing, as in ``Vec (1)`` rather than ``Vec.Element (1)``.

They also pre-instantiate the cursors traits package for convenient use
in the algorithms.
