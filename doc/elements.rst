Element Traits
==============

We want our data structures to store all types of elements that exist in Ada.
There are a number of specificities to take into account:

  1. *Scalar types* can be stored directly in a record or an array. They can be
     safely copied, and are small enough that they can be returned from
     functions without impacting performance.

  2. *Unconstrained types* like arrays, strings or class-wide types cannot be
     stored in a record. Instead, they must be allocated on the heap, and a
     pointer to them stored in the record. Of course, one needs to free the
     pointer when it is no longer needed. We cannot simply duplicate the
     pointer in another record, since we end up with aliasing and freeing one
     of the pointers will result in an invalid memory access when the copy is
     accessed.

  3. *Controlled types* must be copied so that their primitive operations
     ``Adjust`` and ``Finalize`` are called as expected.

  4. *Limited types* cannot be copied, and in general cannot be stored in
     containers anyway. They are manipulated via one level of indirection and a
     pointer.

  5. Some types need to know their address (a record containing a field that
     itself has a discriminant pointing to the parent record for instance).
     Such types are fairly rare, but cannot be moved in memory, so they need
     to be copied (and the old copy deleted).

Performance is important, and a large part of it depends on the number of
memory allocations that we perform in our data structures. The fewer
allocations the faster the algorithms will execute.

A large number of containers are given an initial size, and will grow as needed
to store more elements. This growing can be done by allocating new memory, copy
the old contents to the new location, and freeing the old contents. This is not
optimal though. By using the low-level C ``realloc`` function, it is often
possible that the system will grow the container in place, that is will take
advantage of free memory next to the container to extend its size, without a
need for copying the elements from one location to the next. Whenever possible,
we try to use ``realloc``, but this is not always appropriate.

Writing a container that could contain any of the above types would be
inefficient, since we would systematically have to allocate memory (to accept
unconstrained types), always use explicit copy and free (for pointers and
controlled types) and never use `realloc` (for types that rely on their
address).

.. index: conts-elements.ads
.. index: element traits

Instead, this library introduces the concept of *Element Traits*, which are
packages that describe the various properties of elements. Such a package
is defined in :file:`conts-elements.ads`, and provides the following
information:

  ``Element_Type``

      The type of element as manipulated by the user. It can be any type of
      element (scalar, unconstrained, controlled,...) except a limited type
      since it needs to be copied into the container.

  ``Stored_Type``

      The type that is actually stored in the container. For constrained types
      (records for instance) and scalar types, this can be the type itself. For
      unconstrained types, this will be a newly allocated pointer for instance.

  ``Returned_Type``

      The type returned by the container. In general, this will be
      ``Element_Type`` itself. However, when the type might be large (a string
      for instance), it is more efficient to return a reference type (as
      introduced in Ada 2012). Such a reference type is automatically
      derefenreced, and therefore is similar to ``Element_Type`` for the
      application point of view, in most cases. We do not recommend returning
      the access type itself, since it might be possible for users to
      inadvertently free it.

  ``To_Stored``, ``To_Return`` and ``To_Element``

      These are conversion functions between the three types declared above.
      It is fine for ``To_Stored`` to allocate memory, since that can be
      freed by ``Release`` (see below). The others should not, in general,
      allocate memory.

  ``Release``

      Releases the memory used by ``Stored_Type``. Most of the time, it will
      free the memory, but the more general term release was used since it
      might be decrementing a refcount, or freeing the memory used by some
      internal field, or any other possibility.

  ``Copy`` and ``Copyable``

      When a stored element can be copied via the default Ada assignment
      operator (``:=``), ``Copyable`` should be set to ``True``. In this case,
      the ``Copy`` function will be ignored. This provides more opportunities
      for optimization for the compiler, since copying an array of elements,
      for instance, will copy potentially a whole slice at once. It will also
      properly handle controlled types by calling ``Adjust`` and ``Finalize``
      as needed.

      If you need finer control, you can set ``Copyable`` to ``False`` and
      provide your own copy operation.

  ``Movable``

      As mentioned above, this library will try to use ``realloc`` when
      possible to improve performance. It is possible that this function,
      though, will need to move the elements in memory. Since this is a C
      function, it knows nothing about controlled types, for instance.  In such
      a case, the element should be marked as not movable. This will force the
      library not to use ``realloc``, and explicit allocate a new copy, copy
      all the elements, and free the old ones.

As you can see, there is a large number of information to provide. These are in
general very simple implementations (expression functions will often be
enough). However, the library provides a number of higher-level packages to
make it even easier:

  :file:`conts-elements-definite.ads`

     A simple generic package that only requires an ``Element_Type`` and
     provides all the other fields. It is suitable for scalars and simple
     records, but not for pointer types (since it copies the elements
     directly).

  :file:`conts-elements-indefinite.ads`

     A simple generic package that only requires an ``Element_Type``, which
     could be any type (unconstrained, class-wide, ...). It will allocate
     memory to store the element in containers.

  :file:`conts-elements-indefinite_ref.ads`

     Similar to :file:`conts-elements-indefinite.ads`, but returns a
     reference type rather than the type itself. This will often be more
     efficient.

  :file:`conts-elements-indefinte_spark.ads`

     Similar to :file:`conts-elements-indefinite.ads`, but hides the
     access types. This is suitable for use with the SPARK language.

  :file:`conts-elements-null_elements.ads`

     Storing a null record. This can be used when no additional information
     is needed in fact. For instance, a graph associates some information with
     each of its vertices. But sometimes you have no need for that, and you
     can simply use this package for the type of the extra information.

  :file:`conts-elements-arrays.ads`

     Similar to :file:`conts-elements-indefinite.ads`, but slightly optimized
     for arrays. In particular, this will avoid allocating memory when the
     array is small, which might in some cases improve efficiency (although
     the data structure will be bigger, so you should measure whether you
     do get the expected improvement).

