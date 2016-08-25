Design considerations
=====================

During the design of this library, there were quite a number of design
decisions that were taken and might not be immediately obvious.

This chapter documents these decisions, and perhaps the limitations they bring,
or the flexibility they offer.

This chapter is really about the internal design of the library, and can be
skipped if you are only interested in using the final product.

Nested generics
----------------

As we have seen, this library makes extensive use of generics, and generics
that receive other generics as parameters. This makes the library sometimes
harder to instantiate than one might like.

.. highlight:: ada

Let's take an example. A we saw, a lot of graph algorithms need to store
information in the vertex, like a color to know whether they were already
visited or not. These properties are stored in what we call a property
map. So one approach would be to take both a graph and a property map,
each defined independently of each other::

   generic
      type Graph is private;
      type Vertex is private;
   package Graph_Traits is
   end Graph_Traits;

   generic
      with package Graphs is new Graph_Traits (<>);
      type Key is private;
      type Value is private;
      with function Get (G : Graphs.Graph; K : Key) return Value;
   package Property_Maps is
   end Property_Maps;

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Property_Maps (<>);
   procedure Some_Algo;

Such a description however is useless to us. It is not possible to specify that
the `Color_Maps` is applied to a `Graphs.Vertex` and stores `Color`. So we
cannot in effect use the `Maps` in our algorithm.

What we really would like is a partial constraint applied to the formal
parameters, as in the following invalid Ada code::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Property_Maps
         (Graphs, Graphs.Vertex, Color, <>);   --  Invalid Ada
   procedure Some_Algo;

where the exact implementation of `Get` is unspecified, but we still want to
associate a `Color` with a `Vertex`.

Instead we have to use a slightly more complex definition for the property
maps, which helps us define parts of the parameter, and leave some others
unspecified::

   generic
      type Graph is private;
      type Key is private;
      type Value is private;
   package Property_Maps is
      generic
         with function Get (G : Graph; K : Key) return Value;
      package Map is
      end Map;
   end Property_Maps;

   generic
      type Graph is private;
      type Vertex is private;
   package Graph_Traits is
      package Color_Maps is new Property_Maps (Graph, Vertex, Color);
   end Graph_Traits;

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is new Graphs.Color_Maps (<>);
   procedure Some_Algo;

At this point, the compiler knows that Maps will be a pakage that provides a
`Get` function, that maps our `Vertex` to a `Color` appropriately.  This comes
at the cost of one extra level of instantiation (the `Color_Maps` in the
`Graph_Traits` package).

Pre and post conditions
-----------------------

This library is meant to be (at least for a subset) usable for SPARK users that
want to prove their code. To do so, the API needs to set pre and post conditions
on the relevant subprograms.

One of the difficulties, however, is that some of the preconditions need to be
computed. For instance, a tree layout algorithm would not work on a graph with
cycle. As it happens, there is an algorithm (based on depth-first-search) to
compute whether a graph has cycles.

This is however a generic algorithm, which needs to be instantiated with a
description of what a graph, vertex and edge are (the `Graph_Traits`), as
well as a `Color_Property_Map`. So we have the following::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is
         new Graphs.Color_Property_Maps.Exterior (<>);
   function Is_Acyclic (G : Graphs.Graph) return Booolean;

   generic
      with package Graphs is new Graph_Traits (<>);
   procedure Tree_Layout (G : Graphs.Graph);

However, as it is we cannot use Is_Acyclic in a precondition, since we have
no instance of it.

The only solution therefore is to pass the instance of Is_Acyclic directly
as a formal parameter. The following invalid code would be interesting::

   generic
      with package Graphs is new Graph_Traits (<>);
      with package Maps is
         new Graphs.Color_Property_Maps.Exterior (<>);
   function Is_Acyclic (G : Graphs.Graph) return Boolean;

   --  Invalid Ada
   generic
      with package Graphs is new Graph_Traits (<>);
      with function Acyclic is new Is_Acylic (Graphs, <>);
   procedure Tree_Layout (G : Graphs.Graph)
      with Pre => Acyclic (G);

This is however invalid for two reasons:

  - As we saw before, we cannot use partial instantiation, so we need to also
    have an instance of `Color_Property_Maps` as a parameter::

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
       function Is_Acyclic (G : Graphs.Graph) return Boolean;

       --  Invalid Ada
       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
          with function Acyclic is new Is_Acylic (Graphs, Maps);
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic (G);

  - In fact, as opposed to formal packages, Ada does not let us indicate that a
    formal parameter is an instance of a specific generic subprogram.

    The simpler approach would be to let any value for `Acyclic`, as in::

       generic
          with package Graphs is new Graph_Traits (<>);
          with function Acyclic (G : Graphs.Graph) return Boolean;
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic (G);

    The main drawback here, though, is that the user could pass anything
    function as an actual parameter, so the algorithm does not know for a fact
    that it did receive an acyclic graph. So a better solution is to use a
    generic package instead::

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
       package Is_Acyclic is
          function Acyclic (G : Graphs.Graph) return Boolean;
       end Is_Acyclic;

       generic
          with package Graphs is new Graph_Traits (<>);
          with package Maps is
             new Graphs.Color_Property_Maps.Exterior (<>);
          with package Acyclic is new Is_Acylic (Graphs, Maps);
       procedure Tree_Layout (G : Graphs.Graph)
          with Pre => Acyclic.Acyclic (G);

    Since this all makes the instantiation of `Tree_Layout` harder, even for
    users that do not need the preconditions because they do not run them
    or prove their code. So the final idea is to have two versions of
    `Tree_Layout`: one without the preconditions, as we saw in the first
    example in this section, and one with the preconditions as we just saw,
    which instantiates and run the first version.

    That way, we have no code duplication, and yet have full preconditions
    for users who need them.

The use of these containers in the context of SPARK imposes a minor changes
compared to the API used for the standard Ada containers: we now need to
pass the container explicitly in the cursor operations like `Next` and
`Has_Element`, whereas these are not needed for standard Ada containers.
This is so that the implementation does not need to store a pointer to the
container inside the cursor, which could be unsafe.

In practice, this makes the implementation cleaner and even faster since
the cursor are lighter weight.

Assertion policy
----------------

This library is intended for both Ada and SPARK users. Some of the subprograms
need to perform some validity checks on their parameters. For instance, a
function that `Get` the n-th element of a vector needs to check that `n` is
a valid index.

This check is part of the preconditions for `Get`. This is needed to prove the
use of the containers for SPARK programs.

Ada applications, though, might not be compiled with `-gnata`, and therefore
would not run the assertions. This means we need to duplicate the same check
in the body of `Get`.

This is not elegant, since it results in duplication of the code (and thus
will be hard to keep synchronized), and will result in duplicate checks at
runtime for applications compiled with `-gnata`.

Instead, our approach is to add an `Assertion_Policy` in all packages::

   pragma Assertion_Policy (Pre => Check);
   pragma Assertion_Policy (Post => Ignore);
   pragma Assertion_Policy (Ghost => Ignore);

The effect is that the code for the preconditions will always be run,
no matter whether the application was compiled with `-gnata` or not.
We can therefore remove the explicit checks in the body of the
subprograms, and rely on those checks already written for the
preconditions.

The postconditions are only needed for SPARK users to prove the use of
the containers, but we never need to run them. So we systematically
disable post-conditions, whether we compile with `-gnata` or not.

Default values
--------------

In Ada, it is possible to provide default values for formal subprogram
parameter in generics, as in::

   procedure Bar;

   generic
      with procedure Foo is Bar;
   package Pkg is
   end Pkg;

Unfortunately, the same doesn't exist for formal package parameters, which
makes the instantiation of our various packages more difficult for the end
user. For instance, a number of packages that need memory allocations take
a `Storage_Pool` package to control how the allocations are performed.
Since we cannot set a default value for these, the user will always have
to specify `Conts.Global_Pool` explicitly.

We could also simplify the library a code a bit if the default value for
formal subprograms could be defined as an expression function, rather than
be the name of an explicit subprogram, as in::

    generic
       type Element is private;
       with function Copy (E : Element) return Element is (E);
    package Pkg is
    end Pkg;

and we do not have to write an `Identity` function as done in several
places in this library.


Tasking
-------

Currently, the containers are not thread safe.

There are various ideas on the subject though:

   - a container that is written to from a single task, should be
     safe to read from multiple tasks. This is not the case with the
     standard Ada containers, since even in "read-only" mode they
     still modify their tampering flags.

   - we will likely introduce a new `Tasking_Policy` package to help
     control locking of the data structure: the default implementation
     would do nothing, providing maximum efficiency, but other
     implementation could use locks implemented in different ways.

   - Likewise, we could use atomic counters for reference-counted types,
     and through a policy use standard integers if task-safety is not
     an issue.

Tagged types
------------

For the convenience of using the dot-notation for calling primitive operations,
we are making all containers tagged types.

However, they are not meant to be subclassed, and thus most operations are
class-wide. This provides maximum efficient (no dynamic dispatching), and
matches what is done in the C++ STL (no virtual methods).

The cursors themselves are not tagged. All cursor operations take both the
container and the cursor in parameter (to support bounded containers), and
thus the container is used in the dot-notation call (Self.Has_Element (Pos)).

One other advantage to tagged types is that this forces the instances to
be passed by reference, and thus limits the number of implicit copies
done when passing a container as a parameter to a subprogram.
They do not avoid copies when a function returns a container though.

Implementation Note: currently, the Iterable aspect requires primitive
operations, which make it slow. It should be enhanced to accept class-wide
operations instead, which would also remove a number of primitive operations
on the container types.

Controlled types
----------------

All standard Ada containers are controlled types. This is in general more
convenient for the user, but is not compatible with SPARK.

In this library, we do not force containers to be controlled. Instead, a
generic formal parameter `Base_Type` is often provided to let users
decide whether to use controlled types (with automatic copy and clear
operations for instance), or limited types (which prevent the assignment
iterator to avoid aliasing issues).

Storage pools
-------------

ome packages need to perform memory allocation. In all such cases, we declare
a formal generic package that provides the storage pools to use, so that users
have ultimate control over memory allocation (for instance, the nodes packages
themselves control how many allocations are taking place, and the storage pools
control how they are actually performed).

Rather than pass a single object of type access to Root_Storage_Pool'Class, we
pass both a type for the storage pool, and an object access to that type. This
is to avoid dynamic dispatching when calling the pool, since in some contexts
like libadalang, with highly optimized pools, it has been shown that the cost
of dispatching might become significant.

The drawback is that a formal package parameter cannot have a default value
(nor can a type, of course), so a user systematically has to provide a value.
But this is only when using the low-level generic packages. When using the
higher-level packages, they still simply take an Element_Type, and the
storage_pool is always the default global pool.

Example of use for custom pools:

    - more efficient pool in some contexts

    - persistent containers, by having a pool allocating a large buffer with
      mmap, and then using this buffer when allocating small blocks.

Formal Representation
---------------------
As this library is meant to be usable for SPARK users, annotations are provided
that describe the expected behavior of each subprogram. They are typically
written as Ada 2012 contracts. In this section, we describe the formal
representation of containers used in these annotations and explain how it can be
used to annotate user code.

 - The Notion of Models

   The most straight forward way of annotating a subprogram dealing with
   containers is by reusing the API subprograms. For example, here we have
   annotated a procedure that resets every element of a vector to zero using
   an Ada 2012 quantified expression and two regular functions ``Last`` and
   ``As_Element`` of the vector API::

     procedure Set_All_To_Zero (V : in out Vector) with
       Post => (for all N in 1 .. Last (V) => As_Element (V, N) = 0);

   To annotate the containers API, this simple technique turned out to be
   insufficient. Indeed, when annotating a procedure, it is common to refer to
   the previous value of its in out parameters using the ``'Old`` Ada 2012
   attribute. For example, this is what we could write for a procedure that
   increments each element of a vector::

     procedure Increment_All (V : in out Vector) with
       Post => (for all N in 1 .. Last (V) =>
                As_Element (V, N) = As_Element (V'Old, N) + 1);

   Unfortunately, as the ``'Old`` attribute introduces a copy of its argument,
   it cannot be applied to expressions of a container type which may be limited.
   To work-around this restriction, we introduce models for containers. Models
   are actual objects which represent certain properties of the object they
   model. Typically they are a higher level, simpler view of a complex
   implementation. As an example, a ring buffer may be modelled by an array
   representing its content in the expected order::

     type Ring_Buffer is record
        Content : My_Array (1 ..7);
        First   : Positive := 1;
        Length  : Natural := 0;
     end record;

     function Model (R : Ring_Buffer) return My_Array;

     pragma Assert (Model (Ring_Buffer'(Content => (3, 4, 5, 42, 42, 1, 2),
                                        First   => 6,
                                        Length => 5))
                    = (1, 2, 3, 4, 5));

   Models offer advantages both in terms of expressiveness, readability and
   ease of use. However, they suffer from one major drawback which is
   efficiency. Indeed, they typically require copies of (part of) the objects
   they model and rely on a simple but often inefficient data representation.
   As a consequence, models are often implemented as ghost code, which means
   they can only be used in annotations so that they can be completely removed
   from the final executable by the compiler::

     function Model (R : Ring_Buffer) return My_Array with Ghost;

 - Functional Containers

   To model classical imperative containers, we introduce new simpler,
   mathematical like containers. They are unbounded and may contain indefinite
   elements. Furthermore, to be usable in every context, they are neither
   controlled nor limited. So that these containers can be used safely, we have
   made them functional, that is, no primitives are provided which would allow
   to modify an existing container. Instead, their API features functions
   creating new containers from existing ones. As an example, functional
   containers provide no ``Insert`` procedure but rather a function ``Add``
   which creates a new container with one more element than its parameter::

     function Add (C : Container; E : Element_Type) return Container;

   As a consequence, these containers are highly inefficient. They are also
   memory consuming as the allocated memory is not reclaimed when the container
   is no longer referenced. Thus, they should in general be used in ghost code
   and annotations so that they can be removed from the final executable.

   This library provides three functional containers, sequences, sets, and maps.
   A sequence is no more than an ordered collection of elements. In an Ada like
   manner, the user can choose the range used to index the elements::

     function Length (S : Sequence) return Count_Type;
     function Get (S : Sequence; N : Index_Type) return Element_Type;

   Functional sets offer standard mathematical set functionalities such as
   inclusion, union, and intersection. They are neither ordered nor hashed::

     function Mem (S : Set; E : Element_Type) return Boolean;
     function "<=" (S1, S2 : Set) return Boolean;

   Functional maps offer a dictionary between any two types of elements::

     function Mem (M : Map; K : Key_Type) return Boolean;
     function Get (M : Map; K : Key_Type) return Element_Type;

   Each functional container type supports iteration as appropriate, so that
   its elements can easily be quantified over.

   These containers can easily be used to model user defined data structures.
   They were used to this end to annotate and verify a package of allocators.
   In this example, an allocator featuring a free list implemented in an array
   is modeled by a record containing a set of allocated resources and a
   sequence of available ressources::

     type Status is (Available, Allocated);
     type Cell is record
        Stat : Status;
        Next : Resource;
     end record;
     type Allocator is array (Valid_Resource) of Cell;
     type Model is record 
        Available : Sequence;
        Allocated : Set;
     end record;

 - Models of Imperative Containers

   For each container type, the library provides model functions that are used
   to annotate subprograms from the API. The different models supply different
   levels of abstraction of the container’s functionalities.

    * The higher level view of a container is usually the mathematical
      structure of element it represents. We use a sequence for ordered
      containers such as list and vectors and a mathematical map for imperative
      maps. This allows us to specify the effects of a subprogram in a very high
      level view, not having to consider cursors nor order of elements in a
      map::

        procedure Increment_All (L : in out List) with
          Post => (for all N in 1 .. Length (L) =>
                   Element (Model (L), N) =  Element (Model (L)'Old, N) + 1);
        procedure Increment_All (S : in out Map) with
          Post =>  (for all K of Model (S)'Old => Mem (Model (S), K))
              and then (for all K of Model (S) => Mem (Model (S)'Old, K)
                   and then Get (Model (S), K)  = Get (Model (S)'Old, K) + 1);

    * For maps, there is a lower level model representing the underlying order
      used for iteration in the map. It is a sequence of keys. We can use it if
      we want to specify in ``Increment_All`` on maps that the order in the keys
      are preserved::

        procedure Increment_All (S : in out Map) with
          Post => S_Keys (S) = S_Keys (S)'Old
             and then (for all K of Model (S) =>
                       Get (Model (S), K) = Get (Model (S)'Old, K) + 1);

    * Finally, cursors are modeled using a functional map linking them to their
      position in the container. For example, we can state that the positions
      of cursors in a list are not modified by a call to ``Increment_All``::

        procedure Increment_All (L : in out List) with
          Post => Positions (L) = Positions (L)'Old
             and then (for all N in 1 .. Length (L) =>
                   Element (Model (L), N) =  Element (Model (L)'Old, N) + 1);
