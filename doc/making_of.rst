The "making of" of a generic library
====================================

Design axis
-----------

There already exists lots of libraries that provide various sets of containers
for Ada. Why, then, provide yet another one ?

This chapter describes how the usual containers have a very limited genericity,
which limits their actual reuse or configurability. It also describes how we
ended up with the current design, and how this design provides a much better
control.

All modern programing languages provide a set of containers that can be used by
applications. These containers typically include lists, vectors, dictionaries
(or maps) and sets. Other libraries also include graphs data structures.

In all these cases, the implementers of the library have taken a number of
design choices, which all influence various aspects of the containers:

  - **Efficiency**: must be at least as efficient as hand-coded data
    structures, or developers will reimplement their own;

  - **Safety**: must avoid common pitfalls, like dangling pointers,
    thread-safety,... This typically requires additional runtime checks and
    thus conflicts with efficiency;

  - **Reusability**: the containers, and especially their algorithms, should
    apply to various contexts. Typical examples are sorting, merging and graph
    traversals.

  - **Provability**: users of formal verification tools like SPARK should have
    access to these containers, which adds constraints on memory allocations,
    pre and post conditions,...

In C++, the strong emphasis is on performance. The STL is not safe, in the
sense that you can keep using an iterator after the corresponding element has
been deleted, for instance. STL algorithms are very reusable, thanks to the
concept of iterators and template specialization. An algorithm like find for
instance takes the container as parameter, and for a vector will be implemented
as a loop that checks all elements in turn, whereas it can be implemented much
more efficiently for a set.

In Ada, on the other hand, the emphasis is on safety. A large number of checks
are mandated by the standard to ensure that the user is not modifying a
container while iterating, among others. These checks, however, have an impact
on performance. Reusability of the algorithms is not good since the formal
generic parameter must specify which type of container is expected (the
containers do not share anything). It is possible, via pragmas, to disable
container checks and thus improve performance at the cost of safety.

Containers have traditionally been split along two axis: whether they store
elements with a known size (**definite** elements) or not (**indefinite**), and
whether they store a known maximum number of elements (**bounded** containers)
or an unlimited number of elements (**unbounded**). These attributes have an
impact on when and how often memory is allocated, which in turn has a direct
impact on performance.

In C++, these axis are hidden from the user. Thanks to template specialization,
the STL choses automatically the implementation for each containers, although
it can't always do the best choice. In Ada, on the other hand, users have to
chose which variant to use when they instantiate a container. This gives more
control, at the cost of slightly more complexity for users (who must understand
the concepts) and often code duplication in the library.

There are yet other considerations when designing a container library: should
the containers be **thread-safe**, should the memory be allocated in a special
**pool** (Ada calls them storage_pools, C++ calls them allocators), should
storage be **persistant**, etc...

Generic lists
-------------

A library could conceptually provide a specialized implementation for all the
aspects we described above. That would result in a very large library if all
combinations were supported. Fortunately, the proper use of generic programming
(or templates) provides a nice implementation strategy here.

Let's build a simple container, a list, as an example.

.. highlight:: ada

Our list should be able to store any kind of element, including for instance a
`String` whose size is unknown at compile time. Since we do not know the type
of elements, that must be a generic package. This is in general the amount of
genericity provided in standard libraries. The initial code would look like::

   generic
      type Element (<>) is private;
   package Lists is
      type List is private;
      procedure Prepend (Self : in out List; E : Element);
   private
      type Element_Access is access Element;
      type Node_Record;
      type Node is access Node_Record;
      type Node_Record is record
         Value : Element_Access;
         Next  : Node;
      end record;
      type List is record
         Head : Node;
      end record;
   end Lists;

   package body Lists is
     procedure Prepend (Self : in out List; E : Element) is
     begin
        Self.Head := new Node_Record'
          (Value => new Element'(E), Next => Self.Head);
     end Prepend;
   end Lists;

The code declares a list as a pointer to a node, which itself contains an
element and points to the next node in the list. When we add an element in
front of the list, we are just changing the head of the list (using the Ada
aggregate syntax to ensure that all fields of the node are properly initialized
even if we later add another field like a `Tail`).

This implementation works fine, and is in fact almost optimal for an unbounded
list of indefinite elements. We need one allocation for the node, and one for
the element (since we do not know its size statically). However, these two
allocations will make the lists very slow (compared to other implementations)
for a list of integers. An integer always uses the same size, so can be stored
directly in a `Node`, we do not need to allocate memory.

Element Policies
----------------

Let's assume however that the whole code for a list, with all its operations,
is large and we do not want to duplicate it to better handle definite elements.
We will then introduce a new package that describe the storage policy for
elements. The term **policy** has been used before in the C++ world (see [1]).
Another similar term is **strategy**. In the Ada world, the term **signature
package** has been used for a similar concept. Another close concept are the
**traits** packages in C++, although they are generally used to discover
attributes of types at compile time (like the type of elements stored in an
array for instance).

::

   generic
      type Element (<>) is private;
      type Stored is private;
      with function To_Stored (E : Element) return Stored;
      with function To_Element (S : Stored) return Element;
   package Elements_Policy is
   end Elements_Policy;

This package does nothing useful. It is just a way to encapsulate various
pieces of information about the types. For instance, we have declared that an
Element type can be anything (possibly an indefinite or abstract type). Such a
type cannot be stored in a record, so we need an associated type that we can
store, and conversion functions between the two. This declaration is enough to
improve our list package::

   generic
      with package Elements is new Elements_Policy (<>)
   package Lists is
      type List is private;
      procedure Prepend (Self : in out List; E : Elements.Element);
   private
      type Node_Record;
      type Node is access Node_Record;
      type Node_Record is record
         Value : Elements.Stored;
         Next  : Node;
      end record;
      type List is record
         Head : Node;
      end record;
   end Lists;
   
   package body Lists is
     procedure Prepend (Self : in out List; E : Elements.Element) is
     begin
        Self.Head := new Node'
          (Value => Elements.To_Stored (E), Next => Self.Head);
     end Prepend;
   end Lists;

This new version no longer forces an explicit memory allocation for elements.
The formal parameter has changed, too: we no longer take the element type, but
a policy package.  For an integer, an elements policy would be::

   function Identity (A : Integer) return Integer is (A) with Inline;
   package Integer_Elements is new Elements_Policy
     (Element => Integer, Stored => Integer,
      To_Stored => Identity, To_Element => Identity);

   package Integer_Lists is new Lists (Integer_Elements);

No memory allocation needed, the list will be a lot faster, while remaining as
safe.

Instantiating the `Elements_Policy` package needs to be done once for each type
of element that you intend to store, whatever the number or types of containers
they will be stored in. As a helper, this library provides a
`Conts.Elements.Definite` and a `Conts.Elements.Indefinite` packages, which are
trivial to instantiate.

Node Policies
-------------

In our list implementation, there remains one memory allocation which might be
lifted in the case of a bounded container. Such a container has a known maximum
number of elements it can store, so basically we could store all elements in an
array, and avoid allocating memory for each node.

Let's introduce another policy package::

   generic
      with package Elements is new Elements_Policy (<>);
      type Container (<>) is abstract tagged private;
      type Node is private;
      Null_Node : Node;
      with function Allocate 
        (C : in out Container; E : Elements.Element) return Node;
      with procedure Set_Next (N, Next : Node);
   package Storage_Policy is
   end Storage_Policy;

This package needs to know about the elements, since those are stored in the
nodes. Following our previous discussion, we therefore give it the elements
policy. Nodes could be implemented as access types (as we did before), or as
indexed into an array,.. That array, in fact, needs to be stored somewhere,
which is why we also describe what a container will look like.

We can now implement our list as::

   generic
      with package Storage is new Storage_Policy (<>);
   package Lists is
      type List is private;
      procedure Prepend (Self : in out List; E : Elements.Element);
   private
      type List is new Storage.Container with record
        Head : Storage.Node;
      end record;
   end Lists;
   
   package body Lists is
     procedure Prepend (Self : in out List; E : Elements.Element) is
        N : constant Storage.Node := Storage.Allocate (Self, E);
     begin
        Storage.Set_Next (N, Self.Head);
        Self.Head := N;
     end Prepend;
   end Lists;

This list is no longer forcing any allocation. That decision is left to the two
policy packages we use.  If we now want to provide a list of integers that will
contain at most 1000 elements, we can provide a separate implementation for the
nodes policy that does not do any allocation of memory. Compared to doing two
allocation per elements in our original list, this will be much faster.

Let's look at two possible implementations of this policy. The first one ends
up doing the same thing we were doing before. The nodes are allocated in
memory, and therefore we do not really need the container. Note that we have
declared `Allocate` and `Set_Next` as inline. This ensures that the compiler
will in fact generate the same code it did before, as if we had not added an
extra subprogram call. Thus performance is preserved, and yet our list is more
flexible::

   generic
      with package Elements is new Elements_Policy (<>);
   package Unbounded_Storage is
      type Container is tagged null record;
      type Node_Record;
      type Node is access Node_Record;
      type Node_Record is record
         Value : Elements.Stored;
         Next  : Node;
      end record;
      function Allocate 
        (C : in out Container; E : Elements.Element) return Node
        is (new Node_Record'
             (Value => Elements.To_Stored (E), Next => null))
        with Inline;
      procedure Set_Next (N, Next : Node) with Inline;
      package Policy is new Storage_Policy
         (Elements, Container, Node, null, Allocate, Set_Next);
   end Unbounded_Storage;
   
   package body Unbounded_Storage is
      procedure Set_Next (N, Next : Node) is
      begin
         N.Next := Next;
      end Set_Next;
   end Unbounded_Storage;

The second implementation is for a bounded list, with a known maximum
number of elements::

   generic
      with package Elements is new Elements_Policy (<>);
   package Bounded_Storage is
      type Node is new Integer range 0 .. 1000;
      Unset : constant Node := 0;
      type Element_Array is array (Node) of Elements.Stored;
      type Container is tagged record
         Elms : Element_Array;
         Last : Node := Unset;
      end record;
      function Allocate 
        (C : in out Container; E : Elements.Element) return Node
         with Inline;
      procedure Set_Next (N, Next : Node) is null with Inline;
      package Policy is new Storage_Policy
         (Elements, Container, Node, Unset, Allocate, Set_Next);
   end Bounded_Storage;
   
   package body Bounded_Storage is
      function Allocate 
        (C : in out Container; E : Elements.Element) return Node is
      begin
         C.Last := C.Last + 1; 
         C.Elms (C.Last) := Elements.To_Stored (E);
         return C.Last;          
      end Allocate;
   end Bounded_Storage;

Strictly speaking, this is closer to what a vector would be, not a list, but
this is for purposes of illustration.

Controlling the use of memory allocations (and more importantly deallocations)
is also a major need when one is trying to prove code, for instance in the
context of SPARK. Various implementations of the elements policy and nodes
policy can prove more amenable to SPARK than the ones we just saw.

Algorithms
----------

Standard Ada containers have a lot of similarities in their API (they all provide
a `Cursor` type, most of them provide a `Append` or `Include` operation, ...) This
help users to learn the API and use the various types of containers.

However, strictly speaking they have nothing in common (no inheritance for instance),
so one cannot write an algorithm that are container agnostic. For instance, if
we want to write a simple algorithm that counts the number of elements in a
container, we need one implementation for each type of container::

   generic
      with package Lists is new Doubly_Linked_Lists (<>);
   function Count (L : Lists.List) return Natural is
      Result : Natural := 0;
      C : Lists.Cursor := L.First;
   begin
      while Lists.Has_Element (C) loop
         Result := Result + 1;
         Lists.Next (C);
      end loop;
      return Result;
   end Count;

   generic
      with package Vec is new Vectors (<>);
   function Count (V : Vec.Vector) return Natural is
      --  ... same as above

We could actually write the algorithm once if we pass more formal parameters,
as in::

   generic
      type Element is private;
      type Container is private;
      type Cursor is private;
      with function First (Self : Container) return Cursor is <>;
      with function Has_Element
         (Self : Container; C : Cursor) return Boolean is <>;
      with function Element (C : Cursor) return Element is <>;
      with procedure Next (C : in out Cursor) is <>;
   function Count (L : Container) return Natural is
      --  ... same as before

This is now applicable to any type of container (even those not part of the
standard Ada runtime. However, it has suddenly become a lot more work to
instantiate, despise the use of `is <>` so that the compiler defaults to any
visible subprogram with the correct profile.

At this point, we can introduce our third policy package, the **Cursors**
policy. Since we are only interested in moving forward, we'll call these
the `Forward Cursors`. At the same time, we'll ruse our `Element policy`
which, as we saw before, allow us to control memory allocation and storage
of elements::

   generic
      with package Elements is new Elements_Policy (<>);
      type Container is private;
      type Cursor is private;
      with function First (Self : Container) return Cursor is <>;
      with function Has_Element
         (Self : Container; C : Cursor) return Boolean is <>;
      with function Element (C : Cursor) return Elements.Element is <>;
      with procedure Next (C : in out Cursor) is <>;
   package Forward_Cursors_Policy is
   end Forward_Cursors_Policy;

Our algorithms will now all have a similar list of formal parameters, for
instance::

   generic
      with package Cursors is new Forward_Cursors_Policy (<>);
   function Count (L : Cursors.Container) return Natural;

   generic
      with package Cursors is new Forward_Cursors_Policy (<>);
   function Contains
      (L : Cursors.Container; E : Cursors.Elements.Element)
      return Boolean;

Again, instantiating the `Forward_Cursors_Policy` package needs to be done only
once for each type of data structure. In fact, when you are using the data
structures provided in this library, they already provide the necessary
`Cursors` packages, so that you can easily use them with any of the algorithms.

This library also provides a `Conts.Cursors.Adaptors` which has a number of
nested package that will provide cursors policies for the standard Ada
containers. It is thus relatively easy to also reuse the library's algorithms
with the standard Ada containers.

