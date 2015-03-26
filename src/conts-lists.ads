--  This package provides various generic packages used for the implementation
--  of the various list containers.
--  It is in fact possible to customize the containers to change the way the
--  nodes are stored, for instance, although default implementations are
--  provided for bounded/unbounded containers, and constrained/unconstrained
--  elements.

pragma Ada_2012;
with Ada.Finalization; use Ada.Finalization;

package Conts.Lists is

   type Controlled_Base_List is abstract new Controlled with null record;
   type Limited_Base_List is abstract tagged limited null record;
   --  All lists have a common ancestor, although this cannot be used to
   --  reuse code. This base type is only needed so that we can implement
   --  bounded containers and still share the code.

   -----------------
   -- Node traits --
   -----------------
   --  The following packages are used to describe some types of nodes that
   --  can be used to build a list. We use a different type depending on
   --  whether we have a bounded or unbounded list, for instance. Other
   --  implementations are possible to adapt to existing data structures,
   --  for instance.

   generic
      with package Elements is new Elements_Traits (<>);
      --  The type of elements stored in nodes

      type Container (<>) is abstract tagged limited private;
      --  A container for all nodes.
      --  Such a container is not needed when nodes are allocated on the heap
      --  and accessed via pointers; but it is needed when nodes are stored in
      --  an array, for instance.
      --  This is used as the ancestor type for the list types.

      type Node_Access is private;
      --  Access to a node. This is either an actual pointer or an index into
      --  some other data structure.

      Null_Access : Node_Access;

      with procedure Allocate
         (Nodes : in out Container'Class;
          Element  : Elements.Stored_Element_Type;
          New_Node : out Node_Access);
      --  Allocate a new node, that contains Element. Its next and previous
      --  siblings have been initialized to Null_Access.
      --  This procedure can return Null_Access is the new node could not be
      --  allocated.

      with function Get_Element
         (Nodes    : Container'Class;
          Position : Node_Access) return Elements.Stored_Element_Type is <>;
      with function Get_Next
         (Nodes    : Container'Class;
          Position : Node_Access) return Node_Access is <>;
      with function Get_Previous
         (Nodes    : Container'Class;
          Position : Node_Access) return Node_Access is <>;
      --  Get the next and previous elements for a node

      with procedure Set_Next
         (Nodes    : in out Container'Class;
          Position : Node_Access;
          Next     : Node_Access) is <>;
      with procedure Set_Previous
         (Nodes    : in out Container'Class;
          Position : Node_Access;
          Previous : Node_Access) is <>;
      --  Change the next and previous elements for a node

   package List_Nodes_Traits is
      --  pragma Unreferenced (Null_Access, Allocate, Get_Element, Get_Next);
      --  pragma Unreferenced (Get_Previous, Set_Next, Set_Previous);
      --  ??? Other packages need those, but the compiler is complaining that
      --  these formal parameters are unused in this package.
   end List_Nodes_Traits;

   ------------------------
   -- Bounded list nodes --
   ------------------------
   --  Such nodes are implemented via an array, so that no dynamic memory
   --  allocation is needed

   generic
      with package Elements is new Elements_Traits (<>);
      type Controlled_Or_Limited is abstract tagged limited private;
   package Bounded_List_Nodes_Traits is

      subtype Stored_Element_Type is Elements.Stored_Element_Type;

      type Node_Access is new Count_Type;
      Null_Node_Access : constant Node_Access := 0;
      type Node is record
         Element        : Stored_Element_Type;
         Previous, Next : Node_Access := Null_Node_Access;
      end record;

      type Nodes_Array is array (Count_Type range <>) of Node;

      type Nodes_List (Capacity : Count_Type) is
         abstract new Controlled_Or_Limited
      with record
         Nodes : Nodes_Array (1 .. Capacity);

         Free  : Integer := 0;   --  head of free nodes list
         --  For a negative value, its absolute value points to the first free
         --  element
      end record;

      procedure Allocate
         (Self    : in out Nodes_List'Class;
          Element : Stored_Element_Type;
          N       : out Node_Access);   --  not inlined
      function Get_Element
         (Self : Nodes_List'Class; N : Node_Access) return Stored_Element_Type
         is (Self.Nodes (Count_Type (N)).Element);
      function Get_Next
         (Self : Nodes_List'Class; N : Node_Access) return Node_Access
         is (Self.Nodes (Count_Type (N)).Next);
      function Get_Previous
         (Self : Nodes_List'Class; N : Node_Access) return Node_Access
         is (Self.Nodes (Count_Type (N)).Previous);
      procedure Set_Next
         (Self : in out Nodes_List'Class; N, Next : Node_Access);
      procedure Set_Previous
         (Self : in out Nodes_List'Class; N, Previous : Node_Access);
      pragma Inline (Set_Next, Set_Previous);
      pragma Inline (Get_Element, Get_Next, Get_Previous);

      package Nodes is new List_Nodes_Traits
         (Elements     => Elements,
          Container    => Nodes_List,
          Node_Access  => Node_Access,
          Null_Access  => Null_Node_Access,
          Allocate     => Allocate);
   end Bounded_List_Nodes_Traits;

   --------------------------
   -- Unbounded_List_Nodes --
   --------------------------
   --  Such nodes are implemented via standard access types.
   --  The cursors are also direct access types to the data.

   generic
      with package Elements is new Elements_Traits (<>);
      type Controlled_Or_Limited is abstract tagged limited private;
   package Unbounded_List_Nodes_Traits is

      subtype Stored_Element_Type is Elements.Stored_Element_Type;

      subtype Nodes_Container is Controlled_Or_Limited;
      --  type Nodes_Container is null record;
      type Node;
      type Node_Access is access Node;
      type Node is record
         Element        : Stored_Element_Type;
         Previous, Next : Node_Access;
      end record;
      procedure Allocate
         (Self    : in out Nodes_Container'Class;
          Element : Stored_Element_Type;
          N       : out Node_Access);
      function Get_Element
         (Self : Nodes_Container'Class; N : Node_Access)
         return Stored_Element_Type
         is (N.Element);
      function Get_Next
         (Self : Nodes_Container'Class; N : Node_Access) return Node_Access
         is (N.Next);
      function Get_Previous
         (Self : Nodes_Container'Class; N : Node_Access) return Node_Access
         is (N.Previous);
      procedure Set_Next
         (Self : in out Nodes_Container'Class; N, Next : Node_Access);
      procedure Set_Previous
         (Self : in out Nodes_Container'Class; N, Previous : Node_Access);
      pragma Inline (Allocate, Set_Next, Set_Previous);
      pragma Inline (Get_Element, Get_Next, Get_Previous);

      package Nodes is new List_Nodes_Traits
         (Elements     => Elements,
          Container    => Nodes_Container,
          Node_Access  => Node_Access,
          Null_Access  => null,
          Allocate     => Allocate);
   end Unbounded_List_Nodes_Traits;

   -----------
   -- Lists --
   -----------
   --  A generic and general list implementation
   --  By providing appropriate values for the formal parameters, the same
   --  implementation can be used for bounded and unbounded containers, or for
   --  constrained and unconstrained elements.
   --
   --  Design: in C++ STL, none of the methods are virtual, so there is no
   --  dynamic dispatching. We achieve the same here by using 'Class
   --  parameters.  This still let's use Ada2012 dot notation (the reason why
   --  we use a tagged type, in addition to the Iterable aspect), while
   --  increasing the performance (the count-with-explicit-loop goes from 0.25s
   --  to 0.51s when we do not use 'Class parameters).

   generic
      with package All_Nodes is new List_Nodes_Traits (<>);

      Enable_Asserts : Boolean := False;
      --  If True, extra asserts are added to the code. Apart from them, this
      --  code runs with all compiler checks disabled.

   package Generic_Lists is
      type List is new All_Nodes.Container with private;
      --  We do not define the Iterable aspect here: this is not allowed,
      --  since the parent type is a generic formal parameter. Instead, we
      --  have to define it in the instantiations of Generic_List.

      subtype Element_Type is All_Nodes.Elements.Element_Type;
      subtype Stored_Element_Type is All_Nodes.Elements.Stored_Element_Type;
      type Cursor is private;

      procedure Append
         (Self    : in out List'Class;
          Element : Element_Type)
         with Global => null,
              Pre    => Length (Self) + 1 <= Capacity (Self);
      --  Append a new element to the list.
      --  Complexity: constant
      --  Raises: Storage_Error if Enable_Asserts is True and the node can't
      --     be allocated.

      function Length (Self : List'Class) return Count_Type
         with Inline => True,
              Global => null;
      --  Return the number of elements in the list.
      --  Complexity: linear  (in practice, constant)

      function Capacity (Self : List'Class) return Count_Type
         with Inline => True,
              Global => null;
      --  Return the maximal number of elements in the list. This will be
      --  Count_Type'Last for unbounded containers.
      --  Complexity: constant

      function Class_Wide_First (Self : List'Class) return Cursor
         with Inline => True,
              Global => null;
      function Class_Wide_Element
         (Self : List'Class; Position : Cursor) return Element_Type
         with Inline => True,
              Global => null,
              Pre    => Class_Wide_Has_Element (Self, Position);
      function Class_Wide_Has_Element
         (Self : List'Class; Position : Cursor) return Boolean
         with Inline => True,
              Global => null;
      function Class_Wide_Next
         (Self : List'Class; Position : Cursor) return Cursor
         with Inline => True,
              Global => null,
              Pre    => Class_Wide_Has_Element (Self, Position);
      function Class_Wide_Previous
         (Self : List'Class; Position : Cursor) return Cursor
         with Inline => True,
              Global => null,
              Pre    => Class_Wide_Has_Element (Self, Position);
      --  We pass the container explicitly for the sake of writing the pre
      --  and post conditions.
      --  Complexity: constant for all cursor operations.
      --
      --  These functions are named with a Class_Wide_ prefix, so that when
      --  they are redefined in the list packages (unbounded_indefinite,...)
      --  users can still use Self.First without creating an ambiguity between
      --  the renaming and the Class_Wide function.

      function Class_Wide_Stored_Element
         (Self : List'Class; Position : Cursor) return Stored_Element_Type
         with Inline => True,
              Global => null,
              Pre    => Class_Wide_Has_Element (Self, Position);
      --  Accessing directly the stored element might be more efficient in a
      --  lot of cases.
      --  ??? Can we prevent users from freeing the pointer (when it is a
      --  pointer), or changing the element in place ?

      procedure Next (Self : List'Class; Position : in out Cursor)
         with Inline => True,
              Global => null,
              Pre    => Class_Wide_Has_Element (Self, Position);

      function First_Primitive (Self : List) return Cursor
         is (Class_Wide_First (Self));
      function Element_Primitive
         (Self : List; Position : Cursor) return Element_Type
         is (Class_Wide_Element (Self, Position));
      function Has_Element_Primitive
         (Self : List; Position : Cursor) return Boolean
         is (Class_Wide_Has_Element (Self, Position));
      function Next_Primitive
         (Self : List; Position : Cursor) return Cursor
         is (Class_Wide_Next (Self, Position));
      pragma Inline (First_Primitive, Element_Primitive);
      pragma Inline (Has_Element_Primitive, Next_Primitive);
      --  These are only needed because the Iterable aspect expects a parameter
      --  of type List instead of List'Class. But then it means that the loop
      --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
      --  using an explicit cursor.

   private
      procedure Adjust (Self : in out List) is null;
      procedure Finalize (Self : in out List);
      --  In case the list is a controlled type, but irrelevant when the list
      --  is not controlled.

      type List is new All_Nodes.Container with record
         Head, Tail : All_Nodes.Node_Access := All_Nodes.Null_Access;
         Size : Natural := 0;
      end record;
      --  controlled just to check for performance for now.
      --  Formal containers should not use controlled types, but it might be
      --  necessary to implement some strategies like automatic memory handling
      --  or copy-on-assign for instance.

      type Cursor is record
         Current : All_Nodes.Node_Access;
      end record;

   end Generic_Lists;
end Conts.Lists;
