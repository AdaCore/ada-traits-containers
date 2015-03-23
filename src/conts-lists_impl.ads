pragma Ada_2012;
with Ada.Finalization;      use Ada.Finalization;

package Conts.Lists_Impl is

   -----------------
   -- Node traits --
   -----------------
   --  The following packages are used to describe some types of nodes that
   --  can be used to build a list. We use a different type depending on
   --  whether we have a bounded or unbounded list, for instance. Other
   --  implementations are possible to adapt to existing data structures,
   --  for instance.
   --
   --  ??? This concept of setting and getting values is what is called a
   --  'property map' in Boost Graph, and a is way to abstract whether the
   --  information is stored in a node, or externally.
   --  If the getters and setters are marked as inline, the cost of using
   --  an indirection via a generic package is negligible.

   generic
      with package Elements is new Element_Traits (<>);
      --  The type of elements stored in nodes
     
      type Container is private;
      --  A container for all nodes (if applicable, otherwise it could be a
      --  null record when nodes are allocated on the heap via malloc).
   
      type Node_Access is private;
      --  Access to a node. This is either an actual pointer or an index into
      --  some other data structure.
   
      Null_Access : Node_Access;
      --  ??? We could use a Is_Valid function instead, might be more general
   
      with procedure Allocate
         (Nodes : in out Container;
          Element  : Elements.Stored_Element_Type;
          New_Node : out Node_Access);
      --  Allocate a new node, that contains Element. Its next and previous
      --  siblings have been initialized to Null_Access.
      --  This procedure can return Null_Access is the new node could not be
      --  allocated.
   
      with function Get_Element
         (Nodes    : Container;
          Position : Node_Access) return Elements.Stored_Element_Type is <>;
      with function Get_Next
         (Nodes    : Container;
          Position : Node_Access) return Node_Access is <>;
      with function Get_Previous
         (Nodes    : Container;
          Position : Node_Access) return Node_Access is <>;
      --  Get the next and previous elements for a node
   
      with procedure Set_Next
         (Nodes    : in out Container;
          Position : Node_Access;
          Next     : Node_Access) is <>;
      with procedure Set_Previous
         (Nodes    : in out Container;
          Position : Node_Access;
          Previous : Node_Access) is <>;
      --  Change the next and previous elements for a node
   
   package List_Node_Traits is
      --  pragma Unreferenced (Null_Access, Allocate, Get_Element, Get_Next);
      --  pragma Unreferenced (Get_Previous, Set_Next, Set_Previous);
      --  ??? Other packages need those, but the compiler is complaining that
      --  these formal parameters are unused in this package.
   end List_Node_Traits;

   ------------------------
   -- Bounded list nodes --
   ------------------------
   --  Common code for all nodes of bounded lists.
   --  Such nodes are implemented via an array, so that no dynamic memory
   --  allocation is needed
   
   generic
      with package Elements is new Element_Traits (<>);
      Capacity : Count_Type;
   package Bounded_List_Node_Traits is
   
      subtype Stored_Element_Type is Elements.Stored_Element_Type;
   
      type Node_Access is new Count_Type;
      Null_Node_Access : constant Node_Access := 0;
      type Node is record
         Element        : Stored_Element_Type;
         Previous, Next : Node_Access := Null_Node_Access;
      end record;
   
      type Nodes_Array is array (Node_Access range <>) of Node;
   
      type Nodes_Container is record
         Nodes : Nodes_Array (1 .. Node_Access (Capacity));
   
         Free  : Integer := 0;   --  head of free nodes list
         --  For a negative value, its absolute value points to the first free
         --  element
      end record;
   
      procedure Allocate
         (Self    : in out Nodes_Container;
          Element : Stored_Element_Type;
          N       : out Node_Access);
      function Get_Element
         (Self : Nodes_Container; N : Node_Access) return Stored_Element_Type
         is (Self.Nodes (N).Element);
      function Get_Next
         (Self : Nodes_Container; N : Node_Access) return Node_Access
         is (Self.Nodes (N).Next);
      function Get_Previous
         (Self : Nodes_Container; N : Node_Access) return Node_Access
         is (Self.Nodes (N).Previous);
      procedure Set_Next
         (Self : in out Nodes_Container; N, Next : Node_Access);
      procedure Set_Previous
         (Self : in out Nodes_Container; N, Previous : Node_Access);
      pragma Inline (Allocate, Set_Next, Set_Previous);
      pragma Inline (Get_Element, Get_Next, Get_Previous);
   
      package Nodes is new List_Node_Traits
         (Elements     => Elements,
          Container    => Nodes_Container,
          Node_Access  => Node_Access,
          Null_Access  => Null_Node_Access,
          Allocate     => Allocate);
   end Bounded_List_Node_Traits;

   --------------------------
   -- Unbounded_List_Nodes --
   --------------------------
   --  Common code for all nodes of unbounded lists.
   --  Such nodes are implemented via standard access types

   generic
      with package Elements is new Element_Traits (<>);
   package Unbounded_List_Node_Traits is
   
      subtype Stored_Element_Type is Elements.Stored_Element_Type;
   
      type Nodes_Container is null record;
      type Node;
      type Node_Access is access Node;
      type Node is record
         Element        : Stored_Element_Type;
         Previous, Next : Node_Access;
      end record;
      procedure Allocate
         (Self    : in out Nodes_Container;
          Element : Stored_Element_Type;
          N       : out Node_Access);
      function Get_Element
         (Self : Nodes_Container; N : Node_Access) return Stored_Element_Type
         is (N.Element);
      function Get_Next
         (Self : Nodes_Container; N : Node_Access) return Node_Access
         is (N.Next);
      function Get_Previous
         (Self : Nodes_Container; N : Node_Access) return Node_Access
         is (N.Previous);
      procedure Set_Next
         (Self : in out Nodes_Container; N, Next : Node_Access);
      procedure Set_Previous
         (Self : in out Nodes_Container; N, Previous : Node_Access);
      pragma Inline (Allocate, Set_Next, Set_Previous);
      pragma Inline (Get_Element, Get_Next, Get_Previous);
   
      package Nodes is new List_Node_Traits
         (Elements     => Elements,
          Container    => Nodes_Container,
          Node_Access  => Node_Access,
          Null_Access  => null,
          Allocate     => Allocate);
   end Unbounded_List_Node_Traits;

   -----------
   -- Lists --
   -----------
   --  A generic and general list implementation
   --  By providing appropriate values for the formal parameters, the same
   --  implementation can be used for bounded and unbounded containers, or for
   --  constrained and unconstrained elements.
   --
   --  Design: in C++ STL, none of the methods are virtual, so there is no
   --  dynamic dispatching. We achieve the same here by using 'Class parameters.
   --  This still let's use Ada2012 dot notation (the reason why we use a tagged
   --  type, in addition to the Iterable aspect), while increasing the
   --  performance (the count-with-explicit-loop goes from 0.25s to 0.51s when we
   --  do not use 'Class parameters).

   generic
      with package All_Nodes is new List_Node_Traits (<>);
     
      Enable_Asserts : Boolean := False;
      --  If True, extra asserts are added to the code. Apart from them, this
      --  code runs with all compiler checks disabled.
     
   package Generic_Lists is
      --  A doubly-linked list needs both Previous and Next, but adding
      --  Previous has a significant impact on performance:
      --                               forward-list  doubly-linked   C++
      --       10_000_000 inserts       0.46454        0.52211      0.51946
      --       traversing list          0.150259       0.25763      0.25771
      
      type List is tagged private
         with Iterable => (First       => First_Primitive,
                           Next        => Next_Primitive,
                           Has_Element => Has_Element_Primitive,
                           Element     => Element_Primitive);
   
      subtype Element_Type is All_Nodes.Elements.Element_Type;
      subtype Stored_Element_Type is All_Nodes.Elements.Stored_Element_Type;
      
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
      
      type Cursor is private;
      
      function First (Self : List'Class) return Cursor
         with Inline => True,
              Global => null;
      function Element
         (Self : List'Class; Position : Cursor) return Element_Type
         with Inline => True,
              Global => null,
              Pre    => Has_Element (Self, Position);
      function Has_Element (Self : List'Class; Position : Cursor) return Boolean
         with Inline => True,
              Global => null;
      function Next (Self : List'Class; Position : Cursor) return Cursor
         with Inline => True,
              Global => null,
              Pre    => Has_Element (Self, Position);
      function Previous (Self : List'Class; Position : Cursor) return Cursor
         with Inline => True,
              Global => null,
              Pre    => Has_Element (Self, Position);
      --  We pass the container explicitly for the sake of writing the pre
      --  and post conditions.
      --  Complexity: constant for all cursor operations.
      
      function Stored_Element
         (Self : List'Class; Position : Cursor) return Stored_Element_Type
         with Inline => True,
              Global => null,
              Pre    => Has_Element (Self, Position);
      --  Accessing directly the stored element might be more efficient in a lot
      --  of cases.
      --  ??? Can we prevent users from freeing the pointer (when it is a
      --  pointer), or changing the element in place ?
      
      procedure Next (Self : List'Class; Position : in out Cursor)
         with Inline => True,
              Global => null,
              Pre    => Has_Element (Self, Position);
      
      function First_Primitive (Self : List) return Cursor is (First (Self));
      function Element_Primitive
         (Self : List; Position : Cursor) return Element_Type
         is (Element (Self, Position));
      function Has_Element_Primitive
         (Self : List; Position : Cursor) return Boolean
         is (Has_Element (Self, Position));
      function Next_Primitive
         (Self : List; Position : Cursor) return Cursor
         is (Next (Self, Position));
      pragma Inline (First_Primitive, Element_Primitive);
      pragma Inline (Has_Element_Primitive, Next_Primitive);
      --  These are only needed because the Iterable aspect expects a parameter
      --  of type List instead of List'Class. But then it means that the loop is
      --  doing a lot of dynamic dispatching, and is twice as slow as a loop using
      --  an explicit cursor.
      
   private
      type List is new Controlled with record
         Nodes : All_Nodes.Container;
         Head, Tail : All_Nodes.Node_Access;
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
end Conts.Lists_Impl;
