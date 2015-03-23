--  A generic and general list implementation
--  By providing appropriate values for the formal parameters, the same
--  implementation can be used for bounded and unbounded containers, or for
--  constrained and unconstrained elements.

--  Design: in C++ STL, none of the methods are virtual, so there is no
--  dynamic dispatching. We achieve the same here by using 'Class parameters.
--  This still let's use Ada2012 dot notation (the reason why we use a tagged
--  type, in addition to the Iterable aspect), while increasing the
--  performance (the count-with-explicit-loop goes from 0.25s to 0.51s when we
--  do not use 'Class parameters).

pragma Ada_2012;
with Conts.Generic_List_Nodes;
with Ada.Finalization;      use Ada.Finalization;

generic
   with package All_Nodes is new Conts.Generic_List_Nodes (<>);
  
   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.
  
package Conts.Lists_Impl is
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

end Conts.Lists_Impl;
