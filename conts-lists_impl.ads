pragma Ada_2012;
with Ada.Finalization;   use Ada.Finalization;

generic
   type Element_Type is private;

   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.

   Bounded : Boolean := False;
   --  Whether the container should be bounded (maximum capacity but no memory
   --  allocation) or not.

package Conts.Lists_Impl is
   pragma Suppress (All_Checks);

   type List is tagged private
      with Iterable => (First       => First,
                        Next        => Next,
                        Has_Element => Has_Element,
                        Element     => Element);

   procedure Append
      (Self    : in out List;
       Element : Element_Type)
      with Global => null,
           Pre    => Length (Self) + 1 <= Capacity (Self);
   --  Append a new element to the list.
   --  Complexity: constant
   --  Raises: Storage_Error if Enable_Asserts is True and the node can't
   --     be allocated.

   function Length (Self : List) return Count_Type
      with Inline => True,
           Global => null;
   --  Return the number of elements in the list.
   --  Complexity: linear  (in practice, constant)

   function Capacity (Self : List) return Count_Type
      with Inline => True,
           Global => null;
   --  Return the maximal number of elements in the list. This will be
   --  Count_Type'Last for unbounded containers.
   --  Complexity: constant

   type Cursor is private;
   No_Element : constant Cursor;

   function First (Self : List) return Cursor
      with Inline => True,
           Global => null;
   function Element (Self : List; Position : Cursor) return Element_Type
      with Inline => True,
           Global => null,
           Pre    => Has_Element (Self, Position);
   function Has_Element (Self : List; Position : Cursor) return Boolean
      with Inline => True,
           Global => null;
   function Next (Self : List; Position : Cursor) return Cursor
      with Inline => True,
           Global => null,
           Pre    => Has_Element (Self, Position) or else
                     Position = No_Element;
   --  We pass the container explicitly for the sake of writing the pre
   --  and post conditions.
   --  Complexity: constant for all cursor operations.

   procedure Next (Self : List; Position : in out Cursor)
      with Inline => True,
           Global => null,
           Pre    => Has_Element (Self, Position) or else
                     Position = No_Element;

   generic
      with function ">" (E1, E2 : Element_Type) return Boolean is <>;
   function Native_Count_If_Greater_Than (Self : List; E2 : Element_Type) return Natural;
   --  Tmp, for measuring performance

private
   type Node;
   type Node_Access is access Node;
   type Node is record
      Element  : Element_Type;
      Previous : Node_Access;
      Next     : Node_Access;
      --  A doubly-linked list needs both Previous and Next, but adding
      --  Previous has a significant impact on performance:
      --                               forward-list  doubly-linked   C++
      --       10_000_000 inserts       0.46454        0.52211      0.51946
      --       traversing list          0.150259       0.25763      0.25771
   end record;

   type List is new Controlled with record
      Head, Tail : Node_Access;
      Size : Natural := 0;
   end record;

   type Cursor is record
      Current : Node_Access;
   end record;
   No_Element : constant Cursor := (Current => null);
end Conts.Lists_Impl;
