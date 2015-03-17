pragma Ada_2012;
with Ada.Finalization;   use Ada.Finalization;

generic
   type Element_Type is private;

   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.

package Conts.Lists is
   pragma Suppress (All_Checks);

   type List is tagged private
      with Iterable => (First       => First,
                        Next        => Next,
                        Has_Element => Has_Element,
                        Element     => Element);

   procedure Append
      (Self    : in out List;
       Element : Element_Type);

   type Cursor is private;

   function First (Self : List) return Cursor
      with Inline => True;
   function Element (Self : List; Position : Cursor) return Element_Type
      with Inline => True;
   function Has_Element (Self : List; Position : Cursor) return Boolean
      with Inline => True;
   function Next (Self : List; Position : Cursor) return Cursor
      with Inline => True;
   --  We pass the container explicitly for the sake of writing the pre
   --  and post conditions.

   -------------
   -- Cursors --
   -------------

   --  package Forward_Cursors is new Forward_Cursors_Traits
   --     (Container    => List,
   --      Cursor       => Cursor,
   --      Element_Type => Element_Type);

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
   end record;

   type Cursor is record
      Current : Node_Access;
   end record;
end Conts.Lists;
