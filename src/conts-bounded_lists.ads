--  Bounded lists of constrained elements

pragma Ada_2012;
with Conts.Lists_Impl;

generic
   type Element_Type is private;
   Capacity : Count_Type;

   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.

package Conts.Bounded_Lists is

   package Elements is new Constrained_Element_Traits (Element_Type);
   package Nodes is new Conts.Lists_Impl.Bounded_List_Node_Traits
      (Elements.Elements, Capacity => Capacity);
   package Lists is new Conts.Lists_Impl.Generic_Lists
      (All_Nodes      => Nodes.Nodes,
       Enable_Asserts => Enable_Asserts);
   use Lists;

   subtype List is Lists.List;
   subtype Cursor is Lists.Cursor;

   procedure Append
      (Self : in out List'Class; Element : Element_Type) renames Lists.Append;
   function Length (Self : List'Class) return Count_Type renames Lists.Length;
   function First (Self : List'Class) return Cursor renames Lists.First;
   function Element (Self : List'Class; Position : Cursor) return Element_Type
      renames Lists.Element;
   function Has_Element (Self : List'Class; Position : Cursor) return Boolean
      renames Lists.Has_Element;
   function Next (Self : List'Class; Position : Cursor) return Cursor
      renames Lists.Next;
   function Previous (Self : List'Class; Position : Cursor) return Cursor
      renames Lists.Previous;
   --  Renames for all the subprograms in Lists, for people that do not use
   --  the Ada2012 notation for primitive operations.

   package Bidirectional_Cursors is new Bidirectional_Cursors_Traits
      (Container    => List'Class,
       Cursor       => Cursor,
       Element_Type => Element_Type);
   package Forward_Cursors renames Bidirectional_Cursors.Forward_Cursors;

end Conts.Bounded_Lists;
