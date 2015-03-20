with Ada.Unchecked_Deallocation;
with Conts.Lists_Impl;

generic
   type Element_Type (<>) is private;
   Enable_Asserts : Boolean := False;

package Conts.Indefinite_Lists is

   type Element_Access is access all Element_Type;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Element_Type, Element_Access);
   function To_Element_Access (E : Element_Type) return Element_Access
      is (new Element_Type'(E));
   function To_Element_Type (E : Element_Access) return Element_Type
      is (E.all);
   pragma Inline (To_Element_Access, To_Element_Type);
   package Lists is new Conts.Lists_Impl
      (Element_Type        => Element_Type,
       Stored_Element_Type => Element_Access,
       Convert_From        => To_Element_Access,
       Convert_To          => To_Element_Type,
       Release             => Unchecked_Free,
       Enable_Asserts      => Enable_Asserts);
   use Lists;

   subtype List is Lists.List;
   subtype Cursor is Lists.Cursor;

   --  ??? Should we rename all subprograms from Lists for those people
   --  that do not use Ada2012 dot notation, as done in conts-lists.ads

   package Bidirectional_Cursors is new Bidirectional_Cursors_Traits
      (Container    => List'Class,
       Cursor       => Cursor,
       Element_Type => Element_Type);
   package Forward_Cursors renames Bidirectional_Cursors.Forward_Cursors;

   package Bidirectional_Cursors_Access is new Bidirectional_Cursors_Traits
      (Container    => List'Class,
       Cursor       => Cursor,
       Element_Type => Element_Access,
       Element      => Stored_Element);
   package Forward_Cursors_Access
      renames Bidirectional_Cursors_Access.Forward_Cursors;
   --  Another version of cursors that manipulates the Element_Access. These
   --  might be more efficient.
end Conts.Indefinite_Lists;
