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
   No_Element : constant Cursor := Lists.No_Element;

   --  ??? Should we rename all subprograms from Lists for those people
   --  that do not use Ada2012 dot notation, as done in conts-lists.ads

   package Forward_Cursors is new Forward_Cursors_Traits
      (Container    => List'Class,
       Cursor       => Cursor,
       Element_Type => Element_Type,
       No_Element   => No_Element);
end Conts.Indefinite_Lists;
