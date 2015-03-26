--  Unbounded lists of unconstrained elements

generic
   type Element_Type (<>) is private;
   Enable_Asserts : Boolean := False;

package Conts.Lists.Indefinite_Unbounded is

   package Elements is new Indefinite_Elements_Traits (Element_Type);
   package Nodes is new Unbounded_List_Nodes_Traits
      (Elements              => Elements.Elements,
       Controlled_Or_Limited => Controlled_Base_List);
   package Lists is new Generic_Lists
      (All_Nodes      => Nodes.Nodes,
       Enable_Asserts => Enable_Asserts);
   use Lists;

   --  subtype List is Lists.List;
   subtype Cursor is Lists.Cursor;
   subtype Element_Access is Elements.Element_Access;

   type List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   function First (Self : List'Class) return Cursor
      is (Lists.Class_Wide_First (Self));
   function Element (Self : List'Class; Position : Cursor) return Element_Type
      is (Lists.Class_Wide_Element (Self, Position));
   function Stored_Element (Self : List'Class; Position : Cursor)
      return Stored_Element_Type
      is (Lists.Class_Wide_Stored_Element (Self, Position));
   function Has_Element (Self : List'Class; Position : Cursor) return Boolean
      is (Lists.Class_Wide_Has_Element (Self, Position));
   function Next (Self : List'Class; Position : Cursor) return Cursor
      is (Lists.Class_Wide_Next (Self, Position));
   function Previous (Self : List'Class; Position : Cursor) return Cursor
      is (Lists.Class_Wide_Previous (Self, Position));
   pragma Inline (First, Element, Has_Element, Next, Previous);

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
end Conts.Lists.Indefinite_Unbounded;
