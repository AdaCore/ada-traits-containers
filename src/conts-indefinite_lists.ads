--  Unbounded lists of unconstrained elements

with Conts.Lists_Impl;

generic
   type Element_Type (<>) is private;
   Enable_Asserts : Boolean := False;

package Conts.Indefinite_Lists is

   package Elements is new Unconstrained_Element_Traits (Element_Type);
   package Nodes is new Conts.Lists_Impl.Unbounded_List_Node_Traits
      (Elements.Elements);
   package Lists is new Conts.Lists_Impl.Generic_Lists
      (All_Nodes      => Nodes.Nodes,
       Enable_Asserts => Enable_Asserts);
   use Lists;

   subtype List is Lists.List;
   subtype Cursor is Lists.Cursor;
   subtype Element_Access is Elements.Element_Access;

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
