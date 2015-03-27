--  Bounded lists of constrained elements

pragma Ada_2012;

generic
   type Element_Type is private;

   Enable_Asserts : Boolean := False;
   --  If True, extra asserts are added to the code. Apart from them, this
   --  code runs with all compiler checks disabled.

package Conts.Lists.Definite_Bounded is

   package Elements is new Definite_Elements_Traits (Element_Type);
   package Nodes is new Bounded_List_Nodes_Traits
      (Elements              => Elements.Elements,
       Controlled_Or_Limited => Controlled_Base_List);
   package Lists is new Generic_Lists
      (All_Nodes      => Nodes.Nodes,
       Enable_Asserts => Enable_Asserts);

   subtype Cursor is Lists.Cursor;
   type List (Capacity : Count_Type) is
      new Lists.List (Capacity) with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   package Cursors is new List_Cursors (Lists, List);
end Conts.Lists.Definite_Bounded;
