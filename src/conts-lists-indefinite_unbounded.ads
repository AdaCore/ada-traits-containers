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
      (Nodes          => Nodes.Nodes,
       Enable_Asserts => Enable_Asserts);

   subtype Cursor is Lists.Cursor;
   type List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   package Cursors is new List_Cursors (Lists, List);
end Conts.Lists.Indefinite_Unbounded;
