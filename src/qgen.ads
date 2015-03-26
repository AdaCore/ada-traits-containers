with Conts.Lists;    use Conts, Conts.Lists;

package QGen is

   --  This package checks that an organization as used in the QGen project
   --  is compatible with our proposal.
   --  We have a hierarchy of types, a matching hierarchy of lists, and when
   --  we use a for..of loop on a list, we get the corresponding child type.
   --  We do not want to duplicate the instance of lists, the goal is to
   --  generate minimal additional code since there are hundreds of such
   --  types in QGen.

   type EObject is abstract tagged record
      Id : Integer;
   end record;
   type Block is new EObject with null record;
   type Sum is new Block with null record;

   --  We do our own instances (not the ones in
   --  Conts.Lists.Indefinite_Unbounded) for better sharing of code.

   package Elements is new Indefinite_Elements_Traits (EObject'Class);
   package Nodes is new Unbounded_List_Nodes_Traits
      (Elements.Elements, Controlled_Base_List);
   package Lists is new Generic_Lists (Nodes.Nodes);

   type EObject_List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   type Block_List is new EObject_List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => As_Block);
   function As_Block (C : Block_List; P : Lists.Cursor) return Block'Class
      is (Block'Class (Lists.Element (C, P)))
      with Inline => True;

   type Sum_List is new Block_List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => As_Sum);
   function As_Sum (C : Sum_List; P : Lists.Cursor) return Sum'Class
      is (Sum'Class (Lists.Element (C, P)))
      with Inline => True;

   procedure Test_QGen;

end QGen;
