pragma Ada_2012;
with Formal_Ordered_Sets_Impl;

generic
   type Element_Type (<>) is private;
   with function "<" (E1, E2 : Element_Type) return Boolean is <>;
   with function "=" (E1, E2 : Element_Type) return Boolean is <>;
   --  Comparison over elements. BEWARE: "=" and "<" should be compatible.

package Formal_Ordered_Sets with SPARK_Mode is
   package Impl is new Formal_Ordered_Sets_Impl (Element_Type);

   type Set is new Impl.Base_Set with null record with
     Iterable => (First       => First_Primitive,
                  Next        => Next_Primitive,
                  Has_Element => Has_Element_Primitive,
                  Element     => Element_Primitive);
   --  Iteration over sets can be done over cursors or over elements.

   function Model (Self : Set'Class) return Impl.M.Set is
      (Impl.Model (Self))
   with Ghost;
   pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

end Formal_Ordered_Sets;
