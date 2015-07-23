pragma Ada_2012;
package body Functional_Sequences with SPARK_Mode => Off is
   function Get (S : Sequence; N : Positive) return Element_Type is
     (Element (S, N));
   function Length (S : Sequence) return Natural is
     (Natural (Length (Vector (S))));

   function "=" (S1, S2 : Sequence) return Boolean is
     (Length (S1) = Length (S2)
      and then (for all N in 1 .. Natural (Length (Vector (S1))) =>
                   Get (S1, N) = Get (S2, N)));

   function Set (S : Sequence; N : Positive; E : Element_Type) return Sequence
   is
      SS : Sequence := Copy (S);
   begin
      Replace_Element (SS, N, E);
      return SS;
   end Set;

   function Add (S : Sequence; E : Element_Type) return Sequence
   is
      SS : Sequence := Copy (S);
   begin
      Append (SS, E);
      return SS;
   end Add;
end Functional_Sequences;
