pragma Ada_2012;
package body Functional_Sequences with SPARK_Mode => Off is
   use Element_Lists.Vectors;
   function Get (S : Sequence; N : Index_Type) return Element_Type is
     (Element (S, N));
   function Length (S : Sequence) return Natural is
     (Element_Lists.Vectors.Length (S));

   function "=" (S1, S2 : Sequence) return Boolean is
     (Element_Lists.Vectors.Length (S1) = Element_Lists.Vectors.Length (S2)
      and then
        (for all N in Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) +
                     Element_Lists.Vectors.Length (S1))) =>
                Get (S1, N) = Get (S2, N)));

   function Is_Replace
     (S : Sequence; N : Index_Type; E : Element_Type; Result : Sequence)
      return Boolean is
     (N in Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) +
                     Element_Lists.Vectors.Length (S)))
      and then Length (Element_Lists.Vector (Result)) =
        Length (Element_Lists.Vector (S))
      and then Get (Result, N) = E
      and then
        (for all M in  Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) +
                     Element_Lists.Vectors.Length (S))) =>
             (if M /= N then Get (Result, M) = Get (S, M))));

   function Replace (S : Sequence; N : Index_Type; E : Element_Type)
                     return Sequence
   is
   begin
      return SS : Sequence do
         Assign (SS, S);
         Replace_Element (SS, N, E);
      end return;
   end Replace;

   function Is_Add
     (S : Sequence; E : Element_Type; Result : Sequence) return Boolean is
     (Length (Element_Lists.Vector (Result)) =
        Length (Element_Lists.Vector (S)) + 1
      and then Get (Result, Index_Type'Val
                    ((Index_Type'Pos (Index_Type'First) - 1) +
                       Element_Lists.Vectors.Length (Result))) = E
      and then
        (for all M in Index_Type'First ..
             (Index_Type'Val
                  ((Index_Type'Pos (Index_Type'First) - 1) +
                     Element_Lists.Vectors.Length (S))) =>
              Get (Result, M) = Get (S, M)));

   function Add (S : Sequence; E : Element_Type) return Sequence is
   begin
      return SS : Sequence do
         Assign (SS, S);
         Append (SS, E);
      end return;
   end Add;
end Functional_Sequences;
