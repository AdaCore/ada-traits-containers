pragma Ada_2012;
package body Functional_Sequences with SPARK_Mode => Off is
   use Element_Lists.Vectors;
   function Get (S : Sequence; N : Positive) return Element_Type is
     (Element (S, N));
   function Length (S : Sequence) return Natural is
     (Length (Element_Lists.Vector (S)));

   function "=" (S1, S2 : Sequence) return Boolean is
     (Length (Element_Lists.Vector (S1)) = Length (Element_Lists.Vector (S2))
      and then
        (for all N in 1 .. Natural (Length (Element_Lists.Vector (S1))) =>
              Get (S1, N) = Get (S2, N)));

   function Is_Replace
     (S : Sequence; N : Positive; E : Element_Type; Result : Sequence)
      return Boolean is
     (N in 1 .. Length (Element_Lists.Vector (S))
      and then Length (Element_Lists.Vector (Result)) =
        Length (Element_Lists.Vector (S))
      and then Get (Result, N) = E
      and then
        (for all M in 1 .. Natural (Length (Element_Lists.Vector (S))) =>
             (if M /= N then Get (Result, M) = Get (S, M))));

   function Replace (S : Sequence; N : Positive; E : Element_Type)
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
      and then Get (Result, Length (Element_Lists.Vector (Result))) = E
      and then
        (for all M in 1 .. Natural (Length (Element_Lists.Vector (S))) =>
              Get (Result, M) = Get (S, M)));

   function Add (S : Sequence; E : Element_Type) return Sequence is
   begin
      return SS : Sequence do
         Assign (SS, S);
         Append (SS, E);
      end return;
   end Add;
end Functional_Sequences;
