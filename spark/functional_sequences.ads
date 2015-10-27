pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;

generic
   type Element_Type (<>) is private;
   with function "=" (E1, E2 : Element_Type) return Boolean;
package Functional_Sequences  with
  SPARK_Mode,
  Initial_Condition => Natural'(Length (Empty)) = 0
is
   type Sequence is private;
   Empty : constant Sequence;

   function Length (S : Sequence) return Natural with
     Global => null;
   function Get (S : Sequence; N : Positive) return Element_Type with
     Global => null,
     Pre    => N in 1 .. Length (S);

   function "=" (S1, S2 : Sequence) return Boolean with
     Global => null,
     Post   => "="'Result =
       (Length (S1) = Length (S2)
        and then (for all N in 1 .. Length (S1) =>
            Get (S1, N) = Get (S2, N)));

   function Is_Replace
     (S : Sequence; N : Positive; E : Element_Type; Result : Sequence)
      return Boolean
   with
     Global => null,
       Post   => Is_Replace'Result =
         (N in 1 .. Length (S)
          and then Length (Result) = Length (S)
          and then Get (Result, N) = E
          and then (for all M in 1 .. Length (S) =>
              (if M /= N then Get (Result, M) = Get (S, M))));
   function Replace
     (S : Sequence; N : Positive; E : Element_Type) return Sequence
   with
     Global => null,
     Pre    => N in 1 .. Length (S),
     Post   => Is_Replace (S, N, E, Replace'Result);
   function Is_Add
     (S : Sequence; E : Element_Type; Result : Sequence) return Boolean
   with
     Global => null,
     Post   => Is_Add'Result =
         (Length (Result) = Length (S) + 1
          and then Get (Result, Length (Result)) = E
          and then (for all M in 1 .. Length (S) =>
              Get (Result, M) = Get (S, M)));
   function Add (S : Sequence; E : Element_Type) return Sequence with
     Global => null,
     Post   => Is_Add (S, E, Add'Result);
private
   pragma SPARK_Mode (Off);

   package Element_Lists is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Element_Type,
      Index_Type   => Positive,
      "="          => "=");
   use Element_Lists;

   type Sequence is new Vector with null record;

   Empty : constant Sequence := Sequence'(Empty_Vector with null record);
end Functional_Sequences;
