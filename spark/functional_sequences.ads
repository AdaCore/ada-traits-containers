pragma Ada_2012;
with Ada.Containers;
with Ada.Containers.Vectors;

generic
   type Element_Type is private;
   with function "=" (E1, E2 : Element_Type) return Boolean;
package Functional_Sequences with SPARK_Mode is
   type Sequence is private;
   Empty : constant Sequence;

   function Length (S : Sequence) return Natural;
   function Get (S : Sequence; N : Positive) return Element_Type with
     Pre => N in 1 .. Length (S);

   function "=" (S1, S2 : Sequence) return Boolean with
     Post => "="'Result =
       (Length (S1) = Length (S2)
        and then (for all N in 1 .. Length (S1) =>
            Get (S1, N) = Get (S2, N)));

   function Set (S : Sequence; N : Positive; E : Element_Type) return Sequence
   with
     Pre => N in 1 .. Length (S),
     Post => Length (Set'Result) = Length (S)
     and then Get (Set'Result, N) = E
     and then (for all M in 1 .. Length (S) =>
                   (if M /= N then Get (Set'Result, M) = Get (S, M)));
   function Add (S : Sequence; E : Element_Type) return Sequence
   with
     Post => Length (Add'Result) = Length (S) + 1
     and then Get (Add'Result, Length (Add'Result)) = E
     and then (for all M in 1 .. Length (S) =>
                   Get (Add'Result, M) = Get (S, M));
private
   pragma SPARK_Mode (Off);

   package Element_Lists is new Ada.Containers.Vectors
     (Element_Type => Element_Type,
      Index_Type   => Positive,
      "="          => "=");
   use Element_Lists;

   type Sequence is new Vector with null record;

   Empty : constant Sequence := Sequence'(Empty_Vector with null record);
end Functional_Sequences;
