pragma Ada_2012;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;

generic
   type Element_Type (<>) is private;
   No_Element : Element_Type;
   with function "=" (E1, E2 : Element_Type) return Boolean is <>;
package Functional_Sets  with
  SPARK_Mode,
  Initial_Condition => Is_Empty (Empty)
is
   type Set is private
      with Iterable => (First       => First_Element,
                        Next        => Next_Element,
                        Has_Element => Mem);
   Empty : constant Set;

   function Mem (S : Set; E : Element_Type) return Boolean with
     Global => null,
     Post   => (if E = No_Element then not Mem'Result);

   function Inc (S1, S2 : Set) return Boolean with
     Global => null,
     Post   => Inc'Result = (for all E in S1 => Mem (S2, E));
   function "=" (S1, S2 : Set) return Boolean with
     Global => null,
     Post   =>
       "="'Result = ((for all E in S1 => Mem (S2, E))
                     and (for all E in S2 => Mem (S1, E)));

   pragma Warnings (Off, "unused variable");
   function Is_Empty (S : Set) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (for all E in S => False);
   pragma Warnings (On, "unused variable");

   function Is_Add (S : Set; E : Element_Type; Result : Set) return Boolean
   with
     Global => null,
     Post   => Is_Add'Result =
       (Mem (Result, E)
        and (for all F in Result => Mem (S, F) or F = E)
        and (for all E in S => Mem (Result, E)));
   function Add (S : Set; E : Element_Type) return Set with
     Global => null,
     Post   => Is_Add (S, E, Add'Result);

   function Is_Intersection (S1, S2, Result : Set) return Boolean with
     Global => null,
     Post   => Is_Intersection'Result =
       ((for all E in Result =>
               Mem (S1, E) and Mem (S2, E))
        and (for all E in S1 =>
               (if Mem (S2, E) then Mem (Result, E))));
   function Intersection (S1, S2 : Set) return Set with
     Global => null,
     Post   => Is_Intersection (S1, S2, Intersection'Result);
   function Is_Union (S1, S2, Result : Set) return Boolean with
     Global => null,
     Post   => Is_Union'Result =
       ((for all E in Result => Mem (S1, E) or Mem (S2, E))
        and (for all E in S1 => Mem (Result, E))
        and (for all E in S2 => Mem (Result, E)));
   function Union (S1, S2 : Set) return Set with
     Global => null,
     Post   => Is_Union (S1, S2, Union'Result);

   --  For quantification purpose
   function First_Element (S : Set) return Element_Type;
   function Next_Element (S : Set; E : Element_Type) return Element_Type with
     Pre => Mem (S, E);
private
   pragma SPARK_Mode (Off);

   package Element_Lists is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Element_Type,
      Index_Type   => Positive,
      "="          => "=");

   type Set is new Element_Lists.Vector with null record;

   Empty : constant Set := Set'(Element_Lists.Empty_Vector with null record);
end Functional_Sets;
