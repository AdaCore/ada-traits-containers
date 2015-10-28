pragma Ada_2012;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Vectors;

generic
   type Key_Type (<>) is private;
   No_Key : Key_Type;
   with function "=" (K1, K2 : Key_Type) return Boolean is <>;
   type Element_Type (<>)  is private;
   with function "=" (E1, E2 : Element_Type) return Boolean is <>;
package Functional_Maps with
  SPARK_Mode,
  Initial_Condition => Is_Empty (Empty)
is
   type Map is private
      with Iterable => (First       => First_Key,
                        Next        => Next_Key,
                        Has_Element => Mem,
                        Element     => Get);
   Empty : constant Map;

   function Mem (M : Map; K : Key_Type) return Boolean with
     Global => null,
     Post   => (if K = No_Key then not Mem'Result);
   function Get (M : Map; K : Key_Type) return Element_Type with
     Global => null,
     Pre    => Mem (M, K);

   function Inc (M1, M2 : Map) return Boolean with
     Global => null,
     Post   => Inc'Result =
       (for all K in M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K));
   function "=" (M1, M2 : Map) return Boolean with
     Global => null,
     Post   => "="'Result =
       ((for all K in M1 => Mem (M2, K)
        and then Get (M2, K) = Get (M1, K))
        and (for all K in M2 => Mem (M1, K)));

   pragma Warnings (Off, "unused variable");
   function Is_Empty (M : Map) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (for all K in M => False);
   pragma Warnings (On, "unused variable");

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   with
     Global => null,
     Post   => Is_Add'Result =
         (K /= No_Key and then not Mem (M, K)
          and then Mem (Result, K) and then Get (Result, K) = E
          and then (for all K in M => Mem (Result, K)
                    and then Get (Result, K) = Get (M, K))
          and then (for all KK in Result => KK = K or Mem (M, KK)));
   function Add (M : Map; K : Key_Type; E : Element_Type) return Map with
     Global => null,
     Pre    => K /= No_Key and then not Mem (M, K),
     Post   => Is_Add (M, K, E, Add'Result);
   function Is_Replace
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   with
     Global => null,
     Post   => Is_Replace'Result =
         (Mem (M, K)
          and then Mem (Result, K)
          and then Get (Result, K) = E
          and then (for all KK in M => Mem (Result, KK)
                    and then
                      (if K /= KK
                         then Get (Result, KK) = Get (M, KK)))
          and then (for all K in Result => Mem (M, K)));
   function Replace (M : Map; K : Key_Type; E : Element_Type) return Map with
     Global => null,
     Pre    => Mem (M, K),
     Post   => Is_Replace (M, K, E, Replace'Result);

   --  For quantification purpose
   function First_Key (M : Map) return Key_Type with
     Global => null;
   function Next_Key (M : Map; K : Key_Type) return Key_Type with
     Global => null,
     Pre    => Mem (M, K);
private
   pragma SPARK_Mode (Off);

   type Element_Access is access all Element_Type;
   package Key_Lists is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Key_Type,
      Index_Type   => Positive,
      "="          => "=");

   package Element_Lists is new Ada.Containers.Indefinite_Vectors
     (Element_Type => Element_Type,
      Index_Type   => Positive,
      "="          => "=");

   type Map is record
      Keys     : Key_Lists.Vector;
      Elements : Element_Lists.Vector;
   end record;

   Empty : constant Map :=
     Map'(Key_Lists.Empty_Vector, Element_Lists.Empty_Vector);
end Functional_Maps;
