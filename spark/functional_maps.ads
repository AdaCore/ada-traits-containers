pragma Ada_2012;
with Ada.Containers.Vectors;

generic
   type Key_Type is private;
   No_Key : Key_Type;
   type Element_Type is private;
   with function "=" (E1, E2 : Element_Type) return Boolean;
package Functional_Maps with SPARK_Mode is
   type Map is private
      with Iterable => (First       => First_Key,
                        Next        => Next_Key,
                        Has_Element => Mem);
   Empty : constant Map;

   function Mem (M : Map; K : Key_Type) return Boolean with
     Post => (if K = No_Key then not Mem'Result);
   function Get (M : Map; K : Key_Type) return Element_Type with
     Pre => Mem (M, K);

   function Inc (M1, M2 : Map) return Boolean with
     Post => Inc'Result =
       (for all K in M1 => Mem (M2, K) and then Get (M2, K) = Get (M1, K));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map with
     Pre  => K /= No_Key and then not Mem (M, K),
     Post => Mem (Add'Result, K) and then Get (Add'Result, K) = E
     and then (for all K in M => Mem (Add'Result, K)
               and then Get (Add'Result, K) = Get (M, K))
     and then (for all KK in Add'Result => KK = K or Mem (M, KK));
   function Set (M : Map; K : Key_Type; E : Element_Type) return Map with
     Pre  => Mem (M, K),
     Post => Mem (Set'Result, K) and then Get (Set'Result, K) = E
     and then (for all KK in M => Mem (Set'Result, KK)
               and then
                 (if K /= KK then Get (Set'Result, KK) = Get (M, KK)))
     and then (for all K in Set'Result => Mem (M, K));

   --  For quantification purpose
   function First_Key (M : Map) return Key_Type;
   function Next_Key (M : Map; K : Key_Type) return Key_Type with
     Pre => Mem (M, K);
private
   pragma SPARK_Mode (Off);

   type Node_Type is record
      Key     : Key_Type;
      Element : Element_Type;
   end record;

   package Node_Lists is new Ada.Containers.Vectors
     (Element_Type => Node_Type,
      Index_Type   => Positive,
      "="          => "=");

   type Map is new Node_Lists.Vector with null record;

   Empty : constant Map := Map'(Node_Lists.Empty_Vector with null record);
end Functional_Maps;
