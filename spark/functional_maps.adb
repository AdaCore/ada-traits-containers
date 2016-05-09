pragma Ada_2012;
package body Functional_Maps with SPARK_Mode => Off is
   use Key_Lists.Vectors;
   use Element_Lists.Vectors;

   function Find_Key (M : Map; K : Key_Type) return Natural;
   --  Helper function.
   --  Searches for a key in the map and returns the appropriate index.

   function Find_Key (M : Map; K : Key_Type) return Natural is
   begin
      for I in 1 .. Length (M.Keys) loop
         if Element (M.Keys, I) = K then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Key;

   function Mem (M : Map; K : Key_Type) return Boolean is
     (Find_Key (M, K) > 0);

   function Get (M : Map; K : Key_Type) return Element_Type is
     (Element (M.Elements, Find_Key (M, K)));

   function Inc (M1, M2 : Map) return Boolean is
      I2 : Positive;
   begin
      for I1 in 1 .. Natural (Length (M1.Keys)) loop
         I2 := Find_Key (M2, Element (M1.Keys, I1));
         if I2 = 0
           or else Element (M2.Elements, I2) /= Element (M1.Elements, I1)
         then
            return False;
         end if;
      end loop;
      return True;
   end Inc;

   function "=" (M1, M2 : Map) return Boolean is
   begin
      return Inc (M1, M2) and Inc (M2, M1);
   end "=";

   function Is_Empty (M : Map) return Boolean is
     (Is_Empty (M.Keys));

   function Is_Replace
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   is
     (Mem (M, K)
      and then Mem (Result, K)
      and then Get (Result, K) = E
      and then (for all KK of M => Mem (Result, KK)
                and then
                  (if K /= KK
                   then Get (Result, KK) = Get (M, KK)))
      and then (for all K of Result => Mem (M, K)));

   function Replace (M : Map; K : Key_Type; E : Element_Type) return Map
   is
   begin
      return MM : Map do
         Assign (MM.Keys, M.Keys);
         Assign (MM.Elements, M.Elements);
         Replace_Element (MM.Elements, Find_Key (M, K), E);
      end return;
   end Replace;

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   is
     (not Mem (M, K)
      and then Mem (Result, K) and then Get (Result, K) = E
      and then (for all K of M => Mem (Result, K)
                and then Get (Result, K) = Get (M, K))
      and then (for all KK of Result => KK = K or Mem (M, KK)));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map
   is
   begin
      return MM : Map do
         Assign (MM.Keys, M.Keys);
         Assign (MM.Elements, M.Elements);
         Append (MM.Keys, K);
         Append (MM.Elements, E);
      end return;
   end Add;

   function Iter_First (M : Map) return Private_Key is (First (M.Keys));
   function Iter_Has_Element (M : Map; K : Private_Key) return Boolean
   is (Has_Element (M.Keys, K));
   function Iter_Next (M : Map; K : Private_Key) return Private_Key
   is (Next (M.Keys, K));
   function Iter_Element (M : Map; K : Private_Key) return Key_Type
   is (Element (M.Keys, K));
end Functional_Maps;
