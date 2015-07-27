pragma Ada_2012;
package body Functional_Maps with SPARK_Mode => Off is
   use Key_Lists;
   use Element_Lists;

   function Mem (M : Map; K : Key_Type) return Boolean is
     (Find_Index (M.Keys, K) /= Key_Lists.No_Index);
   function Get (M : Map; K : Key_Type) return Element_Type is
     (Element (M.Elements, Find_Index (M.Keys, K)));

   function Inc (M1, M2 : Map) return Boolean is
      I2 : Key_Lists.Extended_Index;
   begin
      for I1 in 1 .. Natural (Length (M1.Keys)) loop
         I2 := Find_Index (M2.Keys, Element (M1.Keys, I1));
         if I2 = Key_Lists.No_Index
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
      and then (for all KK in M => Mem (Result, KK)
                and then
                  (if K /= KK
                   then Get (Result, KK) = Get (M, KK)))
      and then (for all K in Result => Mem (M, K)));

   function Replace (M : Map; K : Key_Type; E : Element_Type) return Map
   is
      MM : Map := (Copy (M.Keys), Copy (M.Elements));
   begin
      Replace_Element (MM.Elements, Find_Index (M.Keys, K), E);
      return MM;
   end Replace;

   function Is_Add
     (M : Map; K : Key_Type; E : Element_Type; Result : Map) return Boolean
   is
     (K /= No_Key and then not Mem (M, K)
      and then Mem (Result, K) and then Get (Result, K) = E
      and then (for all K in M => Mem (Result, K)
                and then Get (Result, K) = Get (M, K))
      and then (for all KK in Result => KK = K or Mem (M, KK)));

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map
   is
      MM : Map := (Copy (M.Keys), Copy (M.Elements));
   begin
      Append (MM.Keys, K);
      Append (MM.Elements, E);
      return MM;
   end Add;

   function First_Key (M : Map) return Key_Type is
     (if Is_Empty (M.Keys) then No_Key else Element (M.Keys, 1));
   function Next_Key (M : Map; K : Key_Type) return Key_Type is
     (if Find_Index (M.Keys, K) in 1 .. Natural (Length (M.Keys)) - 1
      then Element (M.Keys, Find_Index (M.Keys, K) + 1)
      else No_Key);
end Functional_Maps;
