pragma Ada_2012;
package body Functional_Maps with SPARK_Mode => Off is
   use Node_Lists;

   function Find (M : Map; K : Key_Type) return Extended_Index;

   function Find (M : Map; K : Key_Type) return Extended_Index is
   begin
      if K = No_Key then
         return No_Index;
      end if;

      for I in 1 .. M.Last_Index loop
         if Element (M, I).Key = K then
            return I;
         end if;
      end loop;
      return No_Index;
   end Find;

   function Mem (M : Map; K : Key_Type) return Boolean is
     (Find (M, K) /= No_Index);
   function Get (M : Map; K : Key_Type) return Element_Type is
     (Element (M, Find (M, K)).Element);

   function Inc (M1, M2 : Map) return Boolean is
      I2 : Extended_Index;
   begin
      for I1 in 1 .. Natural (Length (M1)) loop
         I2 := Find (M2, Element (M1, I1).Key);
         if I2 = No_Index
           or else Element (M2, I2).Element /= Element (M1, I1).Element
         then
            return False;
         end if;
      end loop;
      return True;
   end Inc;

   function Set (M : Map; K : Key_Type; E : Element_Type) return Map
   is
      MM : Map := Copy (M);
   begin
      Replace_Element (MM, Find (M, K), (K, E));
      return MM;
   end Set;

   function Add (M : Map; K : Key_Type; E : Element_Type) return Map
   is
      MM : Map := Copy (M);
   begin
      Append (Vector (MM), (K, E));
      return MM;
   end Add;

   function First_Key (M : Map) return Key_Type is
     (if Is_Empty (M) then No_Key else Element (First (M)).Key);
   function Next_Key (M : Map; K : Key_Type) return Key_Type is
     (if Find (M, K) in 1 .. Natural (Length (M)) - 1
      then Element (M, Find (M, K) + 1).Key
      else No_Key);
end Functional_Maps;
