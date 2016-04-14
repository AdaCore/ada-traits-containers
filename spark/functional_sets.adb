pragma Ada_2012;

package body Functional_Sets with SPARK_Mode => Off is
   use Element_Lists.Vectors;

   function Find_Element (S : Set; E : Element_Type) return Natural;
   --  Helper function.
   --  Searches for an element in the set and returns the appropriate index.

   function Find_Element (S : Set; E : Element_Type) return Natural is
   begin
      for I in 1 .. Length (S) loop
         if Element (S, I) = E then
            return I;
         end if;
      end loop;
      return 0;
   end Find_Element;

   function Mem (S : Set; E : Element_Type) return Boolean is
      (Find_Element (S, E) > 0);

   function Inc (S1, S2 : Set) return Boolean is
      I2 : Natural;
   begin
      for I1 in 1 .. Natural (Length (S1)) loop
         I2 := Find_Element (S2, Element (S1, I1));
         if I2 = 0 then
            return False;
         end if;
      end loop;
      return True;
   end Inc;

   function "=" (S1, S2 : Set) return Boolean is
   begin
      return Inc (S1, S2) and Inc (S2, S1);
   end "=";

   function Is_Empty (S : Set) return Boolean is
     (Is_Empty (Vector (S)));

   function Is_Add (S : Set; E : Element_Type; Result : Set) return Boolean
   is
     (Mem (Result, E)
      and (for all F in Result => Mem (S, F) or F = E)
      and (for all E in S => Mem (Result, E)));

   function Add (S : Set; E : Element_Type) return Set is
   begin
      return SS : Set do
         Assign (SS, S);
         Append (SS, E);
      end return;
   end Add;


   function Is_Intersection (S1, S2, Result : Set) return Boolean is
     ((for all E in Result =>
            Mem (S1, E) and Mem (S2, E))
      and (for all E in S1 =>
               (if Mem (S2, E) then Mem (Result, E))));
   function Intersection (S1, S2 : Set) return Set is
   begin
      return SS : Set do
         for I in 1 .. Length (S1) loop
            if Find_Element (S2, Element (S1, I)) > 0 then
               Append (SS, Element (S1, I));
            end if;
         end loop;
      end return;
   end Intersection;

   function Is_Union (S1, S2, Result : Set) return Boolean is
     ((for all E in Result => Mem (S1, E) or Mem (S2, E))
      and (for all E in S1 => Mem (Result, E))
      and (for all E in S2 => Mem (Result, E)));

   function Union (S1, S2 : Set) return Set is
   begin
      return SS : Set do
         Assign (SS, S1);
         for I in 1 .. Length (S2) loop
            declare
               E : Element_Type renames Element (S2, I);
            begin
               if Find_Element (SS, E) = 0 then
                  Append (SS, Element (S2, I));
               end if;
            end;
         end loop;
      end return;
   end Union;

   function First_Element (S : Set) return Element_Type is
      (if Is_Empty (Vector (S)) then No_Element else Element (S, 1));
   function Next_Element (S : Set; E : Element_Type) return Element_Type is
     (if Find_Element (S, E) in 1 .. Natural (Length (S)) - 1
      then Element (S, Find_Element (S, E) + 1)
      else No_Element);
end Functional_Sets;
