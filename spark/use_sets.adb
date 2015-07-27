package body Use_Sets with SPARK_Mode is

   function My_Find (S : My_Sets.Set; E : Integer) return Cursor is
      Cu : Cursor := First (S);
   begin
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Get (Elements (S), I) /= E);
         if Element (S, Cu) = E then
            return Cu;
         end if;
         Next (S, Cu);
      end loop;
      return No_Element;
   end My_Find;

   procedure Apply_F (S : My_Sets.Set; R : in out My_Sets.Set) is
      Cu : Cursor := First (S);
   begin
      Clear (R);
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Capacity (R) = Capacity (R)'Loop_Entry);
         pragma Loop_Invariant (Length (R) <= Get (Positions (S), Cu) - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Mem (Model (R), F (Get (Elements (S), I))));
         pragma Loop_Invariant
           (for all G in Model (R) =>
              (for some I in 1 .. Get (Positions (S), Cu) - 1 =>
                   G = F (Get (Elements (S), I))));
         Include (R, F (Element (S, Cu)));
         Next (S, Cu);
      end loop;
      pragma Assert (for all I in 1 .. Length (S) =>
                       Mem (Model (R), F (Get (Elements (S), I))));
      pragma Assert (for all G in Model (R) =>
                       (for some I in 1 .. Length (S) =>
                            G = F (Get (Elements (S), I))));
   end Apply_F;

   function Are_Disjoint (S1, S2 : My_Sets.Set) return Boolean is
      Cu : Cursor := First (S1);
   begin
      while Has_Element (S1, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S1), Cu) - 1 =>
              not Mem (Model (S2), Get (Elements (S1), I)));
         if Contains (S2, Element (S1, Cu)) then
            pragma Assert (Mem (Model (S1), Element (S1, Cu)));
            return False;
         end if;
         Next (S1, Cu);
      end loop;
      pragma Assert (for all E in Model (S1) =>
                        not Mem (Model (S2), E));
      return True;
   end Are_Disjoint;

   function Are_Disjoint_2 (S1, S2 : My_Sets.Set) return Boolean is
      Cu : Cursor := First (S1);
   begin
      while Has_Element (S1, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S1), Cu) - 1 =>
              not Mem (Model (S2), Get (Elements (S1), I)));
         if Contains (S2, Element (S1, Cu)) then
            return False;
         end if;
         Next (S1, Cu);
      end loop;
      return True;
   end Are_Disjoint_2;

   procedure Union_P (S1 : in out My_Sets.Set; S2 : My_Sets.Set) is
   begin
      Union (S1, S2);
   end Union_P;

end Use_Sets;
