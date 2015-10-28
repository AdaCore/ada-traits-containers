package body Use_Maps with SPARK_Mode is

   function My_Find (S : My_Maps.Map; K : Positive) return Cursor is
      Cu : Cursor := First (S);
   begin
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Get (Keys (S), I) /= K);
         if Key (S, Cu) = K then
            return Cu;
         end if;
         Next (S, Cu);
      end loop;
      return No_Element;
   end My_Find;

   procedure Apply_F (S : My_Maps.Map; R : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      Clear (R);
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Capacity (R) = Capacity (R)'Loop_Entry);
         pragma Loop_Invariant (Length (R) = Get (Positions (S), Cu) - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              (for some G of Model (R) =>
                   F (Get (Model (S), Get (Keys (S), I))) = G));
         pragma Loop_Invariant
           (for all G of Model (R) =>
              (for some E of Model (S) => F (E) = G));
         pragma Loop_Invariant
           (for all K in Model (R) =>
                (for some I in 1 .. Get (Positions (S), Cu) - 1 =>
                     K = Get (Keys (S), I)));
         Include (R, Key (S, Cu), F (Element (S, Cu)));
         Next (S, Cu);
      end loop;
   end Apply_F;

   procedure Apply_F_2 (S : My_Maps.Map; R : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      Clear (R);
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Capacity (R) = Capacity (R)'Loop_Entry);
         pragma Loop_Invariant (Length (R) = Get (Positions (S), Cu) - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Mem (Model (R), Get (Keys (S), I))
            and then Get (Model (R), Get (Keys (S), I)) =
              F (Get (Model (S), Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all K in Model (R) =>
                (for some I in 1 .. Get (Positions (S), Cu) - 1 =>
                     K = Get (Keys (S), I)));
         Include (R, Key (S, Cu), F (Element (S, Cu)));
         Next (S, Cu);
      end loop;
   end Apply_F_2;

   procedure Apply_F_3 (S : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Capacity (S) = Capacity (S)'Loop_Entry);
         pragma Loop_Invariant (Positions (S) = Positions (S)'Loop_Entry);
         pragma Loop_Invariant (Keys (S) = Keys (S)'Loop_Entry);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Get (Model (S), Get (Keys (S), I)) =
                F (Get (Model (S)'Loop_Entry, Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all I in Get (Positions (S), Cu) .. Length (S) =>
              Get (Model (S)'Loop_Entry, Get (Keys (S), I)) =
                Get (Model (S), Get (Keys (S), I)));
         Include (S, Key (S, Cu), F (Element (S, Cu)));
         Next (S, Cu);
      end loop;
   end Apply_F_3;

   procedure Apply_F_4 (S : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Capacity (S) = Capacity (S)'Loop_Entry);
         pragma Loop_Invariant (Positions (S) = Positions (S)'Loop_Entry);
         pragma Loop_Invariant (Keys (S) = Keys (S)'Loop_Entry);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Get (Model (S), Get (Keys (S), I)) =
                F (Get (Model (S)'Loop_Entry, Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all I in Get (Positions (S), Cu) .. Length (S) =>
              Get (Model (S)'Loop_Entry, Get (Keys (S), I)) =
                Get (Model (S), Get (Keys (S), I)));
         Include (S, Key (S, Cu), F (Element (S, Cu)));
         Next (S, Cu);
      end loop;
   end Apply_F_4;

   function Are_Disjoint (S1, S2 : My_Maps.Map) return Boolean is
      Cu : Cursor := First (S1);
   begin
      while Has_Element (S1, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S1), Cu) - 1 =>
              not Mem (Model (S2), Get (Keys (S1), I)));
         if Contains (S2, Key (S1, Cu)) then
            return False;
         end if;
         Next (S1, Cu);
      end loop;
      return True;
   end Are_Disjoint;

   procedure Union_P (S1 : in out My_Maps.Map; S2 : My_Maps.Map) is
      Cu : Cursor := First (S2);
   begin
      while Has_Element (S2, Cu) loop
         pragma Loop_Invariant (Capacity (S1) = Capacity (S1)'Loop_Entry);
         pragma Loop_Invariant
           (Length (S1) < Length (S1)'Loop_Entry + Get (Positions (S2), Cu));
         pragma Loop_Invariant (for all E of Model (S1) => P (E));
         Include (S1, Key (S2, Cu), Element (S2, Cu));
         Next (S2, Cu);
      end loop;
   end Union_P;

end Use_Maps;
