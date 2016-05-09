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
         Cu := Next (S, Cu);
      end loop;
      return No_Element;
   end My_Find;

   procedure Apply_F (S : My_Maps.Map; R : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      Clear (R);
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Length (R) = Get (Positions (S), Cu) - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              (for some K of R =>
                   Element (Model (R), K) =
                 F (Element (Model (S), Get (Keys (S), I)))));
         pragma Loop_Invariant
           (for all K of R =>
                (for some I in 1 .. Get (Positions (S), Cu) - 1 =>
                     Element (Model (R), K) =
                   F (Element (Model (S), Get (Keys (S), I)))));
         pragma Loop_Invariant
           (for all I in Get (Positions (S), Cu) .. Length (S) =>
                 not Mem (Model (R), Get (Keys (S), I)));
         Set (R, Key (S, Cu), F (Element (S, Cu)));
         Cu := Next (S, Cu);
      end loop;
   end Apply_F;

   procedure Apply_F_2 (S : My_Maps.Map; R : in out My_Maps.Map) is
      Cu : Cursor := First (S);
   begin
      Clear (R);
      while Has_Element (S, Cu) loop
         pragma Loop_Invariant (Length (R) = Get (Positions (S), Cu) - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Get (Positions (S), Cu) - 1 =>
              Mem (Model (R), Get (Keys (S), I))
            and then Element (Model (R), Get (Keys (S), I)) =
              F (Element (Model (S), Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all K of R =>
                (for some I in 1 .. Get (Positions (S), Cu) - 1 =>
                     K = Get (Keys (S), I)));
         Set (R, Key (S, Cu), F (Element (S, Cu)));
         Cu := Next (S, Cu);
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
              Element (Model (S), Get (Keys (S), I)) =
                F (Element (Model (S)'Loop_Entry, Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all I in Get (Positions (S), Cu) .. Length (S) =>
              Element (Model (S)'Loop_Entry, Get (Keys (S), I)) =
                Element (Model (S), Get (Keys (S), I)));
         Set (S, Key (S, Cu), F (Element (S, Cu)));
         Cu := Next (S, Cu);
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
              Element (Model (S), Get (Keys (S), I)) =
                F (Element (Model (S)'Loop_Entry, Get (Keys (S), I))));
         pragma Loop_Invariant
           (for all I in Get (Positions (S), Cu) .. Length (S) =>
              Element (Model (S)'Loop_Entry, Get (Keys (S), I)) =
                Element (Model (S), Get (Keys (S), I)));
         Set (S, Key (S, Cu), F (Element (S, Cu)));
         Cu := Next (S, Cu);
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
         Cu := Next (S1, Cu);
      end loop;
      return True;
   end Are_Disjoint;

   procedure Union_P (S1 : in out My_Maps.Map; S2 : My_Maps.Map) is
      Cu : Cursor := First (S2);
   begin
      while Has_Element (S2, Cu) loop
         pragma Loop_Invariant
           (Length (S1) < Length (S1)'Loop_Entry + Get (Positions (S2), Cu));
         pragma Loop_Invariant
           (for all K of S1 => P (Get (Model (S1), K)));
         Set (S1, Key (S2, Cu), Element (S2, Cu));
         Cu := Next (S2, Cu);
      end loop;
   end Union_P;

   function Q (E : Integer) return Boolean is
   begin
      return E >= 0;
   end Q;

   procedure From_Keys_To_Model (S : My_Maps.Map) is null;

   procedure From_Model_To_Keys (S : My_Maps.Map) is null;

   procedure From_Keys_To_Cursors (S : My_Maps.Map) is null;

   procedure From_Cursors_To_Keys (S : My_Maps.Map) is
   begin
      Formal_Model.Lift_Abstraction_Level (S);
   end From_Cursors_To_Keys;

   procedure From_Model_To_Cursors (S : My_Maps.Map) is null;

   procedure From_Cursors_To_Model (S : My_Maps.Map) is
   begin
      Formal_Model.Lift_Abstraction_Level (S);
   end From_Cursors_To_Model;

end Use_Maps;
