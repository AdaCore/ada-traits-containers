pragma Ada_2012;
package body Use_Lists with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);

   function My_Find (L : List; E : Element_Type) return Cursor is
      Cu : Cursor := First (L);
   begin
      while Has_Element (L, Cu) loop
         pragma Loop_Invariant
           (for all I in 1 .. P_Get (Positions (L), Cu) - 1 =>
              Element (Model (L), I) /= E);
         if As_Element (L, Cu) = E then
            return Cu;
         end if;
         Next (L, Cu);
      end loop;
      return No_Element;
   end My_Find;

   procedure Incr_All (L1 : My_Bounded_100; L2 : in out My_Bounded_100) is
      Cu : My_Bounded_Lists.Cursor := My_Bounded_Lists.Lists.First (L1);
   begin
      My_Bounded_Lists.Lists.Clear (L2);
      while My_Bounded_Lists.Lists.Has_Element (L1, Cu) loop
         pragma Loop_Invariant
           (for all N in 1 .. My_Bounded_Lists.Lists.Length (L2) =>
                Is_Incr (My_Bounded_Lists.Lists.Element
              (My_Bounded_Lists.Lists.Model (L1), N),
              My_Bounded_Lists.Lists.Element
                (My_Bounded_Lists.Lists.Model (L2), N)));
         pragma Loop_Invariant
           (My_Bounded_Lists.Lists.Impl.P_Get
              (My_Bounded_Lists.Lists.Impl.Positions (L1), Cu) =
                My_Bounded_Lists.Lists.Length (L2) + 1);
         if My_Bounded_Lists.Lists.As_Element (L1, Cu) < Element_Type'Last then
            My_Bounded_Lists.Lists.Append
              (L2, My_Bounded_Lists.Lists.As_Element (L1, Cu) + 1);
         else
            My_Bounded_Lists.Lists.Append
              (L2, My_Bounded_Lists.Lists.As_Element (L1, Cu));
         end if;
         My_Bounded_Lists.Lists.Next (L1, Cu);
      end loop;
   end Incr_All;

   procedure Incr_All_2 (L : in out List) is
      Cu : Cursor := First (L);
   begin
      while Has_Element (L, Cu) loop
         pragma Loop_Invariant (Capacity (L) = Capacity (L)'Loop_Entry);
         pragma Loop_Invariant (Length (L) = Length (L)'Loop_Entry);
         pragma Loop_Invariant
           (for all N in 1 .. P_Get (Positions (L), Cu) - 1 =>
                Is_Incr (Element (Model (L)'Loop_Entry, N),
                         Element (Model (L), N)));
         pragma Loop_Invariant
           (for all N in P_Get (Positions (L), Cu) .. Length (L) =>
                Element (Model (L)'Loop_Entry, N) =
                Element (Model (L), N));
         if As_Element (L, Cu) < Element_Type'Last then
            Impl.Replace_Element (L, Cu, As_Element (L, Cu) + 1);
         end if;
         Next (L, Cu);
      end loop;
   end Incr_All_2;

   procedure Incr_All_3 (L : in out List) is
      Cu : Cursor := First (L);
   begin
      while Has_Element (L, Cu) loop
         pragma Loop_Invariant (Capacity (L) = Capacity (L)'Loop_Entry);
         pragma Loop_Invariant (Length (L) = Length (L)'Loop_Entry);
         pragma Loop_Invariant
           (for all N in 1 .. P_Get (Positions (L), Cu) - 1 =>
                Is_Incr (Element (Model (L)'Loop_Entry, N),
                         Element (Model (L), N)));
         pragma Loop_Invariant
           (for all N in P_Get (Positions (L), Cu) .. Length (L) =>
                Element (Model (L)'Loop_Entry, N) =
                Element (Model (L), N));
         pragma Loop_Invariant (Positions (L)'Loop_Entry =
                                  Positions (L));
         if As_Element (L, Cu) < Element_Type'Last then
            Impl.Replace_Element (L, Cu, As_Element (L, Cu) + 1);
         end if;
         Next (L, Cu);
      end loop;
   end Incr_All_3;

   procedure Double_Size (L : in out List) is
      Cu   : Cursor := First (L);
      Lgth : Count_Type := Length (L);
   begin
      for I in 1 .. Lgth loop
         pragma Loop_Invariant (Has_Element (L, Cu));
         pragma Loop_Invariant (Length (L) = Length (L)'Loop_Entry + I - 1);
         pragma Loop_Invariant
           (for all I in 1 .. Length (L)'Loop_Entry =>
                Element (Model (L), I) =
              Element (Model (L)'Loop_Entry, I));
         pragma Loop_Invariant
           (for all J in 1 .. I - 1 =>
              Element (Model (L), J + Length (L)'Loop_Entry) =
                Element (Model (L)'Loop_Entry, J));
         pragma Loop_Invariant
           (P_Get (Positions (L), Cu) = I);
         Append (L, As_Element (L, Cu));
         Next (L, Cu);
      end loop;
   end Double_Size;

   procedure Double_Size_2 (L : in out List) is
      Cu : Cursor := First (L);
      N  : Count_Type := 0 with Ghost;
   begin
      while Has_Element (L, Cu) loop
         pragma Loop_Invariant (Length (L) = Length (L)'Loop_Entry + N);
         pragma Loop_Invariant
           (for all I in 1 .. N =>
              Element (Model (L), 2 * I) =
                Element (Model (L)'Loop_Entry, I)
            and Element (Model (L), 2 * I - 1) =
              Element (Model (L)'Loop_Entry, I));
         pragma Loop_Invariant
           (for all I in N + 1 .. Length (L)'Loop_Entry =>
                Element (Model (L), I + N) =
              Element (Model (L)'Loop_Entry, I));
         pragma Loop_Invariant
           (P_Get (Positions (L), Cu) = 2 * N + 1);
         Impl.Insert (L, Cu, As_Element (L, Cu));
         Next (L, Cu);
         N := N + 1;
      end loop;
   end Double_Size_2;

   procedure Update_Range_To_Zero (L : in out List; Fst, Lst : Cursor) is
      Current : Cursor := Fst;
      N_Last  : Cursor := Lst;
   begin
      Next (L, N_Last);
      while Current /= N_Last loop
         pragma Loop_Invariant (Length (L) = Length (L)'Loop_Entry);
         pragma Loop_Invariant (Positions (L) =
                                  Positions (L)'Loop_Entry);
         pragma Loop_Invariant (P_Mem (Positions (L), Current));
         pragma Loop_Invariant
           (P_Get (Positions (L), Current) in
              P_Get (Positions (L), Fst) ..
                P_Get (Positions (L), Lst));
         pragma Loop_Invariant
           (for all I in
              P_Get (Positions (L), Fst) ..
                P_Get (Positions (L), Current) - 1
            =>
                Element (Model (L), I) = 0);
         pragma Loop_Invariant
           (for all I in 1 .. P_Get (Positions (L), Fst) - 1 =>
              Element (Model (L), I) =
                Element (Model (L)'Loop_Entry, I));
         pragma Loop_Invariant
           (for all I in P_Get (Positions (L), Current) ..
              Length (L) =>
                Element (Model (L), I) =
              Element (Model (L)'Loop_Entry, I));
         Impl.Replace_Element (L, Current, 0);
         Next (L, Current);
      end loop;
   end Update_Range_To_Zero;

   procedure Insert_Count (L : in out List; Cu : Cursor) is
   begin
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
      Impl.Insert (L, Cu, 0);
   end Insert_Count;

   function P (E : Element_Type) return Boolean is
   begin
      return E >= 0;
   end P;

   procedure From_Higher_To_Lower (L : List) is null;

   procedure From_Lower_To_Higher (L : List) is
   begin
      Impl.Lift_Abstraction_Level (L);
   end From_Lower_To_Higher;

end Use_Lists;
