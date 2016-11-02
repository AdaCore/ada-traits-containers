pragma Ada_2012;
with Conts;         use Conts;

package body Use_Vectors with SPARK_Mode is

   procedure Incr_All (V1 : My_Bounded_100; V2 : in out My_Bounded_100) is
      Lst : Index_Type'Base := My_Bounded_Vectors.Vectors.Last (V1);
   begin
      My_Bounded_Vectors.Vectors.Clear (V2);
      for I in Index_Type'First .. Lst loop
         pragma Loop_Invariant
           (Integer (My_Bounded_Vectors.Vectors.Length (V2)) =
              I - Index_Type'First);
         pragma Loop_Invariant
           (for all N in Index_Type'First ..
              My_Bounded_Vectors.Vectors.Last (V2) =>
                Is_Incr (My_Bounded_Vectors.Vectors.As_Element (V1, N),
              My_Bounded_Vectors.Vectors.As_Element (V2, N)));

         if My_Bounded_Vectors.Vectors.As_Element (V1, I) <
           Element_Type'Last
         then
            My_Bounded_Vectors.Vectors.Append
              (V2, My_Bounded_Vectors.Vectors.As_Element (V1, I) + 1);
         else
            My_Bounded_Vectors.Vectors.Append
              (V2, My_Bounded_Vectors.Vectors.As_Element (V1, I));
         end if;
      end loop;
   end Incr_All;

   procedure Incr_All_2 (V : in out Vector) is
      Lst : Index_Type'Base := Last (V);
   begin
      for I in Index_Type'First .. Lst loop
         pragma Loop_Invariant (Length (V) = Length (V)'Loop_Entry);
         pragma Loop_Invariant
           (for all N in Index_Type'First .. I - 1 =>
                Is_Incr (Element (Model (V)'Loop_Entry, N),
                         As_Element (V, N)));
         pragma Loop_Invariant
           (for all N in I .. Last (V) =>
                Element (Model (V)'Loop_Entry, N) =
                As_Element (V, N));
         if As_Element (V, I) < Element_Type'Last then
            Replace_Element (V, I, Element (V, I) + 1);
         end if;
      end loop;
   end Incr_All_2;

   procedure Incr_All_3 (V : in out Vector) is
      Cu : Cursor := First (V);
   begin
      while Has_Element (V, Cu) loop
         pragma Loop_Invariant (Length (V) = Length (V)'Loop_Entry);
         pragma Loop_Invariant
           (for all N in Index_Type'First .. Cu - 1 =>
                Is_Incr (Element (Model (V)'Loop_Entry, N),
                         As_Element (V, N)));
         pragma Loop_Invariant
           (for all N in Cu .. Last (V) =>
                Element (Model (V)'Loop_Entry, N) =
                As_Element (V, N));
         pragma Loop_Invariant
           (Valid_Cursors (V)'Loop_Entry = Valid_Cursors (V));
         if As_Element (V, Cu) < Element_Type'Last then
            Replace_Element (V, Cu, Element (V, Cu) + 1);
         end if;
         Next (V, Cu);
      end loop;
   end Incr_All_3;

   procedure Double_Size (V : in out Vector) is
      Lst : Index_Type'Base := Last (V);
   begin
      for I in Index_Type'First .. Lst loop
         pragma Loop_Invariant
           (Integer (Length (V)) =
            Integer (Length (V)'Loop_Entry) + (I - Index_Type'First));
         pragma Loop_Invariant
           (for all J in Index_Type'First .. Last (V)'Loop_Entry =>
              As_Element (V, J) = Element (Model (V)'Loop_Entry, J));
         pragma Loop_Invariant
           (for all J in Index_Type'First .. I - 1 =>
              As_Element (V, J + Integer (Length (V)'Loop_Entry)) =
                Element (Model (V)'Loop_Entry, J));
         Append (V, Element (V, I));
      end loop;
   end Double_Size;

   function My_Find (V : Vector; E : Element_Type) return Index_Type'Base is
      Lst : Index_Type'Base := Last (V);
   begin
      for Current in Index_Type'First .. Lst loop
         pragma Loop_Invariant
           (for all I in Index_Type'First .. Current - 1 =>
              As_Element (V, I) /= E);
         if As_Element (V, Current) = E then
            return Current;
         end if;
      end loop;
      return Index_Type'First - 1;
   end My_Find;

   procedure Update_Range_To_Zero (V : in out Vector; Fst, Lst : Index_Type) is
   begin
      for Current in Fst .. Lst loop
         pragma Loop_Invariant (Length (V) = Length (V)'Loop_Entry);
         pragma Loop_Invariant
           (Valid_Cursors (V) = Valid_Cursors (V)'Loop_Entry);
         pragma Loop_Invariant
           (for all I in Fst .. Current - 1 =>
                As_Element (V, I) = 0);
         pragma Loop_Invariant
           (for all I in Index_Type'First .. Fst - 1 =>
              As_Element (V, I) = Element (Model (V)'Loop_Entry, I));
         pragma Loop_Invariant
           (for all I in Current .. Last (V) =>
                As_Element (V, I) = Element (Model (V)'Loop_Entry, I));
         Replace_Element (V, Current, 0);
      end loop;
   end Update_Range_To_Zero;

   procedure Insert_Count (V : in out Vector; I : Index_Type) is
      Loc : Vector;
      J   : Index_Type;
   begin
      Assign (Loc, V);

      --  Remove all elements after I, and will reinsert them afterward.
      --  This procedure simulates a primitive Insert, which we do not have
      --  yet

      if I = Index_Type'First then
         Clear (V);
      else
         Resize (V, To_Count (I - 1), 0);
      end if;

      --  Now insert the new elements
      --  Here we want to test scalability of GNATprove. We do not use a
      --  loop which would introduce a cutpoint for proof.

      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);

      --  And finally reinsert the other elements

      J := I;
      while J <= Last (Loc) loop
         Append (V, As_Element (Loc, J));
         pragma Loop_Invariant (J in I .. Last (Loc));
         pragma Loop_Invariant
           (Integer (Length (V)) = J - Index_Type'First + Count + 1);
         pragma Loop_Invariant
           (for all K in Index_Type'First .. I - 1 =>
              As_Element (V, K) = As_Element (Loc, K));
         pragma Loop_Invariant
           (for all K in I + Count .. Last (V) =>
                As_Element (V, K) = As_Element (Loc, K - Count));
         pragma Loop_Invariant
           (for all K in I .. I + Count - 1 => As_Element (V, K) = 0);
         J := J + 1;
      end loop;
   end Insert_Count;

   function P (E : Element_Type) return Boolean is
   begin
      return E >= 0;
   end P;

   procedure From_Higher_To_Lower (V : Vector) is null;

   procedure From_Lower_To_Higher (V : Vector) is
   begin
      Impl.Lift_Abstraction_Level (V);
   end From_Lower_To_Higher;

end Use_Vectors;
