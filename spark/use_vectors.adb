pragma Ada_2012;
package body Use_Vectors with SPARK_Mode is

   procedure Incr_All (V1 : Vector; V2 : in out Vector) is
      Lst : Index_Type'Base := Last (V1);
   begin
      Clear (V2);
      for I in Index_Type'First .. Lst loop
         pragma Loop_Invariant (Length (V2) = I - Index_Type'First);
         pragma Loop_Invariant
           (for all N in Index_Type'First .. Last (V2) =>
                Is_Incr (Element (V1, N),
              Element (V2, N)));

         if Element (V1, I) < Integer'Last then
            Append (V2, Element (V1, I) + 1);
         else
            Append (V2, Element (V1, I));
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
                         Element (V, N)));
         pragma Loop_Invariant
           (for all N in I .. Last (V) =>
                Element (Model (V)'Loop_Entry, N) =
                Element (V, N));
         if Element (V, I) < Integer'Last then
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
           (for all N in Index_Type'First .. To_Index (Cu) - 1 =>
                Is_Incr (Element (Model (V)'Loop_Entry, N),
                         Element (V, N)));
         pragma Loop_Invariant
           (for all N in To_Index (Cu) .. Last (V) =>
                Element (Model (V)'Loop_Entry, N) =
                Element (V, N));
         pragma Loop_Invariant
           (Valid_Cursors (V)'Loop_Entry = Valid_Cursors (V));
         if Element (V, Cu) < Integer'Last then
            Replace_Element (V, To_Index (Cu), Element (V, Cu) + 1);
         end if;
         Next (V, Cu);
      end loop;
   end Incr_All_3;

   procedure Double_Size (V : in out Vector) is
      Lst : Index_Type'Base := Last (V);
   begin
      for I in Index_Type'First .. Lst loop
         pragma Loop_Invariant
           (Length (V) = Length (V)'Loop_Entry + (I - Index_Type'First));
         pragma Loop_Invariant
           (for all J in Index_Type'First .. Last (V)'Loop_Entry =>
              Element (V, J) = Element (Model (V)'Loop_Entry, J));
         pragma Loop_Invariant
           (for all J in Index_Type'First .. I - 1 =>
              Element (V, J + Length (V)'Loop_Entry) =
                Element (Model (V)'Loop_Entry, J));
         Append (V, Element (V, I));
      end loop;
   end Double_Size;

   function My_Find (V : Vector; E : Integer) return Index_Type'Base is
      Lst : Index_Type'Base := Last (V);
   begin
      for Current in Index_Type'First .. Lst loop
         pragma Loop_Invariant
           (for all I in Index_Type'First .. Current - 1 =>
              Element (Model (V), I) /= E);
         if Element (V, Current) = E then
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
                Element (Model (V), I) = 0);
         pragma Loop_Invariant
           (for all I in Index_Type'First .. Fst - 1 =>
              Element (Model (V), I) = Element (Model (V)'Loop_Entry, I));
         pragma Loop_Invariant
           (for all I in Current .. Last (V) =>
                Element (Model (V), I) = Element (Model (V)'Loop_Entry, I));
         Replace_Element (V, Current, 0);
      end loop;
   end Update_Range_To_Zero;

   procedure Insert_5 (V : in out Vector; I : Index_Type) is
      Loc : Vector;
      J   : Index_Type;
   begin
      Assign (Loc, V);
      if I = Index_Type'First then
         Clear (V);
      else
         Resize (V, I - 1, 0);
      end if;
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);
      Append (V, 0);

      J := I;
      while J <= Last (Loc) loop
         Append (V, Element (Loc, J));
         pragma Loop_Invariant (J in I .. Last (Loc));
         pragma Loop_Invariant (Length (V) = J + 6 - Index_Type'First);
         pragma Loop_Invariant
           (for all K in Index_Type'First .. I - 1 =>
              Element (Model (V), K) = Element (Model (Loc), K));
         pragma Loop_Invariant
           (for all K in I + 5 .. Last (V) =>
                Element (Model (V), K) = Element (Model (Loc), K - 5));
         pragma Loop_Invariant
           (for all K in I .. I + 4 => Element (Model (V), K) = 0);
         J := J + 1;
      end loop;
   end Insert_5;

   function P (E : Integer) return Boolean is
   begin
      return E >= 0;
   end P;

   procedure From_Higher_To_Lower (V : Vector) is null;

   procedure From_Lower_To_Higher (V : Vector) is
   begin
      Formal_Model.Lift_Abstraction_Level (V);
   end From_Lower_To_Higher;

end Use_Vectors;
