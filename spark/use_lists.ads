pragma Ada_2012;
with Formal_Doubly_Linked_Lists;
pragma Elaborate_All (Formal_Doubly_Linked_Lists);

package Use_Lists with SPARK_Mode is
   package My_Lists is new Formal_Doubly_Linked_Lists
     (Element_Type => Integer,
      "="          => "=");
   use My_Lists;
   use type My_Lists.Cursor;
   use My_Lists.Formal_Model;
   use My_Lists.Formal_Model.Cursor_Map;
   use My_Lists.Formal_Model.Element_Sequence;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Is_Incr (I1, I2 : Integer) return Boolean is
      (if I1 = Integer'Last then I2 = Integer'Last else I2 = I1 + 1);

   procedure Incr_All (L1 : List; L2 : in out List) with
     Pre  => Capacity (L2) >= Capacity (L1),
     Post => Capacity (L2) = Capacity (L2)'Old
     and Length (L2) = Length (L1)
     and (for all N in 1 .. Length (L1) =>
              Is_Incr (Element (Model (L1), N),
                       Element (Model (L2), N)));

   procedure Incr_All_2 (L : in out List) with
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old
     and (for all N in 1 .. Length (L) =>
              Is_Incr (Element (Model (L)'Old, N),
                       Element (Model (L), N)));

   procedure Incr_All_3 (L : in out List) with
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old
     and (for all N in 1 .. Length (L) =>
              Is_Incr (Element (Model (L)'Old, N),
                       Element (Model (L), N)))
     and Positions (L)'Old = Positions (L);

   --  Length is set to a static value to work around a crash in flow analysis.
   --  See O722-006
   procedure Double_Size (L : in out List) with
     Pre  => Capacity (L) / 2 >= Length (L) and then Length (L) = 100,
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = 2 * Length (L)'Old
     and (for all I in 1 .. Length (L)'Old =>
       Element (Model (L), I) = Element (Model (L)'Old, I)
       and Element (Model (L), I + Length (L)'Old) =
           Element (Model (L)'Old, I));

   procedure Double_Size_2 (L : in out List) with
     Pre  => Capacity (L) / 2 >= Length (L),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = 2 * Length (L)'Old
     and (for all I in 1 .. Length (L)'Old =>
       Element (Model (L), 2 * I - 1) = Element (Model (L)'Old, I)
       and Element (Model (L), 2 * I) =
           Element (Model (L)'Old, I));

   function My_Find (L : List; E : Integer) return Cursor with
     Post => My_Find'Result = Find (L, E);

   procedure Update_Range_To_Zero (L : in out List; Fst, Lst : Cursor) with
     Pre  => Has_Element (L, Fst) and then Has_Element (L, Lst)
     and then Get (Positions (L), Lst) >= Get (Positions (L), Fst),
     Post => Positions (L) = Positions (L)'Old
     and (for all I in 1 .. Length (L) =>
              (if I in Get (Positions (L), Fst) .. Get (Positions (L), Lst)
               then Element (Model (L), I) = 0
               else Element (Model (L), I) = Element (Model (L)'Old, I)));

   procedure Insert_5 (L : in out List; Cu : Cursor) with
     Pre  => Has_Element (L, Cu) and Capacity (L) - 5 >= Length (L),
     Post => Length (L) = Length (L)'Old + 5
     and (for all I in 1 .. Get (Positions (L)'Old, Cu) - 1 =>
        Element (Model (L), I) = Element (Model (L)'Old, I))
     and (for all I in Get (Positions (L)'Old, Cu) ..
            Get (Positions (L)'Old, Cu) + 4 =>
        Element (Model (L), I) = 0)
     and (for all I in Get (Positions (L)'Old, Cu) + 5 .. Length (L) =>
              Element (Model (L), I) = Element (Model (L)'Old, I - 5))
     and Mem (Positions (L), Cu)
     and Get (Positions (L), Cu) = Get (Positions (L)'Old, Cu) + 5;
end Use_Lists;
