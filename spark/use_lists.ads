pragma Ada_2012;
with Formal_Doubly_Linked_Lists;
pragma Elaborate_All (Formal_Doubly_Linked_Lists);

package Use_Lists with SPARK_Mode is
   package My_Lists is new
     Formal_Doubly_Linked_Lists (Element_Type => Integer);
   use My_Lists;
   use type My_Lists.Cursor;
   use My_Lists.Formal_Model;
   use My_Lists.Formal_Model.Cursor_Map;
   use My_Lists.Formal_Model.Element_Sequence;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Is_Incr (I1, I2 : Integer) return Boolean is
      (if I1 = Integer'Last then I2 = Integer'Last else I2 = I1 + 1);

   --  Incr_All loops through a list to increment each element.
   --  The first version puts the incremented elements in L2 whereas the two
   --  others modify L. The difference is that in the third one we also want
   --  to know that the cursors are preserved.

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

   --  Double_Size double the size of list by duplicating every element. In the
   --  first version, new elements are appended to the list while in the second
   --  they are inserted just before each duplicated element.

   procedure Double_Size (L : in out List) with
     Pre  => Capacity (L) / 2 >= Length (L),
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

   --  My_Find iterates to find an element.

   function My_Find (L : List; E : Integer) return Cursor with
     Post => My_Find'Result = Find (L, E);

   --  Update_Range_To_Zero replaces every element between Fst and Lst with 0.

   procedure Update_Range_To_Zero (L : in out List; Fst, Lst : Cursor) with
     Pre  => Has_Element (L, Fst) and then Has_Element (L, Lst)
     and then Get (Positions (L), Lst) >= Get (Positions (L), Fst),
     Post => Positions (L) = Positions (L)'Old
     and (for all I in 1 .. Length (L) =>
              (if I in Get (Positions (L), Fst) .. Get (Positions (L), Lst)
               then Element (Model (L), I) = 0
               else Element (Model (L), I) = Element (Model (L)'Old, I)));

   --  Insert_5 inserts 0 5 times just before Cu.

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

   --  Test links between high level, position based model of a container and
   --  lower level, cursor based model.

   function P (E : Integer) return Boolean;

   procedure From_Higher_To_Lower (L : List) with
     Ghost,
     Global => null,
     Pre    => (for all I in 1 .. Length (L) =>
                    P (Element (Model (L), I))),
     Post   => (for all Cu in Positions (L) =>
                    P (Element (Model (L), Get (Positions (L), Cu))));

   procedure From_Lower_To_Higher (L : List) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in Positions (L) =>
                    P (Element (Model (L), Get (Positions (L), Cu)))),
     Post   => (for all I in 1 .. Length (L) =>
                    P (Element (Model (L), I)));
end Use_Lists;
