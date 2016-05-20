pragma Ada_2012;
with  Conts.Lists.Indefinite_Unbounded_SPARK;
pragma Elaborate_All (Conts.Lists.Indefinite_Unbounded_SPARK);
with Conts; use Conts;

package Use_Lists with SPARK_Mode is
   type Element_Type is new Integer;
   package My_Lists is new
     Conts.Lists.Indefinite_Unbounded_SPARK (Element_Type => Element_Type);
   use type My_Lists.Cursor;
   use My_Lists.Lists;
   use type My_Lists.Element_Sequence;
   use all type My_Lists.Cursor_Position_Map;

   pragma Unevaluated_Use_Of_Old (Allow);

   function Is_Incr (I1, I2 : Element_Type) return Boolean is
      (if I1 = Element_Type'Last then I2 = Element_Type'Last else I2 = I1 + 1);

   procedure Incr_All (L1 : List; L2 : in out List) with
     Post => Length (L2) = Length (L1)
     and (for all N in 1 .. Length (L1) =>
              Is_Incr (Element (Model (L1), N),
                       Element (Model (L2), N)));
   --  Loop through a list to increment each element. Store the incremented
   --  elements in L2.

   procedure Incr_All_2 (L : in out List) with
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old
     and (for all N in 1 .. Length (L) =>
              Is_Incr (Element (Model (L)'Old, N),
                       Element (Model (L), N)));
   --  Same as before except that elements are stored back in L.

   procedure Incr_All_3 (L : in out List) with
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old
     and (for all N in 1 .. Length (L) =>
              Is_Incr (Element (Model (L)'Old, N),
                       Element (Model (L), N)))
     and Positions (L)'Old = Positions (L);
   --  Same as before except that we also specify that the cursors are
   --  preserved.

   procedure Double_Size (L : in out List) with
     Pre  => Count_Type'Last / 2 >= Length (L),
     Post => Length (L) = 2 * Length (L)'Old
     and (for all I in 1 .. Length (L)'Old =>
       Element (Model (L), I) = Element (Model (L)'Old, I)
       and Element (Model (L), I + Length (L)'Old) =
           Element (Model (L)'Old, I));
   --  Double the size of list by duplicating every element. New elements are
   --  appended to the list.

   procedure Double_Size_2 (L : in out List) with
     Pre  => Count_Type'Last / 2 >= Length (L),
     Post => Length (L) = 2 * Length (L)'Old
     and (for all I in 1 .. Length (L)'Old =>
              Element (Model (L), 2 * I - 1) =
            Element (Model (L)'Old, I)
       and Element (Model (L), 2 * I) =
            Element (Model (L)'Old, I));
   --  Same as before except that new elements are inserted just before each
   --  duplicated element.

   function My_Find (L : List; E : Element_Type) return Cursor with
     Post => My_Find'Result = Impl.Find (L, E);
   --  Iterate to find an element.

   procedure Update_Range_To_Zero (L : in out List; Fst, Lst : Cursor)
   --  Replace every element between Fst and Lst with 0.

   with
     Pre  => Has_Element (L, Fst) and then Has_Element (L, Lst)
     and then P_Get (Positions (L), Lst) >=
       P_Get (Positions (L), Fst),
     Post => Positions (L) = Positions (L)'Old
     and (for all I in 1 .. Length (L) =>
              (if I in P_Get (Positions (L), Fst) ..
                   P_Get (Positions (L), Lst)
               then Element (Model (L), I) = 0
                 else Element (Model (L), I) =
                   Element (Model (L)'Old, I)));

   Count : constant := 7;

   procedure Insert_Count (L : in out List; Cu : Cursor)
   --  Insert 0 Count times just before Cu.

   with
     Pre  => Has_Element (L, Cu) and Count_Type'Last - Count >= Length (L),
     Post => Length (L) = Length (L)'Old + Count
     and (for all I in 1 .. P_Get (Positions (L)'Old, Cu) - 1 =>
            Element (Model (L), I) =
              Element (Model (L)'Old, I))
     and (for all I in P_Get (Positions (L)'Old, Cu) ..
            P_Get (Positions (L)'Old, Cu) + Count - 1 =>
        Element (Model (L), I) = 0)
     and (for all I in P_Get (Positions (L)'Old, Cu) + Count ..
            Length (L) =>
              Element (Model (L), I) =
            Element (Model (L)'Old, I - Count))
     and P_Mem (Positions (L), Cu)
     and P_Get (Positions (L), Cu) =
       P_Get (Positions (L)'Old, Cu) + Count;

   --  Test links between high level, position based model of a container and
   --  lower level, cursor based model.

   function P (E : Element_Type) return Boolean;
   --  Any property P on an Integer E.

   procedure From_Higher_To_Lower (L : List) with
     Ghost,
     Global => null,
     Pre    => (for all E of L => P (E)),
     Post   => (for all Cu in L => P (Element (L, Cu)));
   --  Test that the link can be done from a property on the elements of a
   --  high level view of a container and its low level view.

   procedure From_Lower_To_Higher (L : List) with
     Ghost,
     Global => null,
     Pre    => (for all Cu in L => P (Element (L, Cu))),
     Post   => (for all E of L => P (E));
   --  Test that the link can be done from a property on the elements of a
   --  low level view of a container and its high level view.
end Use_Lists;
