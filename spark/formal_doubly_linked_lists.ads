pragma Ada_2012;
with Conts.Lists.Indefinite_Unbounded_SPARK;
with Functional_Sequences;
with Functional_Maps;

generic
   type Element_Type (<>) is private;
package Formal_Doubly_Linked_Lists with SPARK_Mode is

   package Element_Lists is new Conts.Lists.Indefinite_Unbounded_SPARK
     (Element_Type => Element_Type);
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.

   subtype Cursor is Element_Lists.Cursor;
   use all type Element_Lists.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Lists.Lists.No_Element;

   type List is tagged limited private with
     Default_Initial_Condition => Length (List) = 0;
   --  Lists are empty when default initialized

   function Capacity (L : List'Class) return Natural;

   function Length (L : List'Class) return Natural with
     Post => Length'Result <= Capacity (L);
   --  The length of a list is always smaller than its capacity

   package Formal_Model is

      --  This package should be Ghost if possible. Currently, the compiler
      --  complains that the parent type of a Ghost type extension shall be
      --  Ghost (see OA30-006).

      package Cursor_Map is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element);
      package Element_Sequence is new Functional_Sequences
        (Element_Type => Element_Type);
      use Element_Sequence;
      use Cursor_Map;

      function Model (L : List'Class) return Sequence with
        Post => Length (Model'Result) = Length (L);
      function Element (S : Sequence; I : Positive) return Element_Type
                        renames Element_Sequence.Get;
      --  The highlevel model of a list is a sequence of elements. Cursors are
      --  not represented in this model.

      function Positions (L : List'Class) return Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Import,
        --  ??? We removed the implementation as GNATprove was wrongly assuming
        --  Positions could be recursive. To be reassed when O722-006 is done
        --  and flow analysis can be called.

        Post =>

          --  Positions of cursors are smaller than the container's length.

          (for all C1 in Positions'Result =>
             Get (Positions'Result, C1) in 1 .. Length (L)

           --  No two cursors have the same position. Note that we do not state
           --  that there is a cursor in the map for each position, as it is
           --  rarely needed.

           and then
             (for all C2 in Positions'Result =>
                (if Get (Positions'Result, C1) =
                     Get (Positions'Result, C2)
                     then C1 = C2)));
   end Formal_Model;

   use Formal_Model;
   use Element_Sequence;
   use Cursor_Map;

   function Element (L : List'Class; C : Cursor) return Element_Type with
     Pre  => Mem (Positions (L), C),
     Post =>

       --  Query Positions to get the position of C in L and use it to fetch
       --  the corresponding element in Model.

       Element'Result = Element (Model (L), Get (Positions (L), C));

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (L : List'Class) return Cursor with
     Post => (if Length (L) = 0 then First'Result = No_Element
              else Mem (Positions (L), First'Result) and then
                  Get (Positions (L), First'Result) = 1);

   procedure Next (L : List'Class; C : in out Cursor) with
     Pre  => Mem (Positions (L), C),
     Post => (if Get (Positions (L), C'Old) = Length (L)
              then C = No_Element
              else Mem (Positions (L), C)
                and then Get (Positions (L), C) =
                  Get (Positions (L), C'Old) + 1);

   function Last (L : List'Class) return Cursor with
     Import,
     Post => (if Length (L) = 0 then Last'Result = No_Element
              else Mem (Positions (L), Last'Result) and then
                  Get (Positions (L), Last'Result) = Length (L));

   function Has_Element (L : List'Class; C : Cursor) return Boolean with
     Post => Has_Element'Result = Mem (Positions (L), C);

   function Find (L : List'Class; E : Element_Type) return Cursor with
     Import,
     Post =>

       --  Either E is not in the model and the result is No_Element

       (Find'Result = No_Element
        and (for all I in 1 .. Length (L) => Element (Model (L), I) /= E))

     --  or the result is a valid cursor, E is stored at its position in L and
     --  there is no previous occurrence of E in L.

     or else
       (Mem (Positions (L), Find'Result)
        and Element (Model (L), Get (Positions (L), Find'Result)) = E
        and (for all I in 1 .. Get (Positions (L), Find'Result) - 1 =>
               Element (Model (L), I) /= E));

   procedure Append (L : in out List'Class; E : Element_Type) with
     Pre  => Length (L) < Capacity (L),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old + 1

     --  Positions contains a new mapping from the last cursor of L to Length.

     and Is_Add (Positions (L)'Old, Last (L), Length (L), Positions (L))

     --  Model contains a new element E at the end.

     and Is_Add (Model (L)'Old, E, Model (L));

   procedure Insert (L : in out List'Class; C : Cursor; E : Element_Type) with
   --  Insert E before the valid cursor C in L.

     Import,
     Pre  => Length (L) < Capacity (L) and then Has_Element (L, C),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old + 1

     --  Every cursor previously valid in L is still valid.

     and (for all D in Positions (L)'Old =>
              Mem (Positions (L), D) and

              --  If it was located before C in L its position is preserved.

              (if Get (Positions (L)'Old, D) <
                   Get (Positions (L)'Old, C)
               then Get (Positions (L), D) = Get (Positions (L)'Old, D)

               --  Otherwise it is shifted by 1.

               else Get (Positions (L), D) =
                 Get (Positions (L)'Old, D) + 1))

     --  Every cursor valid in L was previously valid except for the newly
     --  inserted cursor.

     and (for all D in Positions (L) =>
              Mem (Positions (L)'Old, D) or
              Get (Positions (L), D) = Get (Positions (L)'Old, C))

     --  The elements of L before C are preserved.

     and (for all I in 1 .. Get (Positions (L)'Old, C) - 1 =>
              Element (Model (L), I) =
              Element (Model (L)'Old, I))

     --  Other elements are shifted by 1.

     and (for all I in Get (Positions (L)'Old, C) + 1 .. Length (L) =>
              Element (Model (L), I) =
            Element (Model (L)'Old, I - 1))

     --  E is stored at the previous position of C in L.

     and Element (Model (L), Get (Positions (L)'Old, C)) = E;

   procedure Replace_Element
     (L : in out List'Class; C : Cursor; E : Element_Type) with
     Import,
     Pre  => Has_Element (L, C),
     Post => Capacity (L) = Capacity (L)'Old
     and Length (L) = Length (L)'Old

     --  Cursors are preserved.

     and Positions (L)'Old = Positions (L)

     --  The element at the position of C in L is replaced by E.

     and Is_Replace (Model (L)'Old, Get (Positions (L), C), E, Model (L));

   procedure Clear (L : in out List'Class)
   with
       Post => Capacity (L) = Capacity (L)'Old
     and then Length (L) = 0;

private
   pragma SPARK_Mode (Off);

   type List is new Element_Lists.List with null record;

end Formal_Doubly_Linked_Lists;
