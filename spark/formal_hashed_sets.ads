pragma Ada_2012;
with Functional_Sequences;
with Functional_Maps;
with Functional_Sets;

generic
   type Element_Type (<>) is private;
   None : Element_Type;
   --  Special element that cannot be contained in sets

package Formal_Hashed_Sets with SPARK_Mode is

   package Element_Sets is
      type Set is tagged limited private;
      type Cursor is private;
      No_Element : constant Cursor;
   private
      pragma SPARK_Mode (Off);
      type Cursor is record
         I : Natural;
      end record;
      type Set is tagged limited null record;
      No_Element : constant Cursor := (I => 0);
   end Element_Sets;
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.
   --  To be replaced with an instance of the proper Set package.

   subtype Cursor is Element_Sets.Cursor;
   use all type Element_Sets.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Sets.No_Element;

   type Set is tagged limited private with
     Default_Initial_Condition => Length (Set) = 0;
   --  Sets are empty when default initialized

   function Capacity (S : Set'Class) return Natural with
     Import;

   function Length (S : Set'Class) return Natural with
     Import,
     Post => Length'Result <= Capacity (S);
   --  The length of a set is always smaller than its capacity

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
      package Element_Set is new Functional_Sets
        (Element_Type => Element_Type,
         No_Element   => None);
      use Element_Sequence;
      use Cursor_Map;
      use Element_Set;

      function Model (S : Set'Class) return Element_Set.Set with
        Import;
      --  The highlevel model of a set is a set of elements. Neither cursors
      --  nor order of elements are represented in this model.

      function Elements (S : Set'Class) return Sequence with
      --  The Elements sequence represents the underlying list structure of
      --  sets that is used for iteration. It does not model cursors.

        Import,
        Post => Length (Elements'Result) = Length (S)

        --  It only contains elements contained in Model.

        and then (for all I in 1 .. Length (S) =>
                      Mem (Model (S), Get (Elements'Result, I)))

        --  It contains all the elements contained in Model.

        and then (for all E in Model (S) =>
                      (for some I in 1 .. Length (S) =>
                             Get (Elements'Result, I) = E))

        --  It has no duplicate.

        and then
            (for all I in 1 .. Length (S) =>
               (for all J in 1 .. Length (S) =>
                    (if Get (Elements'Result, I) = Get (Elements'Result, J)
                     then I = J)));

      function Positions (S : Set'Class) return Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Import,
        Post =>

          --  Positions of cursors are smaller than the container's length.

          (for all C1 in Positions'Result =>
             Get (Positions'Result, C1) in 1 .. Length (S)

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
   use Element_Set;

   function Element (S : Set'Class; C : Cursor) return Element_Type with
     Import,
     Pre  => Mem (Positions (S), C),

     --  Query Positions to get the position of C in L and use it to fetch
     --  the corresponding element in Elements.

     Post => Element'Result = Get (Elements (S), Get (Positions (S), C));

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (S : Set'Class) return Cursor with
     Import,
     Post => (if Length (S) = 0 then First'Result = No_Element
              else Mem (Positions (S), First'Result) and then
                  Get (Positions (S), First'Result) = 1);

   procedure Next (S : Set'Class; C : in out Cursor) with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => (if Get (Positions (S), C'Old) = Length (S)
              then C = No_Element
              else Mem (Positions (S), C)
                and then Get (Positions (S), C) =
                  Get (Positions (S), C'Old) + 1);

   function Has_Element (S : Set'Class; C : Cursor) return Boolean with
     Import,
     Post => Has_Element'Result = Mem (Positions (S), C);

   function Contains (S : Set'Class; E : Element_Type) return Boolean with
     Import,
     Post => Contains'Result = Mem (Model (S), E);

   function Find (S : Set'Class; E : Element_Type) return Cursor with
     Import,
     Post =>

       --  Either E is not in the model and the result is No_Element

       (Find'Result = No_Element
        and not Mem (Model (S), E))

     --  or the result is a valid cursor and E is stored at its position in S.

     or else
       (Mem (Model (S), E)
        and Mem (Positions (S), Find'Result)
        and Get (Elements (S), Get (Positions (S), Find'Result)) = E);

   procedure Include (S : in out Set'Class; E : Element_Type) with
   --  Insert an element E in S if E is not already in present.

     Import,
     Pre  => (Length (S) < Capacity (S) and then E /= None)
     or else Mem (Model (S), E),
     Post => Capacity (S) = Capacity (S)'Old

     --  If E is already in S, then the model is unchanged.

     and (if Mem (Model (S)'Old, E) then
            Length (S) = Length (S)'Old
            and Model (S) = Model (S)'Old
            and Elements (S) = Elements (S)'Old
            and Positions (S) = Positions (S)'Old

          --  If E is not in S, then E is a new element of its model.

          else Length (S) = Length (S)'Old + 1
            and Is_Add (Model (S)'Old, E, Model (S))

            --  Cursors that were valid in S are still valid and continue
            --  designating the same element.

            and (for all C in Positions (S)'Old =>
                   Mem (Positions (S), C) and
                     Get (Elements (S), Get (Positions (S), C)) =
                     Get (Elements (S)'Old, Get (Positions (S)'Old, C)))

            --  Cursors that are valid in S were already valid in S except for
            --  the newly inserted cursor.
            --  Nothing is said about the order of elements in S after the
            --  call.

            and (for all C in Positions (S) =>
                   Mem (Positions (S)'Old, C) or
                     Get (Elements (S), Get (Positions (S), C)) = E));

   procedure Exclude (S : in out Set'Class; E : Element_Type) with
   --  Remove an element E of S if it is present.

     Import,
     Post => Capacity (S) = Capacity (S)'Old

     --  If E is not in S, then the model is unchanged.

     and (if not Mem (Model (S)'Old, E) then
            Length (S) = Length (S)'Old
            and Model (S) = Model (S)'Old
            and Elements (S) = Elements (S)'Old
            and Positions (S) = Positions (S)'Old

          --  If E is in S, then E is removed from its model.

          else Length (S) = Length (S)'Old - 1
            and Is_Add (Model (S), E, Model (S)'Old)

            --  Cursors that are valid in S were already valid and continue
            --  designating the same element.

            and (for all C in Positions (S) =>
                   Mem (Positions (S)'Old, C) and
                   Get (Elements (S), Get (Positions (S), C)) =
                     Get (Elements (S)'Old, Get (Positions (S)'Old, C)))

            --  Cursors that were valid in S are still valid in S except for
            --  the removed cursor.
            --  Nothing is said about the order of elements in S after the
            --  call.

            and (for all C in Positions (S)'Old =>
                   Mem (Positions (S), C) or
                     Get (Elements (S)'Old, Get (Positions (S)'Old, C)) = E));

   procedure Union (S1 : in out Set'Class; S2 : Set'Class) with
   --  Include in S1 all the elements of S2

     Import,
     Pre  => Length (S2) <= Capacity (S1) - Length (S1),
     Post => Capacity (S1) = Capacity (S1)'Old

     --  The model of S1 is the union of the previous model of S1 and the model
     --  of S2.

     and Is_Union (Model (S1)'Old, Model (S2), Model (S1))

     --  No more than Length (S2) elements were added to S2. We could be more
     --  precise by using the length of the Intersection if we had a notion of
     --  length on functional sets.

     and Length (S1) in Length (S1)'Old .. Length (S1)'Old + Length (S2)

     --  Cursors that were valid in S1 are still valid and continue designating
     --  the same element.
     --  Nothing is said about the order of elements in S1 after the call.

     and (for all C in Positions (S1)'Old =>
              Mem (Positions (S1), C)
          and Get (Elements (S1), Get (Positions (S1), C)) =
            Get (Elements (S1)'Old, Get (Positions (S1)'Old, C)));

   procedure Intersection (S1 : in out Set'Class; S2 : Set'Class) with
   --  Exclude from S1 all the elements of S2

     Import,
     Post => Capacity (S1) = Capacity (S1)'Old

     --  The model of S1 is the intersection of the previous model of S1 and
     --  the model of S2.

     and Is_Intersection (Model (S1)'Old, Model (S2), Model (S1))

     --  The length of S1 can only have shrinked. We could be more precise by
     --  stating that at most Length (S2) elements have been removed from S1.

     and Length (S1) in 0 .. Length (S1)'Old

     --  Cursors that are valid in S1 we already valid and continue designating
     --  the same element.
     --  Nothing is said about the order of elements in S1 after the call.

     and (for all C in Positions (S1) =>
              Mem (Positions (S1)'Old, C)
          and Get (Elements (S1), Get (Positions (S1), C)) =
            Get (Elements (S1)'Old, Get (Positions (S1)'Old, C)));

   procedure Clear (S : in out Set'Class)
   with
       Import,
       Post => Capacity (S) = Capacity (S)'Old
     and then Length (S) = 0
     and then Is_Empty (Model (S));

private
   pragma SPARK_Mode (Off);

   type Set is new Element_Sets.Set with null record;

end Formal_Hashed_Sets;
