pragma Ada_2012;
with Functional_Sequences;
with Functional_Maps;

generic
   type Element_Type (<>) is private;
   type Key_Type (<>) is private;
   None : Key_Type;
   --  Special key that cannot be contained in sets

package Formal_Hashed_Maps with SPARK_Mode is

   package Element_Maps is
      type Map is tagged limited private;
      type Cursor is private;
      No_Element : constant Cursor;
   private
      pragma SPARK_Mode (Off);
      type Cursor is record
         I : Natural;
      end record;
      type Map is tagged limited null record;
      No_Element : constant Cursor := (I => 0);
   end Element_Maps;
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.
   --  To be replaced with an instance of the proper Map package.

   subtype Cursor is Element_Maps.Cursor;
   use all type Element_Maps.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Maps.No_Element;

   type Map is tagged limited private with
     Default_Initial_Condition => Length (Map) = 0;
   --  Maps are empty when default initialized

   function Capacity (S : Map'Class) return Natural with
     Import;

   function Length (S : Map'Class) return Natural with
     Import,
     Post => Length'Result <= Capacity (S);
   --  The length of a map is always smaller than its capacity

   package Formal_Model is

      --  This package should be Ghost if possible. Currently, the compiler
      --  complains that the parent type of a Ghost type extension shall be
      --  Ghost (see OA30-006).

      package Cursor_Map is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element);
      package Key_Sequence is new Functional_Sequences
        (Element_Type => Key_Type);
      package Element_Map is new Functional_Maps
        (Element_Type => Element_Type,
         Key_Type     => Key_Type,
         No_Key       => None);
      use Key_Sequence;
      use Cursor_Map;
      use Element_Map;

      function Model (S : Map'Class) return Element_Map.Map with
        Import;
      --  The highlevel model of a map is a map from keys to elements. Neither
      --  cursors nor order of elements are represented in this model.

      function Keys (S : Map'Class) return Sequence with
      --  The Keys sequence represents the underlying list structure of maps
      --  that is used for iteration. It does not model cursors nor elements.

        Import,
        Post => Length (Keys'Result) = Length (S)

        --  It only contains keys contained in Model.

        and then (for all I in 1 .. Length (S) =>
                      Mem (Model (S), Get (Keys'Result, I)))

        --  It contains all the keys contained in Model.

        and then (for all E in Model (S) =>
                      (for some I in 1 .. Length (S) =>
                             Get (Keys'Result, I) = E))

        --  It has no duplicate.

        and then
            (for all I in 1 .. Length (S) =>
               (for all J in 1 .. Length (S) =>
                    (if Get (Keys'Result, I) = Get (Keys'Result, J)
                     then I = J)));

      function Positions (S : Map'Class) return Cursor_Map.Map with
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
   use Key_Sequence;
   use Cursor_Map;
   use Element_Map;

   function Key (S : Map'Class; C : Cursor) return Key_Type with
     Import,
     Pre  => Mem (Positions (S), C),

     --  Query Positions to get the position of C in L and use it to fetch
     --  the corresponding key in Keys.

     Post => Key'Result = Get (Keys (S), Get (Positions (S), C));

   function Element (S : Map'Class; K : Key_Type) return Element_Type with
     Import,
     Pre  => Mem (Model (S), K),
     Post => Element'Result = Get (Model (S), K);

   function Element (S : Map'Class; C : Cursor) return Element_Type with
     Import,
     Pre  => Mem (Positions (S), C),

     --  Query Positions to get the position of C in L, use it to fetch the
     --  corresponding key in Keys, and then use this key to get the associated
     --  element from Model.

     Post => Element'Result =
       Get (Model (S), Get (Keys (S), Get (Positions (S), C)));

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (S : Map'Class) return Cursor with
     Import,
     Post => (if Length (S) = 0 then First'Result = No_Element
              else Mem (Positions (S), First'Result) and then
                  Get (Positions (S), First'Result) = 1);

   procedure Next (S : Map'Class; C : in out Cursor) with
     Import,
     Pre  => Mem (Positions (S), C),
     Post => (if Get (Positions (S), C'Old) = Length (S)
              then C = No_Element
              else Mem (Positions (S), C)
                and then Get (Positions (S), C) =
                  Get (Positions (S), C'Old) + 1);

   function Has_Element (S : Map'Class; C : Cursor) return Boolean with
     Import,
     Post => Has_Element'Result = Mem (Positions (S), C);

   function Contains (S : Map'Class; K : Key_Type) return Boolean with
     Import,
     Post => Contains'Result = Mem (Model (S), K);

   function Find (S : Map'Class; K : Key_Type) return Cursor with
     Import,
     Post =>

       --  Either K is not in the model and the result is No_Element

       (Find'Result = No_Element
        and not Mem (Model (S), K))

     --  or the result is a valid cursor and K is stored at its position in S.

     or else
       (Mem (Model (S), K)
        and Mem (Positions (S), Find'Result)
        and Get (Keys (S), Get (Positions (S), Find'Result)) = K);

   procedure Include (S : in out Map'Class; K : Key_Type; E : Element_Type)
   --  Insert a key K and an element E in S if K is not already in present.
   --  Otherwise, replace the element associated to K by E.

   with
     Import,
     Pre  => (Length (S) < Capacity (S) and then K /= None)
       or else Mem (Model (S), K),
     Post => Capacity (S) = Capacity (S)'Old

     --  If E is already in M, then K now maps to E in Model.

     and (if Mem (Model (S)'Old, K) then
            Length (S) = Length (S)'Old
            and Is_Replace (Model (S)'Old, K, E, Model (S))

            --  Keys and cursors are preserved

            and Keys (S) = Keys (S)'Old
            and Positions (S) = Positions (S)'Old

          --  If E is not in S, then E is a new element of its model.

          else Length (S) = Length (S)'Old + 1
            and Is_Add (Model (S)'Old, K, E, Model (S))

            --  Cursors that were valid in S are still valid and continue
            --  designating the same element.

            and (for all C in Positions (S)'Old =>
                 Mem (Positions (S), C) and
                 Get (Keys (S), Get (Positions (S), C)) =
                 Get (Keys (S)'Old, Get (Positions (S)'Old, C)))

            --  Cursors that are valid in S were already valid in S except for
            --  the newly inserted cursor.
            --  Nothing is said about the order of keys in S after the call.

            and (for all C in Positions (S) =>
                   Mem (Positions (S)'Old, C) or
                     Get (Keys (S), Get (Positions (S), C)) = K));

   procedure Clear (S : in out Map'Class)
   with
       Import,
       Post => Capacity (S) = Capacity (S)'Old
     and then Length (S) = 0
     and then Is_Empty (Model (S));

private
   pragma SPARK_Mode (Off);

   type Map is new Element_Maps.Map with null record;

end Formal_Hashed_Maps;
