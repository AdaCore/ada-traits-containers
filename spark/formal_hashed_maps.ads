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

   function Capacity (Self : Map'Class) return Natural with
     Import;

   function Length (Self : Map'Class) return Natural with
     Import,
     Post => Length'Result <= Capacity (Self);
   --  The length of a map is always smaller than its capacity

   package Formal_Model is

      --  This package should be Ghost if possible. Currently, the compiler
      --  complains that the parent type of a Ghost type extension shall be
      --  Ghost (see OA30-006).

      package P is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor,
         No_Key       => No_Element);
      package K is new Functional_Sequences
        (Element_Type => Key_Type,
         Index_Type   => Positive);
      package M is new Functional_Maps
        (Element_Type => Element_Type,
         Key_Type     => Key_Type,
         No_Key       => None);

      function Model (Self : Map'Class) return M.Map with
        Import;
      --  The highlevel model of a map is a map from keys to elements. Neither
      --  cursors nor order of elements are represented in this model.

      function Keys (Self : Map'Class) return K.Sequence with
      --  The Keys sequence represents the underlying list structure of maps
      --  that is used for iteration. It does not model cursors nor elements.

        Import,
        Post => K.Length (Keys'Result) = Length (Self)

        --  It only contains keys contained in Model.

        and then (for all I in 1 .. Length (Self) =>
                      M.Mem (Model (Self), K.Get (Keys'Result, I)))

        --  It contains all the keys contained in Model.

        and then (for all Element in Model (Self) =>
                      (for some I in 1 .. Length (Self) =>
                             K.Get (Keys'Result, I) = Element))

        --  It has no duplicate.

        and then
            (for all I in 1 .. Length (Self) =>
               (for all J in 1 .. Length (Self) =>
                    (if K.Get (Keys'Result, I) = K.Get (Keys'Result, J)
                     then I = J)));

      function Positions (Self : Map'Class) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Import,
        Post =>

          --  Positions of cursors are smaller than the container's length.

          (for all I in Positions'Result =>
             P.Get (Positions'Result, I) in 1 .. Length (Self)

           --  No two cursors have the same position. Note that we do not state
           --  that there is a cursor in the map for each position, as it is
           --  rarely needed.

           and then
             (for all J in Positions'Result =>
                (if P.Get (Positions'Result, I) =
                     P.Get (Positions'Result, J)
                 then I = J)));

      procedure Lift_Abstraction_Level (Self : Map'Class) with
        Import,
        Global => null,
        Post   =>
          (for all I in 1 .. Length (Self) =>
             (for some Cu in Positions (Self) =>
                  K.Get (Keys (Self),  P.Get (Positions (Self), Cu)) =
                K.Get (Keys (Self), I)));
      --  Lift_Abstraction_Level is a ghost procedure that does nothing but
      --  assume that we can access to the same elements by iterating over
      --  positions or cursors.
      --  This information is not generally useful except when switching from
      --  a lowlevel, cursor aware view of a container, to a highlevel position
      --  based view.
   end Formal_Model;

   package M renames Formal_Model.M;
   package K renames Formal_Model.K;
   package P renames Formal_Model.P;

   use type M.Map;
   use type K.Sequence;
   use type P.Map;

   function Model (Self : Map'Class) return M.Map
                   renames Formal_Model.Model;
   function Element (Self : M.Map; Key : Key_Type) return Element_Type
                   renames M.Get;
   function Keys (Self : Map'Class) return K.Sequence
                   renames Formal_Model.Keys;
   function Positions (Self : Map'Class) return P.Map
                   renames Formal_Model.Positions;

   function Key (Self : Map'Class; Position : Cursor) return Key_Type with
     Import,
     Pre  => P.Mem (Positions (Self), Position),

     --  Query Positions to get the position of Position in L and use it to
     --  fetch the corresponding key in Keys.

     Post => Key'Result =
       K.Get (Keys (Self), P.Get (Positions (Self), Position));

   function Element (Self : Map'Class; Key : Key_Type) return Element_Type with
     Import,
     Pre  => M.Mem (Model (Self), Key),
     Post => Element'Result = Element (Model (Self), Key);

   function Element (Self : Map'Class; Position : Cursor) return Element_Type
   with
     Import,
     Pre  => P.Mem (Positions (Self), Position),

     --  Query Positions to get the position of Position in L, use it to fetch
     --  the corresponding key in Keys, and then use this key to get the
     --  associated element from Model.

     Post => Element'Result =
       Element (Model (Self),
                K.Get (Keys (Self), P.Get (Positions (Self), Position)));

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Map'Class) return Cursor with
     Import,
     Post => (if Length (Self) = 0 then First'Result = No_Element
              else P.Mem (Positions (Self), First'Result) and then
                  P.Get (Positions (Self), First'Result) = 1);

   procedure Next (Self : Map'Class; Position : in out Cursor) with
     Import,
     Pre  => P.Mem (Positions (Self), Position),
     Post => (if P.Get (Positions (Self), Position'Old) = Length (Self)
              then Position = No_Element
              else P.Mem (Positions (Self), Position)
                and then P.Get (Positions (Self), Position) =
                  P.Get (Positions (Self), Position'Old) + 1);

   function Has_Element (Self : Map'Class; Position : Cursor) return Boolean
   with
     Import,
     Post => Has_Element'Result = P.Mem (Positions (Self), Position);

   function Contains (Self : Map'Class; Key : Key_Type) return Boolean with
     Import,
     Post => Contains'Result = M.Mem (Model (Self), Key);

   function Find (Self : Map'Class; Key : Key_Type) return Cursor with
     Import,
     Post =>

       --  Either K is not in the model and the result is No_Element

       (Find'Result = No_Element
        and not M.Mem (Model (Self), Key))

     --  or the result is a valid cursor and K is stored at its position in V.

     or else
       (M.Mem (Model (Self), Key)
        and P.Mem (Positions (Self), Find'Result)
        and K.Get (Keys (Self), P.Get (Positions (Self), Find'Result)) = Key);

   procedure Include
     (Self : in out Map'Class; Key : Key_Type; Element : Element_Type)
   --  Insert a key K and an element Element in Self if K is not already in
   --  present.
   --  Otherwise, replace the element associated to K by E.

   with
     Import,
     Pre  => (Length (Self) < Capacity (Self) and then Key /= None)
       or else M.Mem (Model (Self), Key),
     Post => Capacity (Self) = Capacity (Self)'Old

     --  If Element is already in M, then K now maps to Element in Model.

     and (if M.Mem (Model (Self)'Old, Key) then
            Length (Self) = Length (Self)'Old
            and M.Is_Replace (Model (Self)'Old, Key, Element, Model (Self))

            --  Keys and cursors are preserved

            and Keys (Self) = Keys (Self)'Old
            and Positions (Self) = Positions (Self)'Old

          --  If Element is not in Self, then Element is a new element of its
          --  model.

          else Length (Self) = Length (Self)'Old + 1
            and M.Is_Add (Model (Self)'Old, Key, Element, Model (Self))

            --  Cursors that were valid in Self are still valid and continue
            --  designating the same element.

            and (for all Position in Positions (Self)'Old =>
                 P.Mem (Positions (Self), Position) and
                 K.Get (Keys (Self), P.Get (Positions (Self), Position)) =
                   K.Get (Keys (Self)'Old,
                          P.Get (Positions (Self)'Old, Position)))

            --  Cursors that are valid in Self were already valid in Self
            --  except for the newly inserted cursor.
            --  Nothing is said about the order of keys in Self after the call.

            and (for all Position in Positions (Self) =>
                   P.Mem (Positions (Self)'Old, Position) or
                     K.Get (Keys (Self),
                            P.Get (Positions (Self), Position)) = Key));

   procedure Clear (Self : in out Map'Class)
   with
       Import,
       Post => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0
     and then M.Is_Empty (Model (Self));

private
   pragma SPARK_Mode (Off);

   type Map is new Element_Maps.Map with null record;

end Formal_Hashed_Maps;
