pragma Ada_2012;
with Functional_Sequences;
with Functional_Maps;
with Conts; use Conts;
with Conts.Maps.Indef_Indef_Unbounded_SPARK;

generic
   type Element_Type (<>) is private;
   type Key_Type (<>) is private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   None : Key_Type;
   --  Special key that cannot be contained in sets

package Formal_Hashed_Maps with SPARK_Mode is

   package Element_Maps is new Conts.Maps.Indef_Indef_Unbounded_SPARK
     (Key_Type            => Key_Type,
      Element_Type        => Element_Type,
      Container_Base_Type => Limited_Base,
      Hash                => Hash,
      "="                 => "=");
   --  Instance of the container package. That would be better if it was
   --  instantiated in the private part but then the Cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.

   subtype Cursor is Element_Maps.Cursor;
   use all type Element_Maps.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Maps.Impl.No_Element;

   type Map is tagged limited private with
     Default_Initial_Condition => Length (Map) = 0;
   --  Maps are empty when default initialized

   subtype Pair_Type is Element_Maps.Pair_Type;
   function Key   (P : Pair_Type) return Key_Type with
     Global => null;
   function Value (P : Pair_Type) return Element_Type with
     Global => null;
   --  This record contains both the key and the value of an element stored

   function Capacity (Self : Map'Class) return Natural with
     Global => null;

   function Length (Self : Map'Class) return Natural with
     Import,
     Global => null,
     Post   => Length'Result < Capacity (Self);
   --  The length of a map is always strictly smaller than its capacity

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
        Global => null;
      --  The highlevel model of a map is a map from keys to elements. Neither
      --  cursors nor order of elements are represented in this model.

      function Keys (Self : Map'Class) return K.Sequence with
      --  The Keys sequence represents the underlying list structure of maps
      --  that is used for iteration. It does not model cursors nor elements.

        Global => null,
        Post   => K.Length (Keys'Result) = Length (Self)

        --  It only contains keys contained in Model.

        and then (for all I in 1 .. Length (Self) =>
                      M.Mem (Model (Self), K.Get (Keys'Result, I)))

        --  It contains all the keys contained in Model.

        and then (for all Key in Model (Self) =>
                      (for some I in 1 .. Length (Self) =>
                             K.Get (Keys'Result, I) = Key))

        --  It has no duplicate.

        and then
            (for all I in 1 .. Length (Self) =>
               (for all J in 1 .. Length (Self) =>
                    (if K.Get (Keys'Result, I) = K.Get (Keys'Result, J)
                     then I = J)));

      function Positions (Self : Map'Class) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Global => null,
        Post   =>

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

   function Get (Self : Map'Class; Key : Key_Type) return Element_Type with
     Global => null,
     Pre    => M.Mem (Model (Self), Key),
     Post   => Get'Result = Element (Model (Self), Key);

   procedure Set
     (Self : in out Map'Class; Key : Key_Type; Element : Element_Type)
   --  Insert a key Key and an element Element in Self if Key is not already in
   --  present.
   --  Otherwise, replace the element associated to Key by Element.

   with
     Global => null,
     Pre    => Key /= None
       and then (M.Mem (Model (Self), Key)
                 or else Length (Self) < Count_Type'Last - 1),
     Post   =>

   --  If Key is already in Self, then Key now maps to Element in Model.

    (if M.Mem (Model (Self)'Old, Key)
     then Capacity (Self) = Capacity (Self)'Old
      and Length (Self) = Length (Self)'Old
      and M.Is_Replace (Model (Self)'Old, Key, Element, Model (Self))

      --  Keys and cursors are preserved

      and Keys (Self) = Keys (Self)'Old
      and Positions (Self) = Positions (Self)'Old

     --  If Key was not in Self, then Element is a new element of its
     --  model.

     else Capacity (Self) >= Capacity (Self)'Old
      and Length (Self) = Length (Self)'Old + 1
      and M.Is_Add (Model (Self)'Old, Key, Element, Model (Self)));
   --  ??? Are cursors preserved ?

   procedure Resize
     (Self     : in out Map'Class;
      New_Size : Count_Type)
   --  Change the capacity of the container.
   --  It does not change the high level model of Self.
   --  ??? Are cursors preserved ?

   with
     Global => null,
     Post   => Length (Self) = Length (Self)'Old
     and Model (Self) = Model (Self)'Old
     and Capacity (Self) >= New_Size;

   procedure Delete
     (Self : in out Map'Class;
      Key  : Key_Type)
   --  Remove the element from the map.
   --  No exception is raised if the element is not in the map.

   with
     Global => null,
     Post   => Capacity (Self) = Capacity (Self)'Old

     --  If Key was in Self then it is removed from its model.

     and (if M.Mem (Model (Self)'Old, Key) then
            Length (Self) = Length (Self)'Old - 1
            and M.Is_Add (Model (Self),
                          Key,
                          Element (Model (Self)'Old, Key),
                          Model (Self)'Old)

            --  Cursors that are valid in Self were already valid and
            --  designating the same element.

            and (for all Position in Positions (Self) =>
                 P.Mem (Positions (Self)'Old, Position) and
                 K.Get (Keys (Self), P.Get (Positions (Self), Position)) =
                   K.Get (Keys (Self)'Old,
                          P.Get (Positions (Self)'Old, Position)))

            --  Cursors that were valid in Self continue to be valid in Self
            --  except for the newly deleted cursor.
            --  Nothing is said about the order of keys in Self after the call.

            and (for all Position in Positions (Self)'Old =>
                   P.Mem (Positions (Self), Position) or
                     K.Get (Keys (Self)'Old,
                            P.Get (Positions (Self)'Old, Position)) = Key)

          --  If Key was not in Self, then nothing is changed.

          else Length (Self) = Length (Self)'Old
            and Model (Self)'Old = Model (Self)
            and Keys (Self)'Old = Keys (Self)
            and Positions (Self)'Old = Positions (Self));

   procedure Clear (Self : in out Map'Class)
   --  Remove all elements from the map

   with
     Global => null,
     Post   => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0
     and then M.Is_Empty (Model (Self));

   function Contains (Self : Map'Class; Key : Key_Type) return Boolean with
     Import,
     Global => null,
     Post   => Contains'Result = M.Mem (Model (Self), Key);

   function Key (Self : Map'Class; Position : Cursor) return Key_Type with
     Import,
     Global => null,
     Pre    => P.Mem (Positions (Self), Position),

     --  Query Positions to get the position of Position in L and use it to
     --  fetch the corresponding key in Keys.

     Post => Key'Result =
       K.Get (Keys (Self), P.Get (Positions (Self), Position));

   function Element (Self : Map'Class; Position : Cursor) return Element_Type
   with
     Global => null,
     Pre    => P.Mem (Positions (Self), Position),

     --  Query Positions to get the position of Position in L, use it to fetch
     --  the corresponding key in Keys, and then use this key to get the
     --  associated element from Model.

     Post   => Element'Result =
       Element (Model (Self),
                K.Get (Keys (Self), P.Get (Positions (Self), Position)));

   function Pair (Self : Map'Class; Position : Cursor) return Pair_Type
   with
     Global => null,
     Pre    => P.Mem (Positions (Self), Position),

     --  Query Positions to get the position of Position in L, use it to fetch
     --  the corresponding key in Keys, and then use this key to get the
     --  associated element from Model.

     Post   => Key (Pair'Result) =
        K.Get (Keys (Self), P.Get (Positions (Self), Position))
     and then Value (Pair'Result) = Element (Model (Self), Key (Pair'Result));

   function Find (Self : Map'Class; Key : Key_Type) return Cursor with
     Import,
     Global => null,
     Post   =>

       --  Either K is not in the model and the result is No_Element

       (Find'Result = No_Element
        and not M.Mem (Model (Self), Key))

     --  or the result is a valid cursor and K is stored at its position in V.

     or else
       (M.Mem (Model (Self), Key)
        and P.Mem (Positions (Self), Find'Result)
        and K.Get (Keys (Self), P.Get (Positions (Self), Find'Result)) = Key);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Map'Class) return Cursor with
     Global => null,
     Post   => (if Length (Self) = 0 then First'Result = No_Element
                else P.Mem (Positions (Self), First'Result) and then
                  P.Get (Positions (Self), First'Result) = 1);

   function Next (Self : Map'Class; Position : Cursor) return Cursor with
     Global => null,
     Pre    => P.Mem (Positions (Self), Position),
     Post   => (if P.Get (Positions (Self), Position) = Length (Self)
                then Next'Result = No_Element
                else P.Mem (Positions (Self), Next'Result)
                  and then P.Get (Positions (Self), Next'Result) =
                    P.Get (Positions (Self), Position) + 1);

   function Has_Element (Self : Map'Class; Position : Cursor) return Boolean
   with
     Global => null,
     Post   => Has_Element'Result = P.Mem (Positions (Self), Position);

private
   pragma SPARK_Mode (Off);

   type Map is new Element_Maps.Map with null record;

end Formal_Hashed_Maps;
