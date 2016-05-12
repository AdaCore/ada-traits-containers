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
     Default_Initial_Condition => Length (Map) = 0,
     Iterable => (First       => First_Primitive,
                  Next        => Next_Primitive,
                  Has_Element => Has_Element_Primitive,
                  Element     => Element_Primitive);
   --  Maps are empty when default initialized.
   --  Iteration over maps can be done over cursors or over keys.

   function Capacity (Self : Map'Class) return Natural with
     Global => null;

   function Length (Self : Map'Class) return Natural with
     Import,
     Global => null,
     Post   => Length'Result < Capacity (Self);
   --  The length of a map is always strictly smaller than its capacity

   package Formal_Model with Ghost is

      package P is new Functional_Maps
        (Element_Type => Positive,
         Key_Type     => Cursor);
      package K is new Functional_Sequences
        (Element_Type => Key_Type,
         Index_Type   => Positive);
      package M is new Functional_Maps
        (Element_Type => Element_Type,
         Key_Type     => Key_Type);

      function Model (Self : Map'Class) return M.Map with
        Global => null;
      --  The highlevel model of a map is a map from keys to elements. Neither
      --  cursors nor order of elements are represented in this model.

      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

      function Keys (Self : Map'Class) return K.Sequence with
      --  The Keys sequence represents the underlying list structure of maps
      --  that is used for iteration. It does not model cursors nor elements.

        Global => null,
        Post   => K.Length (Keys'Result) = Length (Self)

        --  It only contains keys contained in Model.

        and then (for all Key of Keys'Result => M.Mem (Model (Self), Key))

        --  It contains all the keys contained in Model.

        and then (for all Key of Model (Self) =>
                      (for some L of Keys'Result => L = Key))

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
        Post   => not P.Mem (Positions'Result, No_Element)

        --  Positions of cursors are smaller than the container's length.

        and then
          (for all I of Positions'Result =>
             P.Get (Positions'Result, I) in 1 .. Length (Self)

           --  No two cursors have the same position. Note that we do not state
           --  that there is a cursor in the map for each position, as it is
           --  rarely needed.

           and then
             (for all J of Positions'Result =>
                (if P.Get (Positions'Result, I) =
                     P.Get (Positions'Result, J)
                     then I = J)));

      procedure Lift_Abstraction_Level (Self : Map'Class) with
        Global => null,
        Post   =>
          (for all Key of Keys (Self) =>
             (for some Cu of Positions (Self) =>
                  K.Get (Keys (Self),  P.Get (Positions (Self), Cu)) = Key));
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

   --  The following functions are modeled directly in the formal model.

   function Contains (Self : Map'Class; Key : Key_Type) return Boolean with
     Import,
     Global => null,
     Post   => Contains'Result = M.Mem (Model (Self), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Contains);

   function Get (Self : Map'Class; Key : Key_Type) return Element_Type with
     Global => null,
     Pre    => Contains (Self, Key),
     Post   => Get'Result = Element (Model (Self), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Get);

   function Has_Element (Self : Map'Class; Position : Cursor) return Boolean
   with
     Global => null,
     Post   => Has_Element'Result = P.Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Key (Self : Map'Class; Position : Cursor) return Key_Type with
     Import,
     Global => null,
     Pre    => Has_Element (Self, Position),

     --  Query Positions to get the position of Position in Self and use it to
     --  fetch the corresponding key in Keys.

     Post => Key'Result =
       K.Get (Keys (Self), P.Get (Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Key);

   function Element (Self : Map'Class; Position : Cursor) return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Self, Position),

     --  Query Positions to get the position of Position in Self, use it to
     --  fetch the corresponding key in Keys, and then use this key to get the
     --  associated element from Model.

     Post   => Element'Result =
       Element (Model (Self),
                K.Get (Keys (Self), P.Get (Positions (Self), Position)));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   procedure Set
     (Self : in out Map'Class; Key : Key_Type; Element : Element_Type)
   --  Insert a key Key and an element Element in Self if Key is not already in
   --  present.
   --  Otherwise, replace the element associated to Key by Element.

   with
     Global         => null,
     Pre            => Contains (Self, Key)
     or else Length (Self) < Count_Type'Last - 1,
     Contract_Cases =>

      --  If Key is already in Self, then Key now maps to Element in Model.

     (Contains (Self, Key) =>
          Capacity (Self) = Capacity (Self)'Old
      and Length (Self) = Length (Self)'Old
      and M.Is_Replace (Model (Self)'Old, Key, Element, Model (Self))

      --  Keys and cursors are preserved

      and Keys (Self) = Keys (Self)'Old
      and Positions (Self) = Positions (Self)'Old,

      --  If Key was not in Self, then Element is a new element of its
      --  model.

      others               =>
        Capacity (Self) >= Capacity (Self)'Old
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
     Global         => null,
     Post           => Capacity (Self) = Capacity (Self)'Old,
     Contract_Cases =>

     --  If Key was in Self then it is removed from its model.

     (Contains (Self, Key) =>
          Length (Self) = Length (Self)'Old - 1
      and M.Is_Add (Model (Self),
                    Key,
                    Element (Model (Self)'Old, Key),
                    Model (Self)'Old)

      --  Cursors that are valid in Self were already valid and
      --  designating the same element.

      and (for all Position of Positions (Self) =>
          P.Mem (Positions (Self)'Old, Position) and
            Formal_Hashed_Maps.Key (Self, Position) =
          K.Get (Keys (Self)'Old,
            P.Get (Positions (Self)'Old, Position)))

      --  Cursors that were valid in Self continue to be valid in Self
      --  except for the newly deleted cursor.
      --  Nothing is said about the order of keys in Self after the call.

      and (for all Position of Positions (Self)'Old =>
          Has_Element (Self, Position) or
          K.Get (Keys (Self)'Old,
            P.Get (Positions (Self)'Old, Position)) = Key),

      --  If Key was not in Self, then nothing is changed.

      others               =>
        Length (Self) = Length (Self)'Old
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

   function Find (Self : Map'Class; Key : Key_Type) return Cursor with
     Import,
     Global => null,
     Contract_Cases   =>

     --  Either the result is a valid cursor and K is stored at its position
     --  in Self.

     (Contains (Self, Key) =>
          Has_Element (Self, Find'Result)
      and Formal_Hashed_Maps.Key (Self, Find'Result) = Key,

      --  or K is not in the model and the result is No_Element

      others => Find'Result = No_Element);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Map'Class) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => Has_Element (Self, First'Result)
        and then P.Get (Positions (Self), First'Result) = 1);

   function Next (Self : Map'Class; Position : Cursor) return Cursor with
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P.Get (Positions (Self), Position) = Length (Self) =>
              Next'Result = No_Element,
        others                                             =>
          Has_Element (Self, Next'Result)
        and then P.Get (Positions (Self), Next'Result) =
          P.Get (Positions (Self), Position) + 1);

   function First_Primitive (Self : Map) return Cursor;

   function Element_Primitive
     (Self : Map; Position : Cursor) return Key_Type
   with
     Inline,
     Pre'Class => Has_Element (Self, Position),
     Post =>
           Element_Primitive'Result =
             K.Get (Keys (Self), P.Get (Positions (Self), Position));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);

   function Has_Element_Primitive
     (Self : Map; Position : Cursor) return Boolean
   with Post =>
       Has_Element_Primitive'Result = P.Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);

   function Next_Primitive
     (Self : Map; Position : Cursor) return Cursor
   with
     Inline,
     Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);

   type Map is new Element_Maps.Map with null record;

end Formal_Hashed_Maps;
