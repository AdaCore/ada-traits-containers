pragma Ada_2012;
with Conts.Functional.Sequences;
with Conts.Functional.Maps;
with Conts.Functional.Sets;
with Conts;                  use Conts;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
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
   --  instantiated in the private part but then the cursor type could not be
   --  used to instanciate the Functional_Maps package for the Formal_Model.
   --  To be replaced with an instance of the proper Set package.

   subtype Cursor is Element_Sets.Cursor;
   use all type Element_Sets.Cursor;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : Cursor renames Element_Sets.No_Element;

   type Set is tagged limited private with
     Default_Initial_Condition => Length (Set) = 0,
     Iterable => (First       => First_Primitive,
                  Next        => Next_Primitive,
                  Has_Element => Has_Element_Primitive,
                  Element     => Element_Primitive);
   --  Sets are empty when default initialized.
   --  Iteration over sets can be done over cursors or over elements.

   function Capacity (Self : Set'Class) return Count_Type with
     Import;

   function Length (Self : Set'Class) return Count_Type with
     Import,
     Post => Length'Result < Capacity (Self);
   --  The length of a set is always strictly smaller than its capacity

   package Formal_Model with Ghost is

      package P is new Conts.Functional.Maps
        (Element_Type => Positive_Count_Type,
         Key_Type     => Cursor);
      package E is new Conts.Functional.Sequences
        (Index_Type   => Positive_Count_Type,
         Element_Type => Element_Type);
      package M is new Conts.Functional.Sets
        (Element_Type => Element_Type);

      function Model (Self : Set'Class) return M.Set with
        Import;
      --  The highlevel model of a set is a set of elements. Neither cursors
      --  nor order of elements are represented in this model.

      pragma Annotate (GNATprove, Iterable_For_Proof, "Model", Model);

      function Elements (Self : Set'Class) return E.Sequence with
      --  The Elements sequence represents the underlying list structure of
      --  sets that is used for iteration. It does not model cursors.

        Import,
        Post => E.Length (Elements'Result) = Length (Self)

        --  It only contains elements contained of Model.

        and then (for all Elt of Elements'Result =>
                      M.Mem (Model (Self), Elt))

        --  It contains all the elements contained of Model.

        and then (for all Elt of Model (Self) =>
                      (for some F of Elements'Result => F = Elt))

        --  It has no duplicate.

        and then
            (for all I in 1 .. Length (Self) =>
               (for all J in 1 .. Length (Self) =>
                    (if E.Get (Elements'Result, I) = E.Get (Elements'Result, J)
                     then I = J)));

      function Positions (Self : Set'Class) return P.Map with
      --  The Positions map is used to model cursors. It only contains valid
      --  cursors and map them to their position in the container.

        Import,
        Post => not P.Mem (Positions'Result, No_Element)

        --  Positions of cursors are smaller than the container's length.

        and then
          (for all I of Positions'Result =>
             P.Get (Positions'Result, I) in 1 .. Length (Self)

           --  No two cursors have the same position. Note that we do not state
           --  that there is a cursor in the map for each position, as it is
           --  rarely needed.

           and then
             (for all J of Positions'Result =>
                (if P.Get (Positions'Result, I) = P.Get (Positions'Result, J)
                 then I = J)));

      procedure Lift_Abstraction_Level (Self : Set'Class) with
        Import,
        Global => null,
        Post   =>
          (for all Elt of Elements (Self) =>
             (for some I of Positions (Self) =>
                E.Get (Elements (Self),  P.Get (Positions (Self), I)) = Elt));
      --  Lift_Abstraction_Level is a ghost procedure that does nothing but
      --  assume that we can access to the same elements by iterating over
      --  positions or cursors.
      --  This information is not generally useful except when switching from
      --  a lowlevel, cursor aware view of a container, to a highlevel position
      --  based view.
   end Formal_Model;

   package M renames Formal_Model.M;
   package E renames Formal_Model.E;
   package P renames Formal_Model.P;

   use type M.Set;
   use type E.Sequence;
   use type P.Map;

   function Model (Self : Set'Class) return M.Set
                   renames Formal_Model.Model;
   function Elements (Self : Set'Class) return E.Sequence
                   renames Formal_Model.Elements;
   function Positions (Self : Set'Class) return P.Map
                       renames Formal_Model.Positions;

   --  The following functions are modeled directly in the formal model.

   function Has_Element (Self : Set'Class; Position : Cursor) return Boolean
    with
     Import,
     Post => Has_Element'Result = P.Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Contains (Self : Set'Class; Element : Element_Type) return Boolean
   with
     Import,
     Post => Contains'Result = M.Mem (Model (Self), Element);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Contains);

   function Element (Self : Set'Class; Position : Cursor) return Element_Type
   with
     Import,
     Pre  => Has_Element (Self, Position),

     --  Query Positions to get the position of Position of L and use it to
     --  fetch the corresponding element in Elements.

     Post => Element'Result =
       E.Get (Elements (Self), P.Get (Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Set'Class) return Cursor with
     Import,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => Has_Element (Self, First'Result)
        and then P.Get (Positions (Self), First'Result) = 1);

   function Next (Self : Set'Class; Position : Cursor) return Cursor with
     Import,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P.Get (Positions (Self), Position) = Length (Self) =>
              Next'Result = No_Element,
        others                                             =>
          Has_Element (Self, Next'Result)
        and then P.Get (Positions (Self), Next'Result) =
          P.Get (Positions (Self), Position) + 1);

   function Find (Self : Set'Class; Element : Element_Type) return Cursor with
     Import,
     Contract_Cases =>

     --  Either the result is a valid cursor and Element is stored at its
     --  position in S

     (Contains (Self, Element) => Has_Element (Self, Find'Result)
      and Formal_Hashed_Sets.Element (Self, Find'Result) = Element,

      --  or Element is not in the model and the result is No_Element.

      others                   => Find'Result = No_Element);

   procedure Include (Self : in out Set'Class; Element : Element_Type) with
   --  Insert an element Element in Self if Element is not already present.

     Import,
     Pre            => Length (Self) < Conts.Count_Type'Last - 1
     or else Contains (Self, Element),
     Contract_Cases =>

     --  If Element is already in Self, then the model is unchanged.

     (Contains (Self, Element) =>
          Capacity (Self) = Capacity (Self)'Old
      and Length (Self) = Length (Self)'Old
      and Model (Self) = Model (Self)'Old
      and Elements (Self) = Elements (Self)'Old
      and Positions (Self) = Positions (Self)'Old,

        --  If Element is not in Self, then Element is a new element of its
        --  model.

      others                   =>
          Capacity (Self) >= Capacity (Self)'Old
      and Length (Self) = Length (Self)'Old + 1
      and M.Is_Add (Model (Self)'Old, Element, Model (Self))

      --  Cursors that were valid in Self are still valid and continue
      --  designating the same element.

      and
        (for all Position of Positions (Self)'Old =>
             Has_Element (Self, Position)
         and Formal_Hashed_Sets.Element (Self, Position) =
             E.Get (Elements (Self)'Old,
                    P.Get (Positions (Self)'Old, Position)))

      --  Cursors that are valid in Self were already valid in Self
      --  except for the newly inserted cursor.
      --  Nothing is said about the order of elements in Self after the
      --  call.

      and (for all Position of Positions (Self) =>
          P.Mem (Positions (Self)'Old, Position)
            or Formal_Hashed_Sets.Element (Self, Position) = Element));

   procedure Exclude (Self : in out Set'Class; Element : Element_Type) with
   --  Remove an element Element of Self if it is present.

     Import,
     Post           => Capacity (Self) = Capacity (Self)'Old,
     Contract_Cases =>

     --  If Element is in Self, then it is removed from its model.

     (Contains (Self, Element) =>
          Length (Self) = Length (Self)'Old - 1
      and M.Is_Add (Model (Self), Element, Model (Self)'Old)

      --  Cursors that are valid in Self were already valid and continue
      --  designating the same element.

      and (for all Position of Positions (Self) =>
          P.Mem (Positions (Self)'Old, Position)
        and Formal_Hashed_Sets.Element (Self, Position) =
          E.Get (Elements (Self)'Old,
            P.Get (Positions (Self)'Old, Position)))

      --  Cursors that were valid in Self are still valid in Self except
      --  for the removed cursor.
      --  Nothing is said about the order of elements in Self after the
      --  call.

      and (for all Position of Positions (Self)'Old =>
          Has_Element (Self, Position)
        or E.Get (Elements (Self)'Old,
          P.Get (Positions (Self)'Old, Position)) =
          Element),

      --  If Element is not in Self, then the model is unchanged.

      others                  =>
        Length (Self) = Length (Self)'Old
      and Model (Self) = Model (Self)'Old
      and Elements (Self) = Elements (Self)'Old
      and Positions (Self) = Positions (Self)'Old);

   procedure Union (Self : in out Set'Class; Source : Set'Class) with
   --  Include in Self all the elements of Source

     Import,
     Pre  => Length (Source) < Conts.Count_Type'Last - Length (Self),
     Post => Capacity (Self) >= Capacity (Self)'Old

     --  The model of Self is the union of the previous model of Self and the
     --  model of Source.

     and M.Is_Union (Model (Self)'Old, Model (Source), Model (Self))

     --  No more than Length (Source) elements were added to Source. We could
     --  be more precise by using the length of the Intersection if we had a
     --  notion of length on functional sets.

     and Length (Self) in Length (Self)'Old ..
         Length (Self)'Old + Length (Source)

     --  Cursors that were valid in Self are still valid and continue
     --  designating the same element.
     --  Nothing is said about the order of elements in Self after the call.

     and (for all Position of Positions (Self)'Old =>
              Has_Element (Self, Position)
          and Element (Self, Position) =
              E.Get (Elements (Self)'Old,
                     P.Get (Positions (Self)'Old, Position)));

   procedure Intersection (Self : in out Set'Class; Source : Set'Class) with
   --  Exclude from Self all the elements of Source

     Import,
     Post => Capacity (Self) = Capacity (Self)'Old

     --  The model of Self is the intersection of the previous model of Self
     --  and the model of Source.

     and M.Is_Intersection (Model (Self)'Old, Model (Source), Model (Self))

     --  The length of Self can only be decreased. We could be more precise by
     --  stating that at most Length (Source) elements have been removed from
     --  Self.

     and Length (Self) in 0 .. Length (Self)'Old

     --  Cursors that are valid in Self we already valid and continue
     --  designating the same element.
     --  Nothing is said about the order of elements in Self after the call.

     and (for all Position of Positions (Self) =>
              P.Mem (Positions (Self)'Old, Position)
          and Element (Self, Position) =
              E.Get (Elements (Self)'Old,
                     P.Get (Positions (Self)'Old, Position)));

   procedure Clear (Self : in out Set'Class)
   with
     Import,
     Post => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0
     and then M.Is_Empty (Model (Self));

   function First_Primitive (Self : Set) return Cursor with Import;

   function Element_Primitive
     (Self : Set; Position : Cursor) return Element_Type
   with
     Import,
     Pre'Class => Has_Element (Self, Position),
     Post =>
           Element_Primitive'Result =
             E.Get (Elements (Self), P.Get (Positions (Self), Position));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);

   function Has_Element_Primitive
     (Self : Set; Position : Cursor) return Boolean
   with
     Import,
     Post =>
       Has_Element_Primitive'Result = P.Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);

   function Next_Primitive
     (Self : Set; Position : Cursor) return Cursor
   with
     Import,
     Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);

   type Set is new Element_Sets.Set with null record;

end Formal_Hashed_Sets;
