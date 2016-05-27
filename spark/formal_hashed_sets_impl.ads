pragma Ada_2012;
with Conts.Functional.Sequences;
with Conts.Functional.Maps;
with Conts.Functional.Sets;
with Conts;                  use Conts;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Formal_Hashed_Sets_Impl with SPARK_Mode is
   type Cursor is private;
   pragma Unevaluated_Use_Of_Old (Allow);

   No_Element : constant Cursor;

   type Base_Set is tagged limited private with
     Default_Initial_Condition => Length (Base_Set) = 0;
   --  Sets are empty when default initialized.

   function Capacity (Self : Base_Set'Class) return Count_Type with
     Import;

   function Length (Self : Base_Set'Class) return Count_Type with
     Import,
     Post => Length'Result < Capacity (Self);
   --  The length of a set is always strictly smaller than its capacity

   ------------------
   -- Formal Model --
   ------------------

   type P_Map is private with Ghost,
     Iterable => (First => P_Iter_First,
                  Has_Element => P_Iter_Has_Element,
                  Next => P_Iter_Next,
                  Element => P_Iter_Element);

   type P_Private_Cursor is private with Ghost;
   function P_Iter_First (M : P_Map) return P_Private_Cursor with Ghost;
   function P_Iter_Next (M : P_Map; C : P_Private_Cursor)
                         return P_Private_Cursor with Ghost;
   function P_Iter_Has_Element (M : P_Map; C : P_Private_Cursor)
                                return Boolean with Ghost;
   function P_Iter_Element (M : P_Map; C : P_Private_Cursor) return Cursor
     with Ghost;

   function P_Mem (M : P_Map; C : Cursor) return Boolean with Ghost;
   function P_Get (M : P_Map; C : Cursor) return Positive_Count_Type with
     Ghost,
     Pre => P_Mem (M, C);

   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", P_Mem);

   package E is new Conts.Functional.Sequences
     (Index_Type   => Positive_Count_Type,
      Element_Type => Element_Type);
   package M is new Conts.Functional.Sets
     (Element_Type => Element_Type);

   function Model (Self : Base_Set'Class) return M.Set with
     Ghost,
     Import;
   --  The highlevel model of a set is a set of elements. Neither cursors
   --  nor order of elements are represented in this model.

   function Elements (Self : Base_Set'Class) return E.Sequence with
   --  The Elements sequence represents the underlying list structure of
   --  sets that is used for iteration. It does not model cursors.

     Ghost,
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

   function Positions (Self : Base_Set'Class) return P_Map with
   --  The Positions map is used to model cursors. It only contains valid
   --  cursors and map them to their position in the container.

     Ghost,
     Import,
     Post => not P_Mem (Positions'Result, No_Element)

     --  Positions of cursors are smaller than the container's length.

     and then
       (for all I of Positions'Result =>
          P_Get (Positions'Result, I) in 1 .. Length (Self)

        --  No two cursors have the same position. Note that we do not state
        --  that there is a cursor in the map for each position, as it is
        --  rarely needed.

        and then
          (for all J of Positions'Result =>
             (if P_Get (Positions'Result, I) = P_Get (Positions'Result, J)
                  then I = J)));

   procedure Lift_Abstraction_Level (Self : Base_Set'Class) with
     Ghost,
     Import,
     Global => null,
     Post   =>
       (for all Elt of Elements (Self) =>
          (for some I of Positions (Self) =>
               E.Get (Elements (Self),  P_Get (Positions (Self), I)) = Elt));
   --  Lift_Abstraction_Level is a ghost procedure that does nothing but
   --  assume that we can access to the same elements by iterating over
   --  positions or cursors.
   --  This information is not generally useful except when switching from
   --  a lowlevel, cursor aware view of a container, to a highlevel position
   --  based view.

   use type M.Set;
   use type E.Sequence;
   use type P_Map;

   --  The following functions are modeled directly in the formal model.

   function Has_Element (Self : Base_Set'Class; Position : Cursor)
                         return Boolean
    with
     Import,
     Post => Has_Element'Result = P_Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Contains (Self : Base_Set'Class; Element : Element_Type)
                      return Boolean
   with
     Import,
     Post => Contains'Result = M.Mem (Model (Self), Element);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Contains);

   function Element (Self : Base_Set'Class; Position : Cursor)
                     return Element_Type
   with
     Import,
     Pre  => Has_Element (Self, Position),

     --  Query Positions to get the position of Position of L and use it to
     --  fetch the corresponding element in Elements.

     Post => Element'Result =
       E.Get (Elements (Self), P_Get (Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   --  The subprograms used for iteration over cursors are axiomatized using
   --  Positions only. They are inverse of the Positions map as they allow
   --  to create a valid cursor per position in the container.

   function First (Self : Base_Set'Class) return Cursor with
     Import,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => Has_Element (Self, First'Result)
        and then P_Get (Positions (Self), First'Result) = 1);

   function Next (Self : Base_Set'Class; Position : Cursor) return Cursor with
     Import,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P_Get (Positions (Self), Position) = Length (Self) =>
              Next'Result = No_Element,
        others                                             =>
          Has_Element (Self, Next'Result)
        and then P_Get (Positions (Self), Next'Result) =
          P_Get (Positions (Self), Position) + 1);

   function Find (Self : Base_Set'Class; Element : Element_Type) return Cursor
   with
     Import,
     Contract_Cases =>

     --  Either the result is a valid cursor and Element is stored at its
     --  position in S

     (Contains (Self, Element) => Has_Element (Self, Find'Result)
      and Formal_Hashed_Sets_Impl.Element (Self, Find'Result) = Element,

      --  or Element is not in the model and the result is No_Element.

      others                   => Find'Result = No_Element);

   procedure Include (Self : in out Base_Set'Class; Element : Element_Type)
   with
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
         and Formal_Hashed_Sets_Impl.Element (Self, Position) =
             E.Get (Elements (Self)'Old,
                    P_Get (Positions (Self)'Old, Position)))

      --  Cursors that are valid in Self were already valid in Self
      --  except for the newly inserted cursor.
      --  Nothing is said about the order of elements in Self after the
      --  call.

      and (for all Position of Positions (Self) =>
          P_Mem (Positions (Self)'Old, Position)
            or Formal_Hashed_Sets_Impl.Element (Self, Position) = Element));

   procedure Exclude (Self : in out Base_Set'Class; Element : Element_Type)
   with
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
          P_Mem (Positions (Self)'Old, Position)
        and Formal_Hashed_Sets_Impl.Element (Self, Position) =
          E.Get (Elements (Self)'Old,
            P_Get (Positions (Self)'Old, Position)))

      --  Cursors that were valid in Self are still valid in Self except
      --  for the removed cursor.
      --  Nothing is said about the order of elements in Self after the
      --  call.

      and (for all Position of Positions (Self)'Old =>
          Has_Element (Self, Position)
        or E.Get (Elements (Self)'Old,
          P_Get (Positions (Self)'Old, Position)) =
          Element),

      --  If Element is not in Self, then the model is unchanged.

      others                  =>
        Length (Self) = Length (Self)'Old
      and Model (Self) = Model (Self)'Old
      and Elements (Self) = Elements (Self)'Old
      and Positions (Self) = Positions (Self)'Old);

   procedure Union (Self : in out Base_Set'Class; Source : Base_Set'Class) with
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
                     P_Get (Positions (Self)'Old, Position)));

   procedure Intersection
     (Self : in out Base_Set'Class; Source : Base_Set'Class)
   --  Exclude from Self all the elements of Source

   with
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
              P_Mem (Positions (Self)'Old, Position)
          and Element (Self, Position) =
              E.Get (Elements (Self)'Old,
                     P_Get (Positions (Self)'Old, Position)));

   procedure Clear (Self : in out Base_Set'Class)
   with
     Import,
     Post => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0
     and then M.Is_Empty (Model (Self));

   function First_Primitive (Self : Base_Set) return Cursor with Import;

   function Element_Primitive
     (Self : Base_Set; Position : Cursor) return Element_Type
   with
     Import,
     Pre'Class => Has_Element (Self, Position),
     Post =>
           Element_Primitive'Result =
             E.Get (Elements (Self), P_Get (Positions (Self), Position));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);

   function Has_Element_Primitive
     (Self : Base_Set; Position : Cursor) return Boolean
   with
     Import,
     Post =>
       Has_Element_Primitive'Result = P_Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);

   function Next_Primitive
     (Self : Base_Set; Position : Cursor) return Cursor
   with
     Import,
     Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);

   type Cursor is record
      I : Natural;
   end record;
   type Base_Set is tagged limited null record;
   No_Element : constant Cursor := (I => 0);

   package P is new Conts.Functional.Maps
     (Element_Type => Positive_Count_Type,
      Key_Type     => Cursor);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   type P_Map is record
      Content : P.Map;
   end record;

   type P_Private_Cursor is new P.Private_Key;

   function P_Iter_First (M : P_Map) return P_Private_Cursor is
      (P_Private_Cursor (P.Iter_First (M.Content)));
   function P_Iter_Next (M : P_Map; C : P_Private_Cursor)
                         return P_Private_Cursor is
      (P_Private_Cursor (P.Iter_Next (M.Content, P.Private_Key (C))));
   function P_Iter_Has_Element (M : P_Map; C : P_Private_Cursor)
                                return Boolean is
      (P.Iter_Has_Element (M.Content, P.Private_Key (C)));
   function P_Iter_Element (M : P_Map; C : P_Private_Cursor) return Cursor is
      (P.Iter_Element (M.Content, P.Private_Key (C)));
   function P_Mem (M : P_Map; C : Cursor) return Boolean is
      (P.Mem (M.Content, C));
   function P_Get (M : P_Map; C : Cursor) return Positive_Count_Type is
      (P.Get (M.Content, C));

end Formal_Hashed_Sets_Impl;
