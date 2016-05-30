------------------------------------------------------------------------------
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  A vector abstract data type

pragma Ada_2012;
with Conts.Vectors.Storage;
with Conts.Functional.Sequences;
with Conts.Functional.Sets;

generic
   type Index_Type is (<>);
   --  Because Last needs to return a meaningful value for empty vectors,
   --  (Index_Type'First - 1) must be a valid value in Index_Type'Base.
   --  This means that the index type cannot be Integer.
   --  Nor can it be an enumeration type. However, this would not be a good
   --  use case for vectors anyway, since the number of elements is known at
   --  compile time and a standard Ada array would be more efficient.

   with package Storage is new Conts.Vectors.Storage.Traits (<>);
package Conts.Vectors.Impl with SPARK_Mode is

   subtype Extended_Index is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;
   --  Index_Type with one more element to the left, used to represent
   --  invalid indexes

   subtype Element_Type is Storage.Elements.Element_Type;
   subtype Returned_Type is Storage.Elements.Returned_Type;
   subtype Constant_Returned_Type is Storage.Elements.Constant_Returned_Type;
   subtype Stored_Type is Storage.Elements.Stored_Type;

   use type Storage.Elements.Element_Type;
   use type Storage.Elements.Constant_Returned_Type;
   use type Storage.Elements.Returned_Type;

   type Base_Vector is new Storage.Container with private with
     Default_Initial_Condition => Length (Base_Vector) = 0;
   --  Define the iterable aspect later, since this is not allowed when the
   --  parent type is a generic formal.

   type Cursor is private;
   No_Element : constant Cursor;

   Last_Count : constant Count_Type :=
     (if Index_Type'Pos (Index_Type'Last) < Index_Type'Pos (Index_Type'First)
      then 0
      elsif Index_Type'Pos (Index_Type'Last) < -1
      or else Index_Type'Pos (Index_Type'First) >
          Index_Type'Pos (Index_Type'Last) - Count_Type'Last
      then Index_Type'Pos (Index_Type'Last) -
          Index_Type'Pos (Index_Type'First) + 1
      else Count_Type'Last);
   --  Maximal capacity of any vector. It is the minimum of the size of the
   --  index range and the last possible Count_Type.

   function Max_Capacity (Self : Base_Vector'Class) return Count_Type is
      (Count_Type'Min (Last_Count, Storage.Max_Capacity (Self)));
   --  Maximal capacity of Self. It cannot be modified. It is the maximal
   --  number of elements a vector may contain.

   function Length (Self : Base_Vector'Class) return Count_Type with
     Inline,
     Global => null,
     Post   => Length'Result <= Max_Capacity (Self);
   --  The length of a vector is always smaller than Max_Capacity.

   function Last (Self : Base_Vector'Class) return Extended_Index with
     Inline,
     Global => null,
     Post   => Last'Result =
       Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1)
                       + Length (Self));
   --  On an empty vector, Last returns Extended_Index'First.

   function To_Index (Position : Cursor) return Index_Type with
     Global => null,
     Pre    => Position /= No_Element;
   --  Return the index corresponding to the cursor.
   --  In vectors, cursors cannot change positions. They are associated with a
   --  unique index by To_Index.

   function To_Index (Position : Count_Type) return Extended_Index with
     Global => null,
     Pre    => Position in 0 .. Last_Count,
     Post   => To_Index'Result = Index_Type'Val
       (Position - Conts.Vectors.Storage.Min_Index
        + Count_Type'Base (Index_Type'Pos (Index_Type'First)));

   function To_Count (Idx : Index_Type) return Count_Type with
     Inline,
     Global => null,
     Pre    => Idx in Index_Type'First .. To_Index (Last_Count),
     Post   => To_Index (To_Count'Result) = Idx;
   --  Converts from an index type to an index into the actual underlying
   --  array.

   ------------------
   -- Formal Model --
   ------------------

   pragma Unevaluated_Use_Of_Old (Allow);

   type V_Set is private with Ghost,
     Iterable => (First => V_Iter_First,
                  Has_Element => V_Iter_Has_Element,
                  Next => V_Iter_Next,
                  Element => V_Iter_Element);

   type V_Private_Cursor is private with Ghost;
   function V_Iter_First (S : V_Set) return V_Private_Cursor with Ghost;
   function V_Iter_Next (S : V_Set; C : V_Private_Cursor)
                         return V_Private_Cursor with Ghost;
   function V_Iter_Has_Element (S : V_Set; C : V_Private_Cursor)
                                return Boolean with Ghost;
   function V_Iter_Element (S : V_Set; C : V_Private_Cursor) return Cursor
     with Ghost;

   function V_Mem (S : V_Set; C : Cursor) return Boolean with Ghost;

   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", V_Mem);

   package M is new Conts.Functional.Sequences
     (Index_Type   => Index_Type,
      Element_Type => Element_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   function Model (Self : Base_Vector'Class) return M.Sequence with
     Ghost,
     Global => null,
     Post   => M.Length (Model'Result) = Length (Self);
   --  The highlevel model of a vector is a sequence of elements indexed by
   --  Index_Type. Cursors are not represented in this model.

   function Valid_Cursors (Self : Base_Vector'Class) return V_Set with
   --  Valid_Cursors is the set of cursors that are valid in Self.
   --  No need to store their position in Self as it can be retrieved with
   --  To_Index.

     Ghost,
     Global => null,
     Post   => not V_Mem (Valid_Cursors'Result, No_Element)

     --  Positions of cursors are smaller than the container's last index.

     and then
       (for all I of Valid_Cursors'Result =>
          To_Index (I) in Index_Type'First .. Last (Self)

            --  There is no more than one cursor per position in the container.

        and then
          (for all J of Valid_Cursors'Result =>
             (if To_Index (I) = To_Index (J) then I = J)));

   procedure Lift_Abstraction_Level (Self : Base_Vector'Class) with
     Ghost,
     Global => null,
     Post   =>
       (for all Elt of Model (Self) =>
          (for some I of Valid_Cursors (Self) =>
               M.Get (Model (Self), To_Index (I)) = Elt));
   --  Lift_Abstraction_Level is a ghost procedure that does nothing but
   --  assume that we can access to the same elements by iterating over
   --  positions or cursors.
   --  This information is not generally useful except when switching from
   --  a lowlevel, cursor aware view of a container, to a highlevel position
   --  based view.

   use type M.Sequence;
   function Element (S : M.Sequence; I : Index_Type) return Element_Type
                     renames M.Get;

   -----------------
   -- Subprograms --
   -----------------

   function Element
     (Self : Base_Vector'Class; Position : Index_Type)
         return Constant_Returned_Type
   with Inline,
     Global => null,
     Pre    => Position <= Last (Self),
     Post   => Element'Result = Storage.Elements.To_Constant_Returned
       (Storage.Elements.To_Stored (Element (Model (Self), Position)));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element);

   function Reference
     (Self : Base_Vector'Class; Position : Index_Type)
         return Returned_Type
   with Inline,
     Global => null,
     Pre    => Position <= Last (Self),
     Post   => Reference'Result = Storage.Elements.To_Returned
       (Storage.Elements.To_Stored (Element (Model (Self), Position)));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Reference);

   function Element
     (Self : Base_Vector'Class; Position : Cursor)
         return Constant_Returned_Type
   with
     Global => null,
     Pre    => Has_Element (Self, Position),
     Post   => Element'Result = Storage.Elements.To_Constant_Returned
       (Storage.Elements.To_Stored
          (Element (Model (Self), To_Index (Position))));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   function Last_Element
     (Self : Base_Vector'Class) return Constant_Returned_Type with
     Global => null,
     Pre    => Length (Self) > 0,
     Post   => Last_Element'Result = Element (Self, Last (Self));

   function First (Self : Base_Vector'Class) return Cursor with
     Inline,
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => To_Index (First'Result) = Index_Type'First
        and then Has_Element (Self, First'Result));

   function Has_Element
     (Self : Base_Vector'Class; Position : Cursor) return Boolean
   with
     Inline,
     Global => null,
       Post   => Has_Element'Result = V_Mem (Valid_Cursors (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Next
     (Self : Base_Vector'Class; Position : Cursor) return Cursor
   with Inline,
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) < Last (Self) =>
              To_Index (Next'Result) = Index_Type'Succ (To_Index (Position))
        and Has_Element (Self, Next'Result),
        others                            => Next'Result = No_Element);

   procedure Next (Self : Base_Vector'Class; Position : in out Cursor)
   with Inline,
     Global => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) < Last (Self) =>
              To_Index (Position) = Index_Type'Succ (To_Index (Position'Old))
        and Has_Element (Self, Position),
        others                            => Position = No_Element);

   function Previous
     (Self : Base_Vector'Class; Position : Cursor) return Cursor
   with Inline,
     Global           => null,
     Pre              => Has_Element (Self, Position),
     Contract_Cases   =>
       (To_Index (Position) > Index_Type'First =>
             To_Index (Previous'Result) = Index_Type'Pred (To_Index (Position))
        and Has_Element (Self, Previous'Result),
        others                            => Previous'Result = No_Element);

   procedure Reserve_Capacity
     (Self : in out Base_Vector'Class; Capacity : Count_Type)
   --  Make sure there is enough space for at least Count_Type elements in
   --  Self.

   with
       Global => null,
       Pre    => Capacity <= Max_Capacity (Self),
       Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old;

   procedure Shrink_To_Fit (Self : in out Base_Vector'Class)
   --  Resize the vector to fit its number of elements.
   --  This has no effect on models.

   with
     Global => null,
     Post   => Length (Self) = Length (Self)'Old
     and then Model (Self) = Model (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old;

   function M_Elements_Equal
     (S1, S2 : M.Sequence;
      Fst    : Index_Type;
      Lst    : Extended_Index)
      return Boolean
   --  The slice from Fst to Lst contains the same values in S1 and S2.

   with Ghost,
     Pre  => Lst <= M.Last (S1) and Lst <= M.Last (S2),
     Post => M_Elements_Equal'Result =
     (for all I in Fst .. Lst => Element (S1, I) = Element (S2, I));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Equal);

   function M_Elements_Consts
     (S   : M.Sequence;
      Fst : Index_Type;
      Lst : Extended_Index;
      E   : Storage.Elements.Element_Type)
      return Boolean
   --  The slice from Fst to Lst contains only the value E.

   with Ghost,
     Pre  => Lst <= M.Last (S),
     Post => M_Elements_Consts'Result =
     (for all I in Fst .. Lst => Element (S, I) = E);
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Consts);

   procedure Resize
     (Self    : in out Base_Vector'Class;
      Length  : Index_Type;
      Element : Storage.Elements.Element_Type)
   --  Resize the container so that it contains Length elements.
   --  If Length is smaller than the current container length, Self is
   --     reduced to its first Length elements, destroying the other elements.
   --  If Length is greater than the current container length, new elements
   --     are added as needed, as copied of Element.

   with
     Global => null,
     Pre    => Length <= To_Index (Max_Capacity (Self)),
     Post   => Impl.Length (Self) =
             Index_Type'Pos (Length) - Index_Type'Pos (Index_Type'First) + 1

     --  Elements of Self that were located before the index Length are
     --  preserved.

     and then M_Elements_Equal
       (S1  => Model (Self),
        S2  => Model (Self)'Old,
        Fst => Index_Type'First,
        Lst => Index_Type'Min (Length, Last (Self)'Old))

     --  If elements were appended to Self then they are equal to Element.

     and then
       (if Length > Last (Self)'Old then
              M_Elements_Consts (S   => Model (Self),
                                 Fst => Index_Type'Succ (Last (Self)'Old),
                                 Lst => Last (Self),
                                 E   => Element));

   procedure Clear (Self : in out Base_Vector'Class) with
     Global => null,
     Post   => Length (Self) = 0;

   procedure Append
     (Self    : in out Base_Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   --  Append Count copies of Element to the vector.

   with
     Global => null,
     Pre    => Length (Self) <= Max_Capacity (Self) - Count,
     Post   => Length (Self) = Length (Self)'Old + Count

     --  Elements that were already in Self are preserved.

     and then M_Elements_Equal
       (S1  => Model (Self),
        S2  => Model (Self)'Old,
        Fst => Index_Type'First,
        Lst => Last (Self)'Old)

     --  Appended elements are equal to Element.

     and then M_Elements_Consts (S   => Model (Self),
                                 Fst => Index_Type'Succ (Last (Self)'Old),
                                 Lst => Last (Self),
                                 E   => Element);

   procedure Assign
     (Self : in out Base_Vector'Class; Source : Base_Vector'Class)
   with
     Global => null,
     Post   => Model (Self) = Model (Source);

   function Is_Empty (Self : Base_Vector'Class) return Boolean is
     (Length (Self) = 0)
   with Inline;

   procedure Replace_Element
     (Self     : in out Base_Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   --  Replace the element at the given position.
   --  Nothing is done if Index is not a valid index in the container.

   with
     Global => null,
     Pre    => Index <= Last (Self),  --  ??? why did you add this precondition
     Post   => Length (Self) = Length (Self)'Old
     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old
     and then
       (if Index <= Last (Self)
        then M.Is_Set (Model (Self)'Old, Index, New_Item, Model (Self))
          else Model (Self) = Model (Self)'Old);

   function M_Elements_Shifted
     (S1, S2   : M.Sequence;
      Fst, Lst : Index_Type)
      return Boolean
   --  The slice from Fst to Lst of S1 has been shifted by 1 in S2.

   with Ghost,
     Pre  => Lst <= M.Last (S1) and Lst < M.Last (S2),
     Post => M_Elements_Shifted'Result =
       (for all I in Fst .. Lst => Element (S1, I) =
              Element (S2, Index_Type'Succ (I)));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Shifted);

   procedure Delete (Self : in out Base_Vector'Class; Index : Index_Type)
   --  Remove an element from the vector.

   with
     Global => null,
     Pre    => Index <= Last (Self),
     Post   => Length (Self) = Length (Self)'Old - 1

     --  Elements located before Index are preserved.

     and then M_Elements_Equal
       (S1  => Model (Self),
        S2  => Model (Self)'Old,
        Fst => Index_Type'First,
        Lst => Index_Type'Pred (Index))

     --  Elements located after Index are shifted.

     and then M_Elements_Shifted
       (S1  => Model (Self),
        S2  => Model (Self)'Old,
        Fst => Index,
        Lst => Last (Self));

   procedure Delete_Last (Self : in out Base_Vector'Class) with
   --  Remove the last element of the vector.
   --  ??? are cursors preserved?

     Global => null,
     Pre    => Length (Self) > 0,
     Post   => Length (Self) = Length (Self)'Old - 1
     and then  M_Elements_Equal
       (S1  => Model (Self),
        S2  => Model (Self)'Old,
        Fst => Index_Type'First,
        Lst => Last (Self));

   function M_Elements_Equal_Except
     (S1, S2 : M.Sequence;
      X, Y   : Index_Type)
      return Boolean
   --  S1 and S2 coincide except on X and Y.

   with Ghost,
     Pre  => M.Last (S1) = M.Last (S2),
     Post => M_Elements_Equal_Except'Result =
       (for all I in Index_Type'First .. M.Last (S1) =>
          (if I /= X and I /= Y then
                 Element (S1, I) = Element (S2, I)));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Equal_Except);

   procedure Swap
     (Self        : in out Base_Vector'Class;
      Left, Right : Index_Type)
   --  Efficiently swap the elements at the two positions.

   with
     Global => null,
     Pre    => Left <= Last (Self) and then Right <= Last (Self),
     Post   => Length (Self) = Length (Self)'Old
     and then Element (Model (Self), Left) = Element (Model (Self)'Old, Right)
     and then Element (Model (Self), Right) = Element (Model (Self)'Old, Left)

     --  Valid cursors are preserved

     and then Valid_Cursors (Self) = Valid_Cursors (Self)'Old

     --  Elements that have not been swapped are preserved.

     and then M_Elements_Equal_Except
       (S1 => Model (Self),
        S2 => Model (Self)'Old,
        X  => Left,
        Y  => Right);

   --  Actual implementation for the subprograms renamed in Generics. See the
   --  descriptions in Generics.

   function First_Primitive (Self : Base_Vector) return Cursor
   is (First (Self)) with Inline;
   function Element_Primitive
     (Self : Base_Vector; Position : Cursor) return Constant_Returned_Type
   is (Element (Self, Position))
   with Inline,
     Pre'Class => Has_Element (Self, Position),
     Post      => Element_Primitive'Result =
       Storage.Elements.To_Constant_Returned
       (Storage.Elements.To_Stored
          (Element (Model (Self), To_Index (Position))));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);
   function Has_Element_Primitive
     (Self : Base_Vector; Position : Cursor) return Boolean
   is (Has_Element (Self, Position))
   with Inline,
     Post =>
       Has_Element_Primitive'Result = V_Mem (Valid_Cursors (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);
   function Next_Primitive
     (Self : Base_Vector; Position : Cursor) return Cursor
   is (Next (Self, Position)) with Inline,
   Pre'Class => Has_Element (Self, Position);
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out Base_Vector);
   procedure Finalize (Self : in out Base_Vector);
   --  In case the list is a controlled type, but irrelevant when Self
   --  is not controlled.

   type Cursor is record
      Index   : Count_Type;
   end record;

   No_Element : constant Cursor :=
     (Index => Conts.Vectors.Storage.Min_Index - 1);

   type Base_Vector is new Storage.Container with record
      Last  : Count_Type := No_Element.Index;
      --  Last assigned element
   end record;

   function Last (Self : Base_Vector'Class) return Extended_Index
   is (To_Index (Self.Last));
   function To_Index (Position : Cursor) return Index_Type
   is (To_Index (Position.Index));

   function To_Index (Position : Count_Type) return Extended_Index
   is (Index_Type'Val
       (Position - Conts.Vectors.Storage.Min_Index
        + Count_Type'Base (Index_Type'Pos (Index_Type'First))));

   function To_Count (Idx : Index_Type) return Count_Type
   is (Count_Type
       (Conts.Vectors.Storage.Min_Index
        + Count_Type'Base (Index_Type'Pos (Idx))
        - Count_Type'Base (Index_Type'Pos (Index_Type'First))));

   ------------------
   -- Formal Model --
   ------------------

   package V is new Conts.Functional.Sets
     (Element_Type => Cursor);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   type V_Set is record
      Content : V.Set;
   end record;

   type V_Private_Cursor is new V.Private_Key;

   function V_Iter_First (S : V_Set) return V_Private_Cursor is
     (V_Private_Cursor (V.Iter_First (S.Content)));
   function V_Iter_Next (S : V_Set; C : V_Private_Cursor)
                         return V_Private_Cursor is
     (V_Private_Cursor (V.Iter_Next (S.Content, V.Private_Key (C))));
   function V_Iter_Has_Element (S : V_Set; C : V_Private_Cursor)
                                return Boolean is
     (V.Iter_Has_Element (S.Content, V.Private_Key (C)));
   function V_Iter_Element (S : V_Set; C : V_Private_Cursor) return Cursor is
     (V.Iter_Element (S.Content, V.Private_Key (C)));
   function V_Mem (S : V_Set; C : Cursor) return Boolean is
     (V.Mem (S.Content, C));

   function M_Elements_Consts
     (S   : M.Sequence;
      Fst : Index_Type;
      Lst : Extended_Index;
      E   : Storage.Elements.Element_Type)
      return Boolean
   is
     (for all I in Fst .. Lst => Element (S, I) = E);

   function M_Elements_Equal
     (S1, S2 : M.Sequence;
      Fst    : Index_Type;
      Lst    : Extended_Index)
      return Boolean
   is
     (for all I in Fst .. Lst => Element (S1, I) = Element (S2, I));

   function M_Elements_Equal_Except
     (S1, S2 : M.Sequence;
      X, Y   : Index_Type)
      return Boolean
   is
     (for all I in Index_Type'First .. M.Last (S1) =>
          (if I /= X and I /= Y then
                Element (S1, I) = Element (S2, I)));

   function M_Elements_Shifted
     (S1, S2   : M.Sequence;
      Fst, Lst : Index_Type)
      return Boolean
   is
     (for all I in Fst .. Lst => Element (S1, I) =
          Element (S2, Index_Type'Succ (I)));
end Conts.Vectors.Impl;
