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

--  Implementation details for the vector container.
--  This package takes the same formal arguments as Conts.Vectors.Generics
--  and provides the internal implementation as well as annotations for
--  all the primitive operations.

pragma Ada_2012;
with Conts.Vectors.Storage;
with Conts.Functional.Sequences;

generic
   type Index_Type is (<>);
   with package Storage is new Conts.Vectors.Storage.Traits (<>);
package Conts.Vectors.Impl with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Extended_Index is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;
   No_Index : constant Extended_Index := Extended_Index'First;
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

   subtype Cursor is Extended_Index;
   No_Element : constant Cursor := No_Index;

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

   function Max_Capacity (Self : Base_Vector'Class) return Count_Type
     is (Count_Type'Min (Last_Count, Storage.Max_Capacity (Self)));
   --  Maximal capacity of Self. It cannot be modified. It is the maximal
   --  number of elements a vector may contain.

   function Length (Self : Base_Vector'Class) return Count_Type
   --  The length of a vector is always smaller than Max_Capacity.
     with
       Inline,
       Global => null,
       Post   => Length'Result <= Max_Capacity (Self);

   function Last (Self : Base_Vector'Class) return Extended_Index
   --  On an empty vector, Last returns Extended_Index'First.
     with
       Inline,
       Global => null,
       Post   => Last'Result =
         Index_Type'Val ((Index_Type'Pos (Index_Type'First) - 1)
                         + Length (Self));

   function To_Index (Position : Count_Type) return Extended_Index
   --  Converts from index into the actual array back to the index type
     with
       Global => null,
       Pre    => Position in 0 .. Last_Count,
       Post   => To_Index'Result = Index_Type'Val
         (Position - Conts.Vectors.Storage.Min_Index
          + Count_Type'Base (Index_Type'Pos (Index_Type'First)));

   function To_Count (Idx : Index_Type) return Count_Type
   --  Converts from an index type to an index into the actual underlying
   --  array.
     with
       Inline,
       Global => null,
       Pre    => Idx in Index_Type'First .. To_Index (Last_Count),
       Post   => To_Count'Result = Count_Type
         (Conts.Vectors.Storage.Min_Index
          + Count_Type'Base (Index_Type'Pos (Idx))
          - Count_Type'Base (Index_Type'Pos (Index_Type'First)));

   ------------------
   -- Formal Model --
   ------------------

   pragma Unevaluated_Use_Of_Old (Allow);

   package M is new Conts.Functional.Sequences
     (Index_Type   => Index_Type,
      Element_Type => Element_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   function Model (Self : Base_Vector'Class) return M.Sequence
     with
       Ghost,
       Global => null,
       Post   => M.Length (Model'Result) = Length (Self);

   use type M.Sequence;
   function Element
     (S : M.Sequence; I : Index_Type) return Element_Type
     renames M.Get;
   --  ??? Do we need this subprogram, could we use M.Get everywhere instead ?

   -----------------
   -- Subprograms --
   -----------------

   function Element
     (Self : Base_Vector'Class; Position : Index_Type)
     return Constant_Returned_Type
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global => null,
       Pre    => Position <= Last (Self),
       Post   => Storage.Elements.To_Element (Element'Result) =
          Element (Model (Self), Position);

   function Reference
     (Self : Base_Vector'Class; Position : Index_Type)
     return Returned_Type
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global => null,
       Pre    => Position <= Last (Self);

   function Last_Element
     (Self : Base_Vector'Class) return Constant_Returned_Type
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Pre    => Length (Self) > 0,
       Post   => Last_Element'Result = Element (Self, Last (Self));

   function First (Self : Base_Vector'Class) return Cursor
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global         => null,
       Contract_Cases =>
         (Length (Self) = 0 => First'Result = No_Element,
          others            => First'Result = Index_Type'First
          and then Has_Element (Self, First'Result));

   function Has_Element
     (Self : Base_Vector'Class; Position : Cursor) return Boolean
     is (Position >= Index_Type'First and then Position <= Self.Last)
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global => null,
       Post   => Has_Element'Result =
                           (Position in Index_Type'First .. Self.Last);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Next
     (Self : Base_Vector'Class; Position : Cursor) return Cursor
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global         => null,
       Pre            => Has_Element (Self, Position),
       Contract_Cases   =>
         (Position < Last (Self) => Next'Result = Index_Type'Succ (Position)
             and then Has_Element (Self, Next'Result),
          others => Next'Result = No_Element);

   procedure Next (Self : Base_Vector'Class; Position : in out Cursor)
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global => null,
       Pre    => Has_Element (Self, Position),
       Contract_Cases =>
         (Position < Last (Self) => Position = Index_Type'Succ (Position'Old)
             and then Has_Element (Self, Position),
          others => Position = No_Element);

   function Previous
     (Self : Base_Vector'Class; Position : Cursor) return Cursor
   --  See documentation in conts-vectors-generics.ads
     with
       Inline,
       Global         => null,
       Pre            => Has_Element (Self, Position),
       Contract_Cases =>
         (Position > Index_Type'First =>
             Previous'Result = Index_Type'Pred (Position)
             and then Has_Element (Self, Previous'Result),
          others => Previous'Result = No_Element);

   procedure Reserve_Capacity
     (Self : in out Base_Vector'Class; Capacity : Count_Type)
   --  Make sure there is enough space for at least Count_Type elements in
   --  Self.
     with
       Global => null,
       Pre    => Capacity <= Max_Capacity (Self),
       Post   => Length (Self) = Length (Self)'Old
          and then Model (Self) = Model (Self)'Old;

   procedure Shrink_To_Fit (Self : in out Base_Vector'Class)
   --  Resize the vector to fit its number of elements.
   --  This has no effect on models.
     with
       Global => null,
       Post   => Length (Self) = Length (Self)'Old
          and then Model (Self) = Model (Self)'Old;

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
      Length  : Count_Type;
      Element : Storage.Elements.Element_Type)
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Pre    => Length <= Max_Capacity (Self),
       Post   =>
          Impl.Length (Self) = Length

          --  Elements of Self that were located before the index Length are
          --  preserved.
          and then M_Elements_Equal
            (S1  => Model (Self),
             S2  => Model (Self)'Old,
             Fst => Index_Type'First,
             Lst => Index_Type'Min
                (To_Index (Length),
                 Last (Self)'Old))

          --  If elements were appended to Self then they are equal to Element.
          and then
            (if To_Index (Length) > Last (Self)'Old then
                   M_Elements_Consts (S   => Model (Self),
                                      Fst => Index_Type'Succ (Last (Self)'Old),
                                      Lst => Last (Self),
                                      E   => Element));

   procedure Clear (Self : in out Base_Vector'Class)
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Post   => Length (Self) = 0;

   procedure Append
     (Self    : in out Base_Vector'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
     --  See documentation in conts-vectors-generics.ads
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

   procedure Insert
     (Self    : in out Base_Vector'Class;
      Before  : Extended_Index;
      Element : Element_Type;
      Count   : Count_Type := 1)
   --  See documentation in conts-vectors-generics.ads
     with
       Global  => null,
       Pre     => Length (Self) <= Max_Capacity (Self) - Count
          and then (Before = No_Element or else Has_Element (Self, Before)),
       Post    =>
          Length (Self) = Length (Self)'Old + Count

          --  Elements before Before have not been modified
          and M_Elements_Equal
            (S1   => Model (Self),
             S2   => Model (Self)'Old,
             Fst  => Index_Type'First,
             Lst  => Index_Type'Pred (Before))

         --  Then the new elements
         and M_Elements_Consts
            (Model (Self),
             Fst  => Before,
             Lst  => Index_Type'Val (Index_Type'Pos (Before) + Count - 1),
             E    => Element)

         --  Elements after are unchanged
         and M_Elements_Shifted
            (S1     => Model (Self)'Old,
             S2     => Model (Self),
             Fst    => Before,
             Lst    => Last (Self)'Old,
             Offset => Count);

   procedure Assign
     (Self : in out Base_Vector'Class; Source : Base_Vector'Class)
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Post   => Model (Self) = Model (Source);

   function Is_Empty (Self : Base_Vector'Class) return Boolean
     is (Length (Self) = 0)
     with Inline;
   --  See documentation in conts-vectors-generics.ads

   procedure Replace_Element
     (Self     : in out Base_Vector'Class;
      Index    : Index_Type;
      New_Item : Element_Type)
   --  Replace the element at the given position.
   --  Nothing is done if Index is not a valid index in the container.
     with
       Global => null,
       Pre    => Index <= Last (Self),
       Post   => Length (Self) = Length (Self)'Old
          and then
            (if Index <= Last (Self)
             then M.Is_Set (Model (Self)'Old, Index, New_Item, Model (Self))
               else Model (Self) = Model (Self)'Old);

   function M_Elements_Shifted
     (S1, S2   : M.Sequence;
      Fst, Lst : Index_Type;
      Offset   : Count_Type := 1)
      return Boolean
   --  The slice from Fst to Lst of S1 has been shifted by Offset in S2.
     with Ghost,
       Pre  => Lst <= M.Last (S1) and Lst < M.Last (S2),
       Post =>
         M_Elements_Shifted'Result =
         (for all I in Fst .. Lst => Element (S1, I) =
                Element (S2, Index_Type'Val (Index_Type'Pos (I) + Offset)));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Shifted);

   procedure Delete
     (Self  : in out Base_Vector'Class;
      Index : Index_Type;
      Count : Count_Type := 1)
   --  See documentation in conts-vectors-generics.ads
     with
       Global         => null,
       Pre            => Index <= Last (Self),
       Post           =>

          --  Elements located before Index are preserved.
          M_Elements_Equal
            (S1  => Model (Self),
             S2  => Model (Self)'Old,
             Fst => Index_Type'First,
             Lst => Index_Type'Pred (Index)),

       --  If there are less than Count elements after Index, they are all
       --  erased.
       Contract_Cases =>
       (Count - 1 >= Index_Type'Pos (Last (Self)) - Index_Type'Pos (Index) =>
              Length (Self) =
              Index_Type'Pos (Index) - Index_Type'Pos (Index_Type'First),

        --  Otherwise, Count elements are removed
        others                                                             =>
          Length (Self) = Length (Self)'Old - Count

        --  Elements located after Index are shifted.
        and M_Elements_Shifted
          (S1  => Model (Self),
           S2  => Model (Self)'Old,
           Fst => Index,
           Lst => Last (Self),
           Offset => Count));

   procedure Delete_Last (Self : in out Base_Vector'Class)
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Pre    => Length (Self) > 0,
       Post   => Length (Self) = Length (Self)'Old - 1
       and then M_Elements_Equal
         (S1  => Model (Self),
          S2  => Model (Self)'Old,
          Fst => Index_Type'First,
          Lst => Last (Self));

   function M_Elements_Equal_Except
     (S1, S2 : M.Sequence;
      X, Y   : Index_Type)
      return Boolean
   --  Whether S1 and S2 coincide except on X and Y.
     with
       Ghost,
       Pre  => M.Last (S1) = M.Last (S2),
       Post =>
          M_Elements_Equal_Except'Result =
          (for all I in Index_Type'First .. M.Last (S1) =>
             (if I /= X and I /= Y then
                    Element (S1, I) = Element (S2, I)));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Equal_Except);

   procedure Swap
     (Self        : in out Base_Vector'Class;
      Left, Right : Index_Type)
   --  See documentation in conts-vectors-generics.ads
     with
       Global => null,
       Pre    => Left <= Last (Self) and then Right <= Last (Self),
       Post   => Length (Self) = Length (Self)'Old
          and then Element (Model (Self), Left) =
             Element (Model (Self)'Old, Right)
          and then Element (Model (Self), Right) =
             Element (Model (Self)'Old, Left)

          --  Elements that have not been swapped are preserved.
          and then M_Elements_Equal_Except
            (S1 => Model (Self),
             S2 => Model (Self)'Old,
             X  => Left,
             Y  => Right);

   function First_Primitive (Self : Base_Vector) return Cursor
   --  See documentation in conts-vectors-generics.ads
     is (First (Self))
     with Inline;

   function Element_Primitive
     (Self : Base_Vector; Position : Cursor) return Constant_Returned_Type
   --  See documentation in conts-vectors-generics.ads
     is (Element (Self, Position))
     with
       Inline,
       Pre'Class => Has_Element (Self, Position),
       Post      => Storage.Elements.To_Element (Element_Primitive'Result) =
          Element (Model (Self), Position);

   function Has_Element_Primitive
     (Self : Base_Vector; Position : Cursor) return Boolean
   --  See documentation in conts-vectors-generics.ads
     is (Has_Element (Self, Position))
     with
       Inline,
   Post => Has_Element_Primitive'Result =
                  (Position in Index_Type'First .. Self.Last);
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element_Primitive);

   function Next_Primitive
     (Self : Base_Vector; Position : Cursor) return Cursor
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.
     is (Next (Self, Position))
     with
       Inline,
       Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out Base_Vector);
   procedure Finalize (Self : in out Base_Vector);
   --  In case the list is a controlled type, but irrelevant when Self
   --  is not controlled.

   No_Last : constant Count_Type := Conts.Vectors.Storage.Min_Index - 1;
   --  Indicates that the vector is empty, when its Last index is No_Last

   type Base_Vector is new Storage.Container with record
      Last  : Count_Type := No_Last;
      --  Last assigned element
   end record;

   function Last (Self : Base_Vector'Class) return Extended_Index
     is (To_Index (Self.Last));

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

   function M_Elements_Consts
     (S   : M.Sequence;
      Fst : Index_Type;
      Lst : Extended_Index;
      E   : Storage.Elements.Element_Type)
      return Boolean
     is (for all I in Fst .. Lst => Element (S, I) = E);

   function M_Elements_Equal
     (S1, S2 : M.Sequence;
      Fst    : Index_Type;
      Lst    : Extended_Index)
      return Boolean
     is (for all I in Fst .. Lst => Element (S1, I) = Element (S2, I));

   function M_Elements_Equal_Except
     (S1, S2 : M.Sequence;
      X, Y   : Index_Type)
      return Boolean
     is (for all I in Index_Type'First .. M.Last (S1) =>
          (if I /= X and I /= Y then Element (S1, I) = Element (S2, I)));

   function M_Elements_Shifted
     (S1, S2   : M.Sequence;
      Fst, Lst : Index_Type;
      Offset   : Count_Type := 1)
      return Boolean
     is (for all I in Fst .. Lst => Element (S1, I) =
          Element (S2, Index_Type'Val (Index_Type'Pos (I) + Offset)));

end Conts.Vectors.Impl;
