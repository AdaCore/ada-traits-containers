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

pragma Ada_2012;
with Conts.Lists.Storage;
with Functional_Sequences;
with Functional_Maps;

generic
   with package Storage is new Conts.Lists.Storage.Traits (<>);
   --  Describes how the nodes of the list are stored.

package Conts.Lists.Impl with SPARK_Mode is

   subtype Element_Type is Storage.Elements.Element_Type;
   subtype Returned_Type is Storage.Elements.Returned_Type;
   subtype Stored_Type is Storage.Elements.Stored_Type;
   subtype Constant_Returned_Type is Storage.Elements.Constant_Returned_Type;

   use type Storage.Elements.Element_Type;
   use type Storage.Elements.Constant_Returned_Type;

   type Base_List is new Storage.Container with private with
     Default_Initial_Condition => Length (Base_List) = 0;
   --  We do not define the Iterable aspect here: this is not allowed,
   --  since the parent type is a generic formal parameter. Instead, we
   --  have to define it in the instantiations of Generic_List.

   type Cursor is private;
   No_Element : constant Cursor;

   function Capacity (Self : Base_List'Class) return Count_Type
   is (Storage.Capacity (Self))
   with Inline => True, Global => null;

   function Length (Self : Base_List'Class) return Count_Type
   with --  Inline => True, ??? bug with Default_Initial_Condition
     Global => null,
     Post   => Length'Result <= Capacity (Self);
   --  The length of a list is always smaller than its capacity;

   ------------------
   -- Formal Model --
   ------------------

   pragma Unevaluated_Use_Of_Old (Allow);

   subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

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

   package M is new Functional_Sequences
     (Index_Type   => Positive_Count_Type,
      Element_Type => Element_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   use type M.Sequence;

   function Model (Self : Base_List'Class) return M.Sequence with
     Ghost,
     Post => M.Length (Model'Result) = Length (Self);
   --  The highlevel model of a list is a sequence of elements. Cursors are
   --  not represented in this model.

   function Positions (Self : Base_List'Class) return P_Map with Ghost,
   --  The Positions map is used to model cursors. It only contains valid
   --  cursors and map them to their position in the container.

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

   procedure Lift_Abstraction_Level (Self : Base_List'Class) with
     Global => null,
     Post   =>
       (for all Elt of Model (Self) =>
          (for some I of Positions (Self) =>
               M.Get (Model (Self), P_Get (Positions (Self), I)) = Elt));
   --  Lift_Abstraction_Level is a ghost procedure that does nothing but
   --  assume that we can access to the same elements by iterating over
   --  positions or cursors.
   --  This information is not generally useful except when switching from
   --  a lowlevel, cursor aware view of a container, to a highlevel position
   --  based view.

   function Element (S : M.Sequence; I : Count_Type) return Element_Type
                     renames M.Get;

   -----------------
   -- Subprograms --
   -----------------

   --  Actual implementation for the subprograms renamed in generics. See the
   --  descriptions in generics.

   function Has_Element
     (Self : Base_List'Class; Position : Cursor) return Boolean
   with
     Inline,
     Global => null,
     Post   => Has_Element'Result = P_Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function First (Self : Base_List'Class) return Cursor with
     Inline,
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => First'Result = No_Element,
        others            => Has_Element (Self, First'Result)
        and  P_Get (Positions (Self), First'Result) = 1);

   function Next
     (Self : Base_List'Class; Position : Cursor) return Cursor
   with Inline,
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P_Get (Positions (Self), Position) = Length (Self) =>
              Next'Result = No_Element,
        others                                             =>
          Has_Element (Self, Next'Result)
        and P_Get (Positions (Self), Next'Result) =
          P_Get (Positions (Self), Position) + 1);

   function Previous
     (Self : Base_List'Class; Position : Cursor) return Cursor
   with Inline,
     Global         => null,
     Pre            => Has_Element (Self, Position),
     Contract_Cases =>
       (P_Get (Positions (Self), Position) = 1 =>
              Previous'Result = No_Element,
        others                                             =>
          Has_Element (Self, Previous'Result)
        and P_Get (Positions (Self), Previous'Result) =
          P_Get (Positions (Self), Position) - 1);

   function Last (Self : Base_List'Class) return Cursor with
     Global         => null,
     Contract_Cases =>
       (Length (Self) = 0 => Last'Result = No_Element,
        others            => Has_Element (Self, Last'Result)
        and P_Get (Positions (Self), Last'Result) = Length (Self));

   function Element
     (Self : Base_List'Class; Position : Cursor)
         return Constant_Returned_Type
   with Inline,
     Global => null,
     Pre    => Has_Element (Self, Position),
     Post   =>

     --  Query Positions to get the position of Position in Self and use it
     --  to fetch the corresponding element in Model.

     Element'Result = Storage.Elements.To_Constant_Returned
       (Storage.Elements.To_Stored (Element (Model (Self),
        P_Get (Positions (Self), Position))));
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Element);

   procedure Clear (Self : in out Base_List'Class)
   with
     Post => Capacity (Self) = Capacity (Self)'Old
     and then Length (Self) = 0;

   procedure Assign
     (Self : in out Base_List'Class; Source : Base_List'Class)
   with
     Global => null,
     Post   => Length (Self) = Length (Source)
     and Capacity (Self) = Capacity (Self)'Old
     and Model (Self) = Model (Source);

   procedure Append (Self : in out Base_List'Class; Element : Element_Type)
   with
     Global         => null,
     Pre            => Length (Self) < Capacity (Self),
     Post           => Capacity (Self) = Capacity (Self)'Old
     and Length (Self) = Length (Self)'Old + 1

     --  Positions contains a new mapping from the last cursor of Self to
     --  Length.

     and (not P_Mem (Positions (Self)'Old, Last (Self))
          and P_Mem (Positions (Self), Last (Self))
          and P_Get (Positions (Self), Last (Self)) = Length (Self)
          and (for all Position of Positions (Self)'Old =>
                 P_Mem (Positions (Self), Position)
               and P_Get (Positions (Self), Position) =
                 P_Get (Positions (Self)'Old, Position))
          and (for all Position of Positions (Self) =>
                 Position = Last (Self)
               or P_Mem (Positions (Self)'Old, Position)))

     --  Model contains a new element Element at the end.

     and M.Is_Add (Model (Self)'Old, Element, Model (Self));

   function Find (Self : Base_List'Class; Element : Element_Type) return Cursor
   with
     Contract_Cases =>

     --  Either Element is not in the model and the result is No_Element

     ((for all E of Model (Self) => E /= Element) => Find'Result = No_Element,

     --  or the result is a valid cursor, Element is stored at its position in
     --  Self and there is no previous occurrence of Element in L.

      others                               =>
        Has_Element (Self, Find'Result)
      and Impl.Element (Model (Self),
        P_Get (Positions (Self), Find'Result)) = Element
      and (for all I in 1 .. P_Get (Positions (Self), Find'Result) - 1 =>
            Impl.Element (Model (Self), I) /= Element));

   procedure Insert
     (Self : in out Base_List'Class; Position : Cursor; Element : Element_Type)
     --  Insert Element before the valid cursor Position in L.

   with
     Pre  => Length (Self) < Capacity (Self)
     and then Has_Element (Self, Position),
     Post => Length (Self) = Length (Self)'Old + 1
     and Capacity (Self) = Capacity (Self)'Old

     --  Every cursor previously valid in Self is still valid.

     and (for all I of Positions (Self)'Old =>
              Has_Element (Self, I)

          --  If it was located before Position in Self its position is
          --  preserved.

          and (if P_Get (Positions (Self)'Old, I) <
                   P_Get (Positions (Self)'Old, Position)
                 then P_Get (Positions (Self), I) =
                   P_Get (Positions (Self)'Old, I)

               --  Otherwise it is shifted by 1.

                 else P_Get (Positions (Self), I) =
                   P_Get (Positions (Self)'Old, I) + 1))

     --  Every cursor valid in Self was previously valid except for the newly
     --  inserted cursor.

     and (for all I of Positions (Self) =>
              P_Mem (Positions (Self)'Old, I) or
            P_Get (Positions (Self), I) =
              P_Get (Positions (Self)'Old, Position))

     --  The elements of Self located before Position are preserved.

     and (for all I in 1 .. P_Get (Positions (Self)'Old, Position) - 1 =>
            Impl.Element (Model (Self), I) =
              Impl.Element (Model (Self)'Old, I))

     --  Other elements are shifted by 1.

     and (for all I in
            P_Get (Positions (Self)'Old, Position) + 1 .. Length (Self) =>
              Impl.Element (Model (Self), I) =
            Impl. Element (Model (Self)'Old, I - 1))

     --  Element is stored at the previous position of Position in L.

     and Impl.Element
       (Model (Self), P_Get (Positions (Self)'Old, Position)) = Element;

   procedure Replace_Element
     (Self : in out Base_List'Class; Position : Cursor; Element : Element_Type)
   with
     Pre  => Has_Element (Self, Position),
     Post => Capacity (Self) = Capacity (Self)'Old
     and Length (Self) = Length (Self)'Old

     --  Cursors are preserved.

     and Positions (Self)'Old = Positions (Self)

     --  The element at the position of Position in Self is replaced by E.

     and M.Is_Replace (Model (Self)'Old,
                       P_Get (Positions (Self), Position),
                       Element,
                       Model (Self));

   function First_Primitive (Self : Base_List) return Cursor
   is (First (Self)) with Inline;
   function Element_Primitive
     (Self : Base_List; Position : Cursor) return Constant_Returned_Type
   is (Element (Self, Position))
   with Inline,
     Pre'Class => Has_Element (Self, Position),
     Post =>
       Element_Primitive'Result =
     Storage.Elements.To_Constant_Returned
       (Storage.Elements.To_Stored
          (Element (Model (Self), P_Get (Positions (Self), Position))));
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Element_Primitive);
   function Has_Element_Primitive
     (Self : Base_List; Position : Cursor) return Boolean
   is (Has_Element (Self, Position))
   with Inline,
     Post =>
         Has_Element_Primitive'Result = P_Mem (Positions (Self), Position);
   pragma Annotate
     (GNATprove, Inline_For_Proof, Entity => Has_Element_Primitive);
   function Next_Primitive
     (Self : Base_List; Position : Cursor) return Cursor
   is (Next (Self, Position)) with Inline,
   Pre'Class => Has_Element (Self, Position);
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out Base_List);
   procedure Finalize (Self : in out Base_List);
   --  In case the list is a controlled type, but irrelevant when the list
   --  is not controlled.

   type Base_List is new Storage.Container with record
      Head, Tail : Storage.Node_Access := Storage.Null_Access;
      Size       : Count_Type := 0;
   end record;

   type Cursor is record
      Current : Storage.Node_Access;
   end record;
   No_Element : constant Cursor := (Current => Storage.Null_Access);

   package P is new Functional_Maps
     (Element_Type => Positive_Count_Type,
      Key_Type     => Cursor);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   type P_Map is record
      Content : P.Map;
   end record;

   type P_Private_Cursor is new P.Private_Key;

end Conts.Lists.Impl;
