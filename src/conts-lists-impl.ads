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

--  Implementation details for the list container.
--  This package takes the same formal arguments as Conts.Lists.Generics
--  and provides the internal implementation as well as annotations for
--  all the primitive operations.

pragma Ada_2012;
with Conts.Lists.Storage;
with Conts.Functional.Sequences;
with Conts.Functional.Maps;

generic
   with package Storage is new Conts.Lists.Storage.Traits (<>);
package Conts.Lists.Impl with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Element_Type is Storage.Elements.Element_Type;
   subtype Returned_Type is Storage.Elements.Returned_Type;
   subtype Stored_Type is Storage.Elements.Stored_Type;
   subtype Constant_Returned_Type is Storage.Elements.Constant_Returned_Type;

   use type Storage.Elements.Element_Type;
   use type Storage.Elements.Constant_Returned_Type;

   type Base_List is new Storage.Container with private
     with Default_Initial_Condition => Length (Base_List) = 0;
   --  We do not define the Iterable aspect here: this is not allowed,
   --  since the parent type is a generic formal parameter. Instead, we
   --  have to define it in the instantiations of Generic_List.

   type Cursor is private;
   No_Element : constant Cursor;

   function Capacity (Self : Base_List'Class) return Count_Type
   --  The capacity of a list cannot be modified, it is the maximal number of
   --  elements that the list may contain.
     is (Storage.Capacity (Self))
     with
       Inline => True,
       Global => null;

   function Length (Self : Base_List'Class) return Count_Type
     with
       --  Inline => True, ??? bug with Default_Initial_Condition
       Global => null,
       Post   =>
          --  The length of a list is always smaller than its capacity
          Length'Result <= Capacity (Self);

   function Is_Empty (Self : Base_List'Class) return Boolean
      is (Length (Self) = 0)
      with Inline;
   --  See documentation in conts-lists-generics.ads

   ------------------
   -- Formal Model --
   ------------------

   pragma Unevaluated_Use_Of_Old (Allow);

   type P_Map is private with Ghost,
     Iterable => (First => P_Iter_First,
                  Has_Element => P_Iter_Has_Element,
                  Next => P_Iter_Next,
                  Element => P_Iter_Element);

   type P_Private_Cursor is private with Ghost;
   function P_Iter_First (M : P_Map) return P_Private_Cursor
     with Ghost;
   function P_Iter_Next
     (M : P_Map; C : P_Private_Cursor) return P_Private_Cursor
     with Ghost;
   function P_Iter_Has_Element
     (M : P_Map; C : P_Private_Cursor) return Boolean
     with Ghost;
   function P_Iter_Element (M : P_Map; C : P_Private_Cursor) return Cursor
     with Ghost;

   function P_Mem (M : P_Map; C : Cursor) return Boolean with Ghost;
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", P_Mem);

   function P_Get (M : P_Map; C : Cursor) return Positive_Count_Type
     with
       Ghost,
       Pre => P_Mem (M, C);

   package M is new Conts.Functional.Sequences
     (Index_Type   => Positive_Count_Type,
      Element_Type => Element_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   use type M.Sequence;

   function Model (Self : Base_List'Class) return M.Sequence
   --  The highlevel model of a list is a sequence of elements. Cursors are
   --  not represented in this model.
     with
       Ghost,
       Post => M.Length (Model'Result) = Length (Self);

   function Positions (Self : Base_List'Class) return P_Map
   --  The Positions map is used to model cursors. It only contains valid
   --  cursors and map them to their position in the container.
     with
       Ghost,
       Post => not P_Mem (Positions'Result, No_Element)
          --  Positions of cursors are smaller than the container's length.
          and then
            (for all I of Positions'Result =>
               P_Get (Positions'Result, I) in 1 .. Length (Self)

             --  No two cursors have the same position. Note that we do not
             --  state that there is a cursor in the map for each position,
             --  as it is rarely needed.
             and then
               (for all J of Positions'Result =>
                  (if P_Get (Positions'Result, I) = P_Get (Positions'Result, J)
                   then I = J)));

   procedure Lift_Abstraction_Level (Self : Base_List'Class)
   --  Lift_Abstraction_Level is a ghost procedure that does nothing but
   --  assume that we can access to the same elements by iterating over
   --  positions or cursors.
   --  This information is not generally useful except when switching from
   --  a lowlevel, cursor aware view of a container, to a highlevel position
   --  based view.
     with
       Global => null,
       Post   =>
         (for all Elt of Model (Self) =>
            (for some I of Positions (Self) =>
                 M.Get (Model (Self), P_Get (Positions (Self), I)) = Elt));

   function Element (S : M.Sequence; I : Count_Type) return Element_Type
     renames M.Get;
   --  To improve readability of contracts, we rename the function used to
   --  access an element in the model to Element.

   -----------------
   -- Subprograms --
   -----------------

   function Has_Element
     (Self : Base_List'Class; Position : Cursor) return Boolean
   --  See documentation in conts-lists-generics.ads
     with
       Inline,
       Global => null,
       Pre    => Position = No_Element or P_Mem (Positions (Self), Position),
       Post   => Has_Element'Result = P_Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function First (Self : Base_List'Class) return Cursor
   --  See documentation in conts-lists-generics.ads
     with
       Inline,
       Global         => null,
       Contract_Cases =>
         (Length (Self) = 0 => First'Result = No_Element,
          others            => Has_Element (Self, First'Result)
          and  P_Get (Positions (Self), First'Result) = 1);

   function Next
     (Self : Base_List'Class; Position : Cursor) return Cursor
   --  See documentation in conts-lists-generics.ads
     with
       Inline,
       Global         => null,
       Pre            => P_Mem (Positions (Self), Position),
       Contract_Cases =>
         (P_Get (Positions (Self), Position) = Length (Self) =>
                Next'Result = No_Element,
          others => Has_Element (Self, Next'Result)
             and then P_Get (Positions (Self), Next'Result) =
                 P_Get (Positions (Self), Position) + 1);

   function Previous
     (Self : Base_List'Class; Position : Cursor) return Cursor
   --  See documentation in conts-lists-generics.ads
     with
       Inline,
       Global         => null,
       Pre            => P_Mem (Positions (Self), Position),
       Contract_Cases =>
         (P_Get (Positions (Self), Position) = 1 =>
                Previous'Result = No_Element,
          others =>
            Has_Element (Self, Previous'Result)
            and then P_Get (Positions (Self), Previous'Result) =
               P_Get (Positions (Self), Position) - 1);

   procedure Next (Self : Base_List'Class; Position : in out Cursor)
   --  See documentation in conts-lists-generics.ads
     with
       Inline,
       Global => null,
       Pre    => P_Mem (Positions (Self), Position),
       Contract_Cases =>
         (Impl.P_Get (Impl.Positions (Self), Position) = Length (Self) =>
                Position = No_Element,
          others =>
            Has_Element (Self, Position)
            and then Impl.P_Get (Impl.Positions (Self), Position) =
              Impl.P_Get (Impl.Positions (Self), Position'Old) + 1);

   function Last (Self : Base_List'Class) return Cursor
   --  See documentation in conts-lists-generics.ads
     with
       Global         => null,
       Contract_Cases =>
         (Length (Self) = 0 => Last'Result = No_Element,
          others            => Has_Element (Self, Last'Result)
            and P_Get (Positions (Self), Last'Result) = Length (Self));

   function Element
     (Self : Base_List'Class; Position : Cursor)
     return Constant_Returned_Type
     with
       Inline,
       Global => null,
       Pre    => P_Mem (Positions (Self), Position),
       Post   =>
          --  Query Positions to get the position of Position in Self and use
          --  it to fetch the corresponding element in Model.
          Storage.Elements.To_Element (Element'Result) =
          Element (Model (Self), P_Get (Positions (Self), Position));

   procedure Clear (Self : in out Base_List'Class)
   --  See documentation in conts-lists-generics.ads
     with
       Post => Capacity (Self) = Capacity (Self)'Old
          and then Length (Self) = 0;

   procedure Assign
     (Self : in out Base_List'Class; Source : Base_List'Class)
   --  See documentation in conts-lists-generics.ads
     with
       Global => null,
       Post   => Length (Self) = Length (Source)
          and Capacity (Self) = Capacity (Self)'Old
          and Model (Self) = Model (Source);

   function P_Is_Add
     (P1, P2 : P_Map;
      K      : Cursor;
      E      : Positive_Count_Type) return Boolean
     with
       Ghost,
       Post =>
          --  P2 is P1 with an additional mapping from K to E.
          P_Is_Add'Result =
          (not P_Mem (P1, K) and P_Mem (P2, K) and P_Get (P2, K) = E
           and (for all I of P1 => P_Mem (P2, I)
                and P_Get (P2, I) = P_Get (P1, I))
           and (for all I of P2 => I = K or P_Mem (P1, I)));

   function M_Elements_Cst
     (S        : M.Sequence;
      Fst, Lst : Count_Type;
      E        : Element_Type)
     return Boolean
   --  Every element of the slice from Fst to Lst in S is E.
     with
       Ghost,
       Pre  => Lst <= M.Length (S),
       Post => M_Elements_Cst'Result =
         (for all I in Fst .. Lst => Element (S, I) = E);
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Cst);

   function M_Elements_Equal
     (S1, S2   : M.Sequence;
      Fst, Lst : Count_Type;
      Offset   : Count_Type'Base := 0)
     return Boolean
   --  The slice from Fst to Lst in S1 has been shifted by Offset in S2.
     with
       Ghost,
       Pre  => Lst <= M.Length (S1)
          and Offset in 1 - Fst .. M.Length (S2) - Lst,
       Post => M_Elements_Equal'Result =
         (for all I in Fst .. Lst =>
            Element (S1, I) = Element (S2, I + Offset));
   pragma Annotate (GNATprove, Inline_For_Proof, M_Elements_Equal);

   procedure Append
     (Self    : in out Base_List'Class;
      Element : Element_Type;
      Count   : Count_Type := 1)
   --  See documentation in conts-lists-generics.ads
     with
       Global         => null,
       Pre            => Length (Self) <= Capacity (Self) - Count,
       Post           => Capacity (Self) = Capacity (Self)'Old
          and Length (Self) = Length (Self)'Old + Count,
       Contract_Cases =>
       (Count = 1 =>
          --  Positions contains a new mapping from the last cursor of Self to
          --  Length.
          P_Is_Add
            (Positions (Self)'Old, Positions (Self),
             Last (Self), Length (Self))

          --  Model contains Count new elements Element at the end.
          and M.Is_Add (Model (Self)'Old, Element, Model (Self)),
        others    =>

          --  Elements already in Self are preserved
          M_Elements_Equal (S1     => Model (Self)'Old,
                            S2     => Model (Self),
                            Fst    => 1,
                            Lst    => Length (Self)'Old)

          --  Other elements are set to E
          and M_Elements_Cst (S   => Model (Self),
                              Fst => Length (Self)'Old + 1,
                              Lst => Length (Self),
                              E   => Element));

   function M_Not_Until
     (S : M.Sequence; Lst : Positive_Count_Type; E : Element_Type)
     return Boolean
     with
       Ghost,
       Pre  => Lst <= M.Length (S),
       Post =>
          --  E is not in S until Lst.
          M_Not_Until'Result =
            (for all I in 1 .. Lst => Element (S, I) /= E);

   function P_Insert_Position
     (P1, P2 : P_Map; Cut : Positive_Count_Type) return Boolean
   --  P2 is the resul of inserting a cursor at position Cut in P1.
     with
       Ghost,
       Pre  => (for all I of P1 => P_Get (P1, I) < Count_Type'Last),
       Post => P_Insert_Position'Result =
          --  Every cursor valid in P1 is valid in P2.
          ((for all I of P1 => P_Mem (P2, I)

            --  If it was located before Cut its position is preserved.
            and (if P_Get (P1, I) < Cut then P_Get (P2, I) = P_Get (P1, I)
                 --  Otherwise it is shifted by 1.
                 else P_Get (P2, I) = P_Get (P1, I) + 1))

           --  Every cursor valid in P2 is valid P1 except the one at position
           --  Cut.
          and (for all I of P2 => P_Mem (P1, I) or P_Get (P2, I) = Cut));

   procedure Delete
     (Self     : in out Base_List'Class;
      Position : in out Cursor;
      Count    : Count_Type := 1)
   --  See documentation in conts-lists-generics.ads
     with
       Global       => null,
       Pre          => P_Mem (Positions (Self), Position),
       Post         =>
          --  We removed at least one element
          Length (Self) <= Length (Self)'Old - 1

          --  and no more than Count
          and Length (Self) >= Length (Self)'Old - Count

          --  The elements of Self located before Position are preserved.
          and M_Elements_Equal
            (S1  => Model (Self),
             S2  => Model (Self)'Old,
             Fst => 1,
             Lst => P_Get (Positions (Self)'Old, Position'Old) - 1),
     Contract_Cases =>

     --  If there are less than Count Elements after Position
     (Length (Self) - Count <= P_Get (Positions (Self), Position) =>

        --  Self is cut at Position
        Length (Self) = P_Get (Positions (Self)'Old, Position'Old) - 1

      --  Position is No_Element
      and Position = No_Element,

      --  Otherwise, Count elements have been erased after Position
      others                                                      =>
        Length (Self) = Length (Self)'Old - Count

      and P_Mem (Positions (Self), Position)

      --  The elements located after Position + Count are preserved
      and M_Elements_Equal
        (S1     => Model (Self),
         S2     => Model (Self)'Old,
         Fst    => P_Get (Positions (Self), Position),
         Lst    => Length (Self),
         Offset => Count)

      --  Position is set to the next element
      and P_Get (Positions (Self)'Old, Position'Old) =
        P_Get (Positions (Self), Position));

   procedure Insert
     (Self    : in out Base_List'Class;
      Before  : Cursor;
      Element : Element_Type;
      Count   : Count_Type := 1)
   --  See documentation in conts-lists-generics.ads
     with
       Global         => null,
       Pre            => Length (Self) <= Capacity (Self) - Count
          and then (Before = No_Element
                    or else P_Mem (Positions (Self), Before)),
       Post           => Length (Self) = Length (Self)'Old + Count
          and Capacity (Self) = Capacity (Self)'Old,
       Contract_Cases =>
       (Before = No_Element =>

          --  The elements of Self are preserved.
          M_Elements_Equal
             (S1  => Model (Self),
              S2  => Model (Self)'Old,
              Fst => 1,
              Lst => Length (Self)'Old)

        and
          (if Count = 1
           then

               --  A new cursor has been inserted at the end of Self
               P_Insert_Position
                 (Positions (Self)'Old, Positions (Self),
                  Cut => Length (Self)'Old + 1)

               --  Element is stored at the end of Self.
             and Impl.Element
               (Model (Self), Length (Self)'Old + 1) = Element
           else
             (for all I in Length (Self)'Old + 1 .. Length (Self)
              => Impl.Element (Model (Self), I) = Element)),
        others              =>

          --  The elements of Self located before Before are preserved.
          M_Elements_Equal
             (S1  => Model (Self),
              S2  => Model (Self)'Old,
              Fst => 1,
              Lst => P_Get (Positions (Self)'Old, Before) - 1)

          --  Other elements are shifted by Count.
          and M_Elements_Equal
            (S1     => Model (Self)'Old,
             S2     => Model (Self),
             Fst    => P_Get (Positions (Self)'Old, Before),
             Lst    => Length (Self)'Old,
             Offset => Count)
          and
          (if Count = 1
           then

               --  A new cursor has been inserted at position Before in Self
               P_Insert_Position
                 (Positions (Self)'Old, Positions (Self),
                  Cut => P_Get (Positions (Self)'Old, Before))

               --  Element is stored at the previous position of Before in L.
             and Impl.Element
               (Model (Self), P_Get (Positions (Self)'Old, Before)) = Element
           else
             (for all I in P_Get (Positions (Self)'Old, Before) ..
                P_Get (Positions (Self)'Old, Before) + Count - 1
              => Impl.Element (Model (Self), I) = Element)));

   procedure Replace_Element
     (Self : in out Base_List'Class; Position : Cursor; Element : Element_Type)
     with
       Global => null,
       Pre    => P_Mem (Positions (Self), Position),
       Post   => Capacity (Self) = Capacity (Self)'Old
          and Length (Self) = Length (Self)'Old

          --  Cursors are preserved.
          and Positions (Self)'Old = Positions (Self)

          --  The element at the position of Position in Self is replaced by E.
          and M.Is_Set (Model (Self)'Old,
                        P_Get (Positions (Self), Position),
                        Element,
                        Model (Self));
   --  See documentation in conts-lists-generics.ads

   function First_Primitive (Self : Base_List) return Cursor
     is (First (Self)) with Inline;
   --  See documentation in conts-lists-generics.ads

   function Element_Primitive
     (Self : Base_List; Position : Cursor) return Constant_Returned_Type
   --  See documentation in conts-lists-generics.ads
     is (Element (Self, Position))
     with
       Inline,
       Pre'Class => P_Mem (Positions (Self), Position),
       Post => Storage.Elements.To_Element (Element_Primitive'Result) =
          Element (Model (Self), P_Get (Positions (Self), Position));

   function Has_Element_Primitive
     (Self : Base_List; Position : Cursor) return Boolean
   --  See documentation in conts-lists-generics.ads
     is (Has_Element (Self, Position))
     with
       Inline,
       Pre'Class => Position = No_Element
            or P_Mem (Positions (Self), Position),
       Post      => Has_Element_Primitive'Result =
          P_Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Has_Element_Primitive);

   function Next_Primitive
     (Self : Base_List; Position : Cursor) return Cursor
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type List instead of List'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.
     is (Next (Self, Position))
     with
       Inline,
       Pre'Class => P_Mem (Positions (Self), Position);

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

   ------------------
   -- Formal Model --
   ------------------

   package P is new Conts.Functional.Maps
     (Element_Type => Positive_Count_Type,
      Key_Type     => Cursor);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   type P_Map is record
      Content : P.Map;
   end record;

   type P_Private_Cursor is new P.Private_Key;

   function P_Iter_First (M : P_Map) return P_Private_Cursor
     is (P_Private_Cursor (P.Iter_First (M.Content)));
   function P_Iter_Next
     (M : P_Map; C : P_Private_Cursor) return P_Private_Cursor
     is (P_Private_Cursor (P.Iter_Next (M.Content, P.Private_Key (C))));
   function P_Iter_Has_Element
     (M : P_Map; C : P_Private_Cursor) return Boolean
     is (P.Iter_Has_Element (M.Content, P.Private_Key (C)));
   function P_Iter_Element (M : P_Map; C : P_Private_Cursor) return Cursor
     is (P.Iter_Element (M.Content, P.Private_Key (C)));
   function P_Mem (M : P_Map; C : Cursor) return Boolean
     is (P.Mem (M.Content, C));
   function P_Get (M : P_Map; C : Cursor) return Positive_Count_Type
     is (P.Get (M.Content, C));

   function P_Is_Add
      (P1, P2 : P_Map;
       K      : Cursor;
       E      : Positive_Count_Type) return Boolean
     is
       (not P_Mem (P1, K) and P_Mem (P2, K) and P_Get (P2, K) = E
        and (for all I of P1 => P_Mem (P2, I)
             and P_Get (P2, I) = P_Get (P1, I))
        and (for all I of P2 => I = K or P_Mem (P1, I)));

   function P_Insert_Position
      (P1, P2 : P_Map; Cut : Positive_Count_Type) return Boolean
     is
       ((for all I of P1 => P_Mem (P2, I)
        and (if P_Get (P1, I) < Cut then P_Get (P2, I) = P_Get (P1, I)
             else P_Get (P2, I) = P_Get (P1, I) + 1))
       and (for all I of P2 => P_Mem (P1, I) or P_Get (P2, I) = Cut));

   function M_Not_Until
     (S : M.Sequence; Lst : Positive_Count_Type; E : Element_Type)
     return Boolean
     is (for all I in 1 .. Lst => Element (S, I) /= E);

   function M_Elements_Cst
     (S        : M.Sequence;
      Fst, Lst : Count_Type;
      E        : Element_Type)
      return Boolean
    is (for all I in Fst .. Lst => Element (S, I) = E);

   function M_Elements_Equal
     (S1, S2   : M.Sequence;
      Fst, Lst : Count_Type;
      Offset   : Count_Type'Base := 0)
      return Boolean
     is (for all I in Fst .. Lst =>
            Element (S1, I) = Element (S2, I + Offset));

end Conts.Lists.Impl;
