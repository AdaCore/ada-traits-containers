------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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

--  Implementation details for the map container.
--  This package takes the same formal arguments as Conts.Vectors.Generics
--  and provides the internal implementation as well as annotations for
--  all the primitive operations.

pragma Ada_2012;
with Conts.Elements;
with Conts.Functional.Sequences;
with Conts.Functional.Maps;

generic
   with package Keys is new Conts.Elements.Traits (<>);
   with package Elements is new Conts.Elements.Traits (<>);
   type Container_Base_Type is abstract tagged limited private;
   with function Hash (Key : Keys.Element_Type) return Hash_Type;
   type Probing is new Probing_Strategy with private;
   with package Pool is new Conts.Pools (<>);
   with function "="
     (Left  : Keys.Element_Type;
      Right : Keys.Stored_Type) return Boolean is <>;
   with function Resize_Strategy
     (Used     : Count_Type;
      Fill     : Count_Type;
      Capacity : Count_Type) return Count_Type is Resize_2_3;
package Conts.Maps.Impl with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   subtype Key_Type is Keys.Element_Type;
   subtype Element_Type is Elements.Element_Type;
   subtype Returned_Type is Elements.Returned_Type;
   subtype Constant_Returned_Type is Elements.Constant_Returned_Type;
   subtype Constant_Returned_Key_Type is Keys.Constant_Returned_Type;

   use type Elements.Element_Type;
   use type Elements.Constant_Returned_Type;
   use type Keys.Element_Type;
   use type Keys.Constant_Returned_Type;

   type Base_Map is new Container_Base_Type with private;

   type Cursor is private;
   No_Element : constant Cursor;
   --  A cursor is only valid until the next change to the map. As soon as
   --  an element is added or removed, the cursor should no longer be used.
   --  For performance reasons, this is not checked.

   function Capacity (Self : Base_Map'Class) return Count_Type with
     Global => null;

   function Length (Self : Base_Map'Class) return Count_Type with
     Global => null,
     Post   => Length'Result = 0 or Length'Result < Capacity (Self);
   --  The length of a map is always strictly smaller than its capacity
   --  except when the map's capacity is 0.

   ------------------
   -- Formal Model --
   ------------------

   type P_Map is private with Ghost,
     Iterable => (First => P_Iter_First,
                  Has_Element => P_Iter_Has_Element,
                  Next => P_Iter_Next,
                  Element => P_Iter_Element);

   function "=" (M1, M2 : P_Map) return Boolean with
     Ghost,
     Global => null,
     Post   => "="'Result =
       ((for all K of M1 =>
            P_Mem (M2, K) and then P_Get (M2, K) = P_Get (M1, K))
        and then (for all K of M2 => P_Mem (M1, K)));

   type P_Private_Cursor is private with Ghost;
   function P_Iter_First (M : P_Map) return P_Private_Cursor with Ghost;
   function P_Iter_Next
     (M : P_Map; C : P_Private_Cursor) return P_Private_Cursor with Ghost;
   function P_Iter_Has_Element
     (M : P_Map; C : P_Private_Cursor) return Boolean with Ghost;
   function P_Iter_Element (M : P_Map; C : P_Private_Cursor) return Cursor
     with Ghost;

   function P_Mem (M : P_Map; C : Cursor) return Boolean with Ghost;
   function P_Get (M : P_Map; C : Cursor) return Positive_Count_Type
     with
       Ghost,
       Pre => P_Mem (M, C);
   pragma Annotate (GNATprove, Iterable_For_Proof, "Contains", P_Mem);

   package K is new Conts.Functional.Sequences
     (Element_Type => Key_Type,
      Index_Type   => Positive_Count_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   package M is new Conts.Functional.Maps
     (Element_Type => Element_Type,
      Key_Type     => Key_Type);
   --  This instance should be ghost but it is not currently allowed by the RM.
   --  See P523-006

   use type M.Map;
   use type K.Sequence;

   function Model (Self : Base_Map'Class) return M.Map
   --  The highlevel model of a map is a map from keys to elements. Neither
   --  cursors nor order of elements are represented in this model.
     with
       Ghost,
       Global => null;

   function S_Keys (Self : Base_Map'Class) return K.Sequence
   --  The S_Keys sequence represents the underlying list structure of
   --  maps that is used for iteration. It does not model cursors nor elements.
     with
       Ghost,
       Global => null,
       Post   => K.Length (S_Keys'Result) = Length (Self)

          --  It only contains keys contained in Model.
          and then (for all Key of S_Keys'Result => M.Mem (Model (Self), Key))

          --  It contains all the keys contained in Model.
          and then (for all Key of Model (Self) =>
                      (for some L of S_Keys'Result => L = Key))

          --  It has no duplicate.
          and then
            (for all I in 1 .. Length (Self) =>
               (for all J in 1 .. Length (Self) =>
                    (if K.Get (S_Keys'Result, I) =
                           K.Get (S_Keys'Result, J)
                           then I = J)));

   function Positions (Self : Base_Map'Class) return P_Map
   --  The Positions map is used to model cursors. It only contains valid
   --  cursors and map them to their position in the container.
     with
       Ghost,
       Global => null,
       Post   => not P_Mem (Positions'Result, No_Element)

          --  Positions of cursors are smaller than the container's length.
          and then
            (for all I of Positions'Result =>
               P_Get (Positions'Result, I) in 1 .. Length (Self)

             --  No two cursors have the same position. Note that we do not
             --  state that there is a cursor in the map for each position,
             --  as it is rarely needed.
             and then
               (for all J of Positions'Result =>
                  (if P_Get (Positions'Result, I) =
                       P_Get (Positions'Result, J)
                       then I = J)));

   procedure Lift_Abstraction_Level (Self : Base_Map'Class)
   --  Lift_Abstraction_Level is a ghost procedure that does nothing but
   --  assume that we can access to the same elements by iterating over
   --  positions or cursors.
   --  This information is not generally useful except when switching from
   --  a lowlevel, cursor aware view of a container, to a highlevel position
   --  based view.
     with
       Ghost,
       Global => null,
       Post   =>
         (for all Key of S_Keys (Self) =>
            (for some Cu of Positions (Self) =>
                 K.Get (S_Keys (Self),
                        P_Get (Positions (Self), Cu)) = Key));

   function Element (S : M.Map; K : Key_Type) return Element_Type
     renames M.Get;

   -----------------
   -- Subprograms --
   -----------------

   function Contains (Self : Base_Map'Class; Key : Key_Type) return Boolean
     with
       Global => null,
       Post   => Contains'Result = M.Mem (Model (Self), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Contains);

   procedure Assign
     (Self : in out Base_Map'Class; Source : Base_Map'Class)
     with
       Global => null,
       Post   => Length (Self) = Length (Source)
          and then Model (Self) = Model (Source);

   procedure Resize
     (Self     : in out Base_Map'Class;
      New_Size : Count_Type)
   --  Change the capacity of the container.
   --  It does not change the high level model of Self.
     with
       Global => null,
       Post   => Length (Self) = Length (Self)'Old
          and Model (Self) = Model (Self)'Old
          and Capacity (Self) >= New_Size;

   procedure Set
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type;
      Value    : Elements.Element_Type)
   --  Insert a key Key and an element Element in Self if Key is not already in
   --  present.
   --  Otherwise, replace the element associated to Key by Element.
     with
       Global         => null,
       Pre            => Contains (Self, Key)
          or else Length (Self) < Count_Type'Last - 1,
       Contract_Cases =>
          --  If Key is already in Self, then Key now maps to Element in Model.
          (M.Mem (Model (Self), Key) => Capacity (Self) = Capacity (Self)'Old
              and Length (Self) = Length (Self)'Old
              and M.Is_Set (Model (Self)'Old, Key, Value, Model (Self))

              --  Keys and cursors are preserved
              and S_Keys (Self) = S_Keys (Self)'Old
              and Positions (Self) = Positions (Self)'Old,

           --  If Key was not in Self, then Element is a new element of its
           --  model.
           others => Capacity (Self) >= Capacity (Self)'Old
              and Length (Self) = Length (Self)'Old + 1
              and M.Is_Add (Model (Self)'Old, Key, Value, Model (Self)));

   function Get
     (Self : Base_Map'Class;
      Key  : Keys.Element_Type)
      return Elements.Constant_Returned_Type
     with
       Global => null,
       Pre    => Contains (Self, Key),
       Post   => Elements.To_Element (Get'Result) =
       Element (Model (Self), Key);

   function As_Element
     (Self : Base_Map'Class; Key : Keys.Element_Type) return Element_Type
     is (Elements.To_Element (Self.Get (Key)))
     with
       Inline,
       Global => null,
       Pre    => Contains (Self, Key),
       Post   => As_Element'Result = Element (Model (Self), Key);
   pragma Annotate (GNATprove, Inline_For_Proof, As_Element);

   procedure Clear (Self : in out Base_Map'Class)
   --  Remove all elements from the map
     with
       Global => null,
       Post   => Length (Self) = 0
          and then M.Is_Empty (Model (Self));

   function Mapping_Preserved
      (S1, S2 : K.Sequence; P1, P2 : P_Map) return Boolean
   --  The mapping between cursors and elements represented by the position
   --  map P1 and the sequence of keys S1 is preserved by P2 and S2.
     with
       Ghost,
       Post => Mapping_Preserved'Result =
         (for all I of P1 =>
             P_Mem (P2, I)
             and then K.Get (S1, P_Get (P1, I)) = K.Get (S2, P_Get (P2, I)));

   function Cursors_Preserved
      (P1, P2 : P_Map; S1 : K.Sequence; V : Key_Type) return Boolean
   --  The cursors of P1 are those of P2 except the one mapped to V by S1.
     with
       Ghost,
       Post => Cursors_Preserved'Result =
         (for all I of P1 =>
             P_Mem (P2, I) or else K.Get (S1, P_Get (P1, I)) = V);

   procedure Delete
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type)
   --  Remove the element from the map.
   --  No exception is raised if the element is not in the map.
     with
       Global         => null,
       Post           => Capacity (Self) = Capacity (Self)'Old,
       Contract_Cases =>
          --  If Key was in Self then it is removed from its model.

          (M.Mem (Model (Self), Key) =>
               Length (Self) = Length (Self)'Old - 1
              and M.Is_Add
                 (Model (Self),
                  Key,
                  Element (Model (Self)'Old, Key),
                  Model (Self)'Old)

              --  Cursors that are valid in Self were already valid and
              --  designating the same element.

              and Mapping_Preserved
                 (S1 => S_Keys (Self),
                  S2 => S_Keys (Self)'Old,
                  P1 => Positions (Self),
                  P2 => Positions (Self)'Old)

              --  Cursors that were valid in Self continue to be valid in Self
              --  except for the newly deleted cursor.
              --  Nothing is said about the order of keys in Self after the
              --  call.

              and Cursors_Preserved
                 (P1 => Positions (Self)'Old,
                  P2 => Positions (Self),
                  S1 => S_Keys (Self)'Old,
                  V  => Key),

           --  If Key was not in Self, then nothing is changed.
           others => Length (Self) = Length (Self)'Old
              and Model (Self)'Old = Model (Self)
              and S_Keys (Self)'Old = S_Keys (Self)
              and Positions (Self)'Old = Positions (Self));

   function Key
     (Self : Base_Map'Class; Position : Cursor)
     return Constant_Returned_Key_Type
     with
       Inline,
       Global => null,
       Pre    => Has_Element (Self, Position),
       Post   =>
          --  Query Positions to get the position of Position in Self and use
          --  it to fetch the corresponding key in Keys.
          Keys.To_Element (Key'Result) =
             K.Get (S_Keys (Self), P_Get (Positions (Self), Position));

   function As_Key
     (Self : Base_Map'Class; Position : Cursor) return Key_Type
     is (Keys.To_Element (Self.Key (Position)))
     with
       Inline,
       Global => null,
       Pre    => Has_Element (Self, Position),
       Post => As_Key'Result =
          K.Get (S_Keys (Self), P_Get (Positions (Self), Position));
   pragma Annotate (GNATprove, Inline_For_Proof, As_Key);

   function Element
     (Self : Base_Map'Class; Position : Cursor)
     return Constant_Returned_Type
     with
        Inline,
        Global => null,
        Pre    => Has_Element (Self, Position),
        Post   =>
           --  Query Positions to get the position of Position in Self, use it
           --  to fetch the corresponding key in Keys, and then use this key to
           --  get the associated element from Model.
           Elements.To_Element (Element'Result) = Element
              (Model (Self), K.Get (S_Keys (Self),
               P_Get (Positions (Self), Position)));

   function As_Element
     (Self : Base_Map'Class; Position : Cursor) return Element_Type
     is (Elements.To_Element (Self.Element (Position)))
     with
       Inline,
       Global => null,
       Pre    => Has_Element (Self, Position),
       Post => As_Element'Result = Element
          (Model (Self), K.Get (S_Keys (Self),
           P_Get (Positions (Self), Position)));
   pragma Annotate (GNATprove, Inline_For_Proof, As_Element);

   function First (Self : Base_Map'Class) return Cursor
     with
       Inline,
       Global         => null,
       Contract_Cases =>
         (Length (Self) = 0 => First'Result = No_Element,
          others            => Has_Element (Self, First'Result)
             and then P_Get (Positions (Self), First'Result) = 1);

   function Has_Element
     (Self : Base_Map'Class; Position : Cursor) return Boolean
     with
       Inline,
       Global => null,
       Post   => Has_Element'Result = P_Mem (Positions (Self), Position);
   pragma Annotate (GNATprove, Inline_For_Proof, Entity => Has_Element);

   function Next
     (Self : Base_Map'Class; Position : Cursor) return Cursor
   --  Actual implementation for the subprograms renamed in generics. See the
   --  descriptions in generics.
     with
       Inline,
       Global         => null,
       Pre            => Has_Element (Self, Position),
       Contract_Cases =>
         (P_Get (Positions (Self), Position) = Length (Self) =>
             not Has_Element (Self, Next'Result),
          others => Has_Element (Self, Next'Result)
             and then P_Get (Positions (Self), Next'Result) =
               P_Get (Positions (Self), Position) + 1);

   function First_Primitive (Self : Base_Map) return Cursor
      is (First (Self)) with Inline;

   function Key_Primitive
     (Self : Base_Map; Position : Cursor) return Constant_Returned_Key_Type
     is (Key (Self, Position))
     with
       Inline,
       Pre'Class => Has_Element (Self, Position);

   function Has_Element_Primitive
     (Self : Base_Map; Position : Cursor) return Boolean
     is (Has_Element (Self, Position)) with Inline;

   function Next_Primitive
     (Self : Base_Map; Position : Cursor) return Cursor
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type Map instead of Map'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.
     is (Next (Self, Position))
     with
       Inline,
       Pre'Class => Has_Element (Self, Position);

private
   pragma SPARK_Mode (Off);
   procedure Adjust (Self : in out Base_Map);
   procedure Finalize (Self : in out Base_Map);
   --  In case the mp is a controlled type, but irrelevant when Self
   --  is not controlled.

   type Slot_Kind is (Empty, Dummy, Full);
   --  A node can have three statuses:
   --    * empty: it was never assigned
   --      Hash is 0
   --    * dummy: the node had been allocated, but was then deleted. It can
   --      be reused as soon as possible
   --      Hash is 0
   --    * full: the node is currently in use.

   type Slot is record
      Key   : Keys.Stored_Type;
      Value : Elements.Stored_Type;
      Hash  : Hash_Type;
      --  Cached value for the hash, to speed lookups in the table
      --  (before we do more extensive comparison of the key), and
      --  also to speed up the resizing.
      Kind  : Slot_Kind := Empty;
   end record;
   pragma Pack (Slot);
   --  The order of fields is important here to achieve a compact structure
   --  and save memory.
   --  On our example with 250000 items stored in the table, we end up
   --  allocating/reallocating 15900kb instead of 19500kb.

   type Slot_Table is array (Hash_Type range <>) of Slot;
   type Slot_Table_Access is access Slot_Table;
   for Slot_Table_Access'Storage_Pool use Pool.Pool;

   type Cursor is record
      Index : Hash_Type := Hash_Type'Last;
   end record;
   No_Element : constant Cursor := (Index => Hash_Type'Last);

   type Base_Map is new Container_Base_Type with record
      Used   : Count_Type := 0;
      --  Number of slots occupied by keys

      Fill   : Count_Type := 0;
      --  Number of slots occupied by keys or dummy slots

      Table  : Slot_Table_Access;
      --  The slots table. This is always a power of 2, since we use the
      --  size as a mask for hashes.
   end record;

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

   use type P.Map;

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

   function "=" (M1, M2 : P_Map) return Boolean is (M1.Content = M2.Content);

   function Mapping_Preserved
     (S1, S2 : K.Sequence; P1, P2 : P_Map) return Boolean
     is (for all I of P1 => P_Mem (P2, I)
         and K.Get (S1, P_Get (P1, I)) = K.Get (S2, P_Get (P2, I)));

   function Cursors_Preserved
     (P1, P2 : P_Map; S1 : K.Sequence; V : Key_Type) return Boolean
     is (for all I of P1 => P_Mem (P2, I) or K.Get (S1, P_Get (P1, I)) = V);

end Conts.Maps.Impl;
