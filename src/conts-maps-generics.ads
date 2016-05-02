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

pragma Ada_2012;
with Conts.Cursors;
with Conts.Elements;
with Conts.Properties;

generic
   with package Keys is new Conts.Elements.Traits (<>);
   with package Elements is new Conts.Elements.Traits (<>);
   type Container_Base_Type is abstract tagged limited private;

   with function Hash (Key : Keys.Element_Type) return Hash_Type;

   type Probing is new Probing_Strategy with private;

   with package Pool is new Conts.Pools (<>);
   --  The storage pool used to allocate the buckets

   with function "="
     (Left  : Keys.Element_Type;
      Right : Keys.Stored_Type) return Boolean is <>;
   --  Compares a key given by the user with a stored key. For efficiency
   --  reasons, we do not convert Right back to Keys.Element_Type, although
   --  of course your function could do it with Keys.To_Return and
   --  Keys.To_Element.

   with function Resize_Strategy
     (Used     : Hash_Type;
      Fill     : Count_Type;
      Capacity : Count_Type) return Hash_Type is Resize_2_3;
   --  This function decides whether the hash-table needs to be resized.
   --  Resizing is a costly operation, so should be performed as rarely
   --  as possible. On the other hand, a table should have plenty of empty
   --  slots to make inserting efficient.
   --  If this function returns 0, no resizing takes place and the current
   --  capacity is preserved. Otherwise, it returns the new desired size
   --
   --  Used is the number of slots used to store elements.
   --  Fill is the number of slots used to store elements or previously
   --  used for now-deleted elements.
   --  Capacity is the maximum number of elements that can be stored.

package Conts.Maps.Generics with SPARK_Mode is

   subtype Key_Type is Keys.Element_Type;
   subtype Element_Type is Elements.Element_Type;
   subtype Returned_Type is Elements.Returned_Type;
   subtype Constant_Returned_Type is Elements.Constant_Returned_Type;

   package Impl is

      type Base_Map is new Container_Base_Type with private;

      type Cursor is private;
      No_Element : constant Cursor;

      type Pair_Type is private;
      function Key
        (P : Pair_Type) return Keys.Constant_Returned_Type with Inline;
      function Value
        (P : Pair_Type) return Elements.Constant_Returned_Type with Inline;

      function Capacity (Self : Base_Map'Class) return Count_Type with
        Global => null;
      procedure Assign
        (Self : in out Base_Map'Class; Source : Base_Map'Class);
      procedure Resize
        (Self     : in out Base_Map'Class;
         New_Size : Hash_Type)
      with
        Global => null;
      procedure Set
        (Self     : in out Base_Map'Class;
         Key      : Keys.Element_Type;
         Value    : Elements.Element_Type)
      with
        Global => null;
      function Get
        (Self     : Base_Map'Class;
         Key      : Keys.Element_Type)
         return Elements.Constant_Returned_Type
      with
        Global => null;
      procedure Clear (Self : in out Base_Map'Class) with
        Global => null;
      procedure Delete
        (Self     : in out Base_Map'Class;
         Key      : Keys.Element_Type)
      with
        Global => null;
      function Pair
        (Self : Base_Map'Class; Position : Cursor) return Pair_Type
         with Inline, Global => null;
      function Element
        (Self : Base_Map'Class; Position : Cursor)
         return Constant_Returned_Type
      is (Value (Pair (Self, Position)))
      with Global => null;
      function As_Element
        (Self : Base_Map'Class; Position : Cursor) return Element_Type
         is (Elements.To_Element (Value (Pair (Self, Position))))
         with Inline, Global => null;
      function First (Self : Base_Map'Class) return Cursor
        with Inline, Global => null;
      function Has_Element
        (Self : Base_Map'Class; Position : Cursor) return Boolean
        with Inline, Global => null;
      function Next
        (Self : Base_Map'Class; Position : Cursor) return Cursor
        with Inline, Global => null,
             Pre    => Has_Element (Self, Position);
      --  Actual implementation for the subprograms renamed below. See the
      --  descriptions below.

      function First_Primitive (Self : Base_Map) return Cursor
         is (First (Self)) with Inline;
      function Element_Primitive
        (Self : Base_Map; Position : Cursor) return Pair_Type
         is (Pair (Self, Position)) with Inline;
      function Has_Element_Primitive
        (Self : Base_Map; Position : Cursor) return Boolean
         is (Has_Element (Self, Position)) with Inline;
      function Next_Primitive
        (Self : Base_Map; Position : Cursor) return Cursor
         is (Next (Self, Position)) with Inline;
      --  These are only needed because the Iterable aspect expects a parameter
      --  of type Map instead of Map'Class. But then it means that the loop
      --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
      --  using an explicit cursor.

   private
      pragma SPARK_Mode (Off);
      procedure Adjust (Self : in out Base_Map);
      procedure Finalize (Self : in out Base_Map);
      --  In case the mp is a controlled type, but irrelevant when Self
      --  is not controlled.

      type Pair_Type is record
         Key   : Keys.Stored_Type;
         Value : Elements.Stored_Type;
      end record;

      function Key   (P : Pair_Type) return Keys.Constant_Returned_Type
         is (Keys.To_Constant_Returned (P.Key));
      function Value (P : Pair_Type) return Elements.Constant_Returned_Type
         is (Elements.To_Constant_Returned (P.Value));

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
      for Slot_Table_Access'Storage_Pool use Pool.Pool.all;

      type Cursor is record
         Index : Hash_Type := Hash_Type'Last;
      end record;
      No_Element : constant Cursor := (Index => Hash_Type'Last);

      type Base_Map is new Container_Base_Type with record
         Used   : Hash_Type := 0;
         --  Number of slots occupied by keys

         Fill   : Count_Type := 0;
         --  Number of slots occupied by keys or dummy slots

         Table  : Slot_Table_Access;
         --  The slots table. This is always a power of 2, since we use the
         --  size as a mask for hashes.
      end record;
   end Impl;

   subtype Base_Map is Impl.Base_Map;
   subtype Cursor is Impl.Cursor;
   No_Element : constant Cursor := Impl.No_Element;

   subtype Pair_Type is Impl.Pair_Type;
   function Key   (P : Pair_Type) return Keys.Constant_Returned_Type
     renames Impl.Key;
   function Value (P : Pair_Type) return Elements.Constant_Returned_Type
     renames Impl.Value;
   --  This record contains both the key and the value of an element stored

   function Capacity (Self : Base_Map'Class) return Count_Type
     renames Impl.Capacity;
   --  Return the current capacity of the container.

   procedure Resize
     (Self     : in out Base_Map'Class;
      New_Size : Hash_Type) renames Impl.Resize;
   --  Change the capacity of the container.
   --  If you know you are going to insert n items, calling Resize (n) is
   --  likely to improve the performance by limiting the number of times the
   --  map will be resized during the insertions.

   procedure Set
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type;
      Value    : Elements.Element_Type) renames Impl.Set;
   --  This never resizes Self if there is already an element with that key
   --  in the table. That means it is safe to iterate over a map and change
   --  some of the elements, but not insert new ones or remove ones.

   function Get
     (Self     : Base_Map'Class;
      Key      : Keys.Element_Type)
      return Elements.Constant_Returned_Type
     renames Impl.Get;
   --  Raises a Constraint_Error if there is no such element in the table

   procedure Delete
     (Self     : in out Base_Map'Class;
      Key      : Keys.Element_Type)
     renames Impl.Delete;
   --  Remove the element from the map.
   --  No exception is raised if the element is not in the map.

   procedure Clear (Self : in out Base_Map'Class) renames Impl.Clear;
   --  Remove all elements from the map

   function Pair
     (Self : Base_Map'Class; Position : Cursor) return Pair_Type
     renames Impl.Pair;
   function Element
     (Self : Base_Map'Class; Position : Cursor) return Constant_Returned_Type
     renames Impl.Element;
   function As_Element
     (Self : Base_Map'Class; Position : Cursor) return Elements.Element_Type
     renames Impl.As_Element;

   function First (Self : Base_Map'Class) return Cursor
     renames Impl.First;
   function Has_Element
     (Self : Base_Map'Class; Position : Cursor) return Boolean
     renames Impl.Has_Element;
   function Next
     (Self : Base_Map'Class; Position : Cursor) return Cursor
     renames Impl.Next;

   ------------------
   -- for-of loops --
   ------------------

   type Map is new Base_Map with null record
     with Constant_Indexing => Constant_Reference,
          Iterable => (First       => First_Primitive,
                       Next        => Next_Primitive,
                       Has_Element => Has_Element_Primitive,
                       Element     => Element_Primitive);

   function Constant_Reference
     (Self : Map; Key : Key_Type) return Constant_Returned_Type
     is (Get (Self, Key)) with Inline;

   -------------
   -- Cursors --
   -------------

   package Cursors is
      package Forward is new Conts.Cursors.Forward_Cursors
        (Container_Type => Base_Map'Class,
         Cursor_Type    => Cursor,
         First          => First,
         Next           => Next,
         Has_Element    => Has_Element);
   end Cursors;

   -------------------------
   -- Getters and setters --
   -------------------------

   package Maps is
      package Pair is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Pair_Type, Pair);
      package Element is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Element_Type, As_Element);
      package Constant_Returned is new Conts.Properties.Read_Only_Maps
        (Base_Map'Class, Cursor, Elements.Constant_Returned,
         Conts.Maps.Generics.Element);
   end Maps;

end Conts.Maps.Generics;
