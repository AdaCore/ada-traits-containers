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
with Conts.Elements;

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

package Conts.Maps.Generics is

   type Map is new Container_Base_Type with private;

   function Capacity (Self : Map'Class) return Count_Type;
   --  Return the current capacity of the container.

   procedure Resize
     (Self     : in out Map'Class;
      New_Size : Hash_Type);
   --  Change the capacity of the container

   procedure Set
     (Self     : in out Map'Class;
      Key      : Keys.Element_Type;
      Value    : Elements.Element_Type);
   --  This never resizes Self if there is already an element with that key
   --  in the table. That means it is safe to iterate over a map and change
   --  some of the elements, but not insert new ones or remove ones.

   function Get
     (Self     : Map'Class;
      Key      : Keys.Element_Type)
         return Elements.Return_Type;
   --  Raises a Constraint_Error if there is no such element in the table

   procedure Delete
     (Self     : in out Map'Class;
      Key      : Keys.Element_Type);
   --  Remove the element from the map.
   --  No exception is raised if the element is not in the map.

   procedure Clear (Self : in out Map'Class);
   --  Remove all elements from the map

   type Cursor is private;

   type Pair is private;
   function Key   (P : Pair) return Keys.Return_Type with Inline;
   function Value (P : Pair) return Elements.Return_Type with Inline;
   --  This record contains both the key and the value of an element stored
   --  in the table.

   function First (Self : Map'Class) return Cursor
     with Inline, Global => null;
   function Element
     (Self : Map'Class; Position : Cursor) return Pair
     with Inline, Global => null;
   function Has_Element
     (Self : Map'Class; Position : Cursor) return Boolean
     with Inline, Global => null;
   function Next
     (Self : Map'Class; Position : Cursor) return Cursor
     with Inline,
     Global => null,
     Pre    => Has_Element (Self, Position);

   function First_Primitive (Self : Map) return Cursor
      is (First (Self)) with Inline;
   function Element_Primitive
     (Self : Map; Position : Cursor) return Pair
     is (Element (Self, Position)) with Inline;
   function Has_Element_Primitive
     (Self : Map; Position : Cursor) return Boolean
     is (Has_Element (Self, Position)) with Inline;
   function Next_Primitive
     (Self : Map; Position : Cursor) return Cursor
     is (Next (Self, Position)) with Inline;
   --  These are only needed because the Iterable aspect expects a parameter
   --  of type Map instead of Map'Class. But then it means that the loop
   --  is doing a lot of dynamic dispatching, and is twice as slow as a loop
   --  using an explicit cursor.

private
   procedure Adjust (Self : in out Map);
   procedure Finalize (Self : in out Map);
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
   for Slot_Table_Access'Storage_Pool use Pool.Pool.all;

   type Pair is record
      Key   : Keys.Stored_Type;
      Value : Elements.Stored_Type;
   end record;

   type Cursor is record
      Index : Hash_Type := Hash_Type'Last;
   end record;
   No_Element : constant Cursor := (Index => Hash_Type'Last);

   type Map is new Container_Base_Type with record
      Used   : Hash_Type := 0;
      --  Number of slots occupied by keys

      Fill   : Count_Type := 0;
      --  Number of slots occupied by keys or dummy slots

      Table  : Slot_Table_Access;
      --  The slots table. This is always a power of 2, since we use the
      --  size as a mask for hashes.
   end record;

   function Key   (P : Pair) return Keys.Return_Type
   is (Keys.To_Return (P.Key));
   function Value (P : Pair) return Elements.Return_Type
   is (Elements.To_Return (P.Value));

end Conts.Maps.Generics;
