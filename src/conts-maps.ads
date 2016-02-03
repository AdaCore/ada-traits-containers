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

package Conts.Maps is

   generic
      with package Keys is new Conts.Elements.Traits (<>);
      with package Elements is new Conts.Elements.Traits (<>);
      type Base_Type is abstract tagged limited private;

      with function Hash (Key : Keys.Element_Type) return Hash_Type;
      with function "="
        (Left  : Keys.Element_Type;
         Right : Keys.Stored_Type) return Boolean is <>;

   package Maps is

      type Map is new Base_Type with private;

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

      type Map is new Base_Type with record
         Used   : Hash_Type := 0;
         --  Number of slots occupied by keys

         Fill   : Count_Type := 0;
         --  Number of slots occupied by keys or dummy slots

         Table  : Slot_Table_Access;
         --  The slots table. This is always a power of 2, since we use the
         --  size as a mask for hashes.

      end record;
   end Maps;

end Conts.Maps;
