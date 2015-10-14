------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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
with Interfaces; use Interfaces;

package Conts.Dicts is

   generic
      type Index_Type is mod <>;

      with package Keys is new Elements_Traits (<>);

      with package Values is new Elements_Traits (<>);

      with function Hash
        (Key : Keys.Element_Type)
         return Index_Type;
      --  Return hash value for key element

      with function Equals
        (Left, Right : Keys.Element_Type)
         return Boolean;
      --  Return True if keys are considered equivalent, False otherwise

      with function Container_Size
        (Current_Size, Number_Of_Elements : Index_Type)
        return Index_Type;
      --  Given the current container size and the expected number of elements
      --  in the dictionary, returns the new container size. The current
      --  container size is needed in order to be able to have different
      --  behaviors whether dictionary is shrinking or expanding.

      In_Place_Table_Limit : Index_Type := 4;
      --  If the size of the dict is inferior or equal to that limit then the
      --  elements are stored in an inplace table inside the dict structure.
      --  Note that in that case the hash function is not used.
   package Dict_Traits is
   end Dict_Traits;

   generic
      with package Dicts is new Dict_Traits (<>);
   package Generic_Dicts is
      use Dicts;

      subtype Key_Type is Dicts.Keys.Element_Type;
      subtype Value_Type is Dicts.Values.Element_Type;

      type Dict is private;

      type Cursor is private;

      Key_Error : exception;

      procedure Allocate
        (Self     : in out Dict;
         Min_Size : Index_Type := 0);
      --  Allocate memory for a dictionary. The function should be called to
      --  initialize a dictionary. Min_Size can be used to allocate sufficient
      --  to store Min_Size elements. Note that to ensure that the dictionary
      --  won't be shrinked at first insertion you should set Enable_Realloc to
      --  False when calling Set function.

      procedure Set
        (Self           : in out Dict;
         Key            : Key_Type;
         Value          : Value_Type;
         Enable_Realloc : Boolean := True);
      --  Do Self[Key] = Value. If a Value is already associated with Key then
      --  the element is replaced. If Enable_Realloc is set to False then no
      --  attempt to resize the dictionary will be done.

      function Element (Self : Dict; Key : Key_Type) return Value_Type;
      --  Retrieve value associated with Key. If there is no value associated
      --  with Key then a Key_Error exception is raised.

      function Contains (Self : Dict; Key : Key_Type) return Boolean;
      --  Return True if there is a value associated with Key in dictionary
      --  Self. Otherwise return False.

      procedure Delete
         (Self        : in out Dict;
          Key         : Key_Type;
          Freeze_Size : Boolean := False);

      function Size (Self : Dict) return Index_Type;
      --  Return the number of elements stored in the dictionary.

      procedure Release (Self : in out Dict);
      --  Reset dictionary and free allocated space

      function Copy (Self : Dict) return Dict;
      --  Do a shallow copy of the dictionary

      ----------------------
      -- Cursor functions --
      ----------------------

      function Element (Self : Dict; Key : Key_Type) return Cursor with Inline;

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Key      : Key_Type;
                     Value    : Value_Type;
                     Enable_Realloc : Boolean := True);

      function Value (Self : Dict; Position : Cursor) return Value_Type
         with Inline;
      function Key (Self : Dict; Position : Cursor) return Key_Type
         with Inline;
      function First (Self : Dict) return Cursor;
      function Next (Self : Dict; Position : Cursor) return Cursor;
      function Has_Element
         (Self : Dict; Position : Cursor) return Boolean with Inline;

   private
      use Dicts;

      type Slot_Kind is (Empty, Dummy, Active);

      type Slot (Kind : Slot_Kind := Empty) is record
         case Kind is
            when Active =>
               Hash  : Index_Type;
               Key   : Dicts.Keys.Stored_Element_Type;
               Value : Dicts.Values.Stored_Element_Type;
            when others =>
               null;
         end case;
      end record;

      Empty_Slot : constant Slot := (Kind => Empty);
      Dummy_Slot : constant Slot := (Kind => Dummy);

      type Cursor is record
         Kind  : Slot_Kind;
         Index : Index_Type;
         Hash  : Index_Type;
      end record;

      Null_Cursor : constant Cursor := (Empty, Index_Type'Last, 0);

      type Dict_Kind is (Short_Dict, Long_Dict);

      type Slot_Table is array (Index_Type range <>) of Slot;

      type Slot_Table_Access is access Slot_Table;

      type Dict (Kind : Dict_Kind := Short_Dict) is record
         Fill   : Index_Type := 0;
         Active : Index_Type := 0;
         Mask   : Index_Type := Dicts.In_Place_Table_Limit - 1;
         case Kind is
            when Short_Dict =>
               Short_Table : Slot_Table (0 .. In_Place_Table_Limit - 1);
            when Long_Dict =>
               Long_Table  : Slot_Table_Access := null;
         end case;
      end record;

   end Generic_Dicts;

   function Dynamic_Container_Size
     (Current_Size, Number_Of_Elements : Unsigned_32) return Unsigned_32;

end Conts.Dicts;
