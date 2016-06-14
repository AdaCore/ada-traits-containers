------------------------------------------------------------------------------
--                     Copyright (C) 2016-2016, AdaCore                     --
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

--  An implementation of property maps that can be used when the key
--  maps to indexes. These are implemented as vectors, rather than
--  arrays, so that they can be used without necessarily knowing the
--  required size in advance, and because a very large array for a
--  very large container could blow the stack.
--  The map is also set as a limited type to limit the number of
--  copies. This ensures that Create_Map builds the map in place.

pragma Ada_2012;
with Conts.Vectors.Definite_Unbounded;

generic
   type Container_Type (<>) is limited private;
   type Key_Type (<>) is limited private;
   type Element_Type is private;

   Default_Value : Element_Type;
   --  These maps are implemented as vectors, and a default value is needed
   --  when the vector is resized.

   type Index_Type is (<>);
   type Container_Base_Type is abstract tagged limited private;

   with function Get_Index (K : Key_Type) return Index_Type is <>;
   --  Maps the key to an index

   with function Length (G : Container_Type) return Count_Type is <>;
   --  Use to reserve the initial capacity for the vector. This can
   --  safely return 0 or any values, since the vector is unbounded. But
   --  returning a proper value will speed things up by avoiding reallocs.

package Conts.Properties.Indexed is

   package Value_Vectors is new Conts.Vectors.Definite_Unbounded
     (Index_Type, Element_Type, Container_Base_Type => Container_Base_Type);

   type Map is limited record
      Values : Value_Vectors.Vector;
   end record;
   function Get (M : Map; K : Key_Type) return Element_Type;
   procedure Set (M : in out Map; K : Key_Type; Val : Element_Type);
   procedure Clear (M : in out Map);

   function Create_Map (G : Container_Type) return Map;
   --  Create a new uninitialized map

   package As_Map is new Maps (Map, Key_Type, Element_Type, Set, Get, Clear);
   package As_Read_Only renames As_Map.As_Read_Only;

end Conts.Properties.Indexed;
