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

--  This package describes how to associate values with keys.
--  This is in particular used for graph algorithms to store whether a vertex
--  was visited.
--  There are two approaches here:
--    * Either the container itself is able to store the information directly
--      (in the vertex, the edge, or in some other internal field).
--    * Or an external data structure (provided just for the duration of
--      the algorithm) is used. For instance, a vector indexed by a unique
--      integer id associated with the vertices or edges. Or a map.
--
--  Both types of maps have similar generic profiles, but in some cases the
--  Map will be the container itself (and most likely the Clear operation
--  should do nothing). In other cases, the algorithm will need to create the
--  map explicitly.
--
--  In general, multiple versions of the algorithms will be provided, one for
--  each type of map (interior or exterior), and one that takes the map
--  explicitly in parameter so that the algorithm does not need to create the
--  map on its own, and the container can act as its own map.

pragma Ada_2012;
with Conts.Vectors.Definite_Unbounded;

package Conts.Properties is

   -------------------
   -- Property maps --
   -------------------

   generic
      type Map (<>) is limited private;
      --  The place where the information is stored, either the container
      --  itself or a separate data structure.

      type Key (<>) is limited private;
      type Value is private;

      with procedure Set (M : in out Map; K : Key; V : Value) is <>;
      with function Get (M : Map; K : Key) return Value is <>;
      with procedure Clear (M : in out Map) is null;
   package Maps is
   end Maps;

   ---------------------------
   -- Indexed property maps --
   ---------------------------
   --  An implementation of property maps that can be used when the key
   --  maps to indexes. These are implemented as vectors, rather than
   --  arrays, so that they can be used without necessarily knowing the
   --  required size in advance, and because a very large array for a
   --  very large container could blow the stack.
   --  The map is also set as a limited type to limit the number of
   --  copies. This ensures that Create_Map builds the map in place.

   generic
      type Container (<>) is limited private;
      type Key (<>) is limited private;
      type Value is private;

      Default_Value : Value;
      --  These maps are implemented as vectors, and a default value is needed
      --  when the vector is resized.

      type Index_Type is (<>);
      type Base_Type is abstract tagged limited private;

      with function Get_Index (K : Key) return Index_Type is <>;
      --  Maps the key to an index

      with function Length (G : Container) return Count_Type is <>;
      --  Use to reserve the initial capacity for the vector. This can
      --  safely return 0 or any values, since the vector is unbounded. But
      --  returning a proper value will speed things up by avoiding reallocs.

   package Indexed_Maps is

      package Value_Vectors is new Conts.Vectors.Definite_Unbounded
        (Index_Type, Value, Base_Type => Base_Type);
      type Map is limited record
         Values : Value_Vectors.Vector;
      end record;

      function Get (M : Map; K : Key) return Value;
      procedure Set (M : in out Map; K : Key; Val : Value);
      procedure Clear (M : in out Map);

      function Create_Map (G : Container) return Map;
      --  Create a new uninitialized map

      package As_Map is new Maps (Map, Key, Value, Set, Get, Clear);
   end Indexed_Maps;

end Conts.Properties;
