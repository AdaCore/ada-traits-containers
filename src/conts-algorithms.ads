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
with Conts.Cursors;
with Conts.Properties;

package Conts.Algorithms is

   --------------
   -- Count_If --
   --------------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Key_Type => Cursors.Cursor, others => <>);
   function Count_If_With_External_Get
     (Self      : Cursors.Container;
      Map       : Getters.Map;
      Predicate : not null access
        function (E : Getters.Element) return Boolean)
     return Natural
     with Global => null;
   --  Count the number of elements in the container that match the predicate

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
   function Count_If
     (Self      : Cursors.Container;
      Predicate : not null access
        function (E : Getters.Element) return Boolean)
     return Natural
     with Global => null;
   --  Same as above, but the container itself is the property map to
   --  retrieve the element

   -------------
   -- Shuffle --
   -------------

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Random is new Conts.Uniform_Random_Traits
        (Discrete_Type => Cursors.Index, others => <>);
      with procedure Swap
        (Self        : in out Cursors.Container;
         Left, Right : Cursors.Index) is <>;
   procedure Shuffle
     (Self : in out Cursors.Container;
      Gen  : in out Random.Generator)
     with Global => null;
   --  Generates a random permutation of Self.
   --  If you 'use' the package for your container (vector for instance), then
   --  Swap will generally be visible by default.
   --  Complexity: O(n)

   ----------
   -- Find --
   ----------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
   function Find
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Cursors.Cursor
     with Global => null;

   --------------
   -- Contains --
   --------------

   generic
      with package Cursors is new Conts.Cursors.Forward_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Cursor,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
   function Contains
     (Self      : Cursors.Container;
      E         : Getters.Element)
     return Boolean
     with Global => null;

   ------------
   -- Equals --
   ------------

   generic
      with package Cursors is new Conts.Cursors.Random_Access_Cursors (<>);
      with package Getters is new Conts.Properties.Read_Only_Maps
        (Map_Type => Cursors.Container,
         Key_Type => Cursors.Index_Type,
         others   => <>);
      with function "=" (K1, K2 : Getters.Element) return Boolean is <>;
   function Equals (Left, Right  : Cursors.Container) return Boolean
     with Global => null;

end Conts.Algorithms;
