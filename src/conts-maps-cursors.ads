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

--  A generic and general Map implementation
--  By providing appropriate values for the formal parameters, the same
--  implementation can be used for bounded and unbounded containers, or for
--  constrained and unconstrained elements.
--
--  Design: in C++ STL, none of the methods are virtual, so there is no
--  dynamic dispatching. We achieve the same here by using 'Class
--  parameters.  This still let's use Ada2012 dot notation (the reason why
--  we use a tagged type, in addition to the Iterable aspect), while
--  increasing the performance (the count-with-explicit-loop goes from 0.25s
--  to 0.51s when we do not use 'Class parameters).

pragma Ada_2012;
with Conts.Cursors;
with Conts.Maps.Generics;

generic
   with package Maps is new Conts.Maps.Generics (<>);
package Conts.Maps.Cursors with SPARK_Mode is
   --  Convenient package for creating the cursors traits for a Map.
   --  These cursor traits cannot be instantiated in Generic_Maps itself,
   --  since the Map type is frozen too late.
   --  We also assume that Map might be a child of Maps.Map, not the
   --  same type directly, so we need to have proxies for the cursor
   --  subprograms

   subtype Map       is Maps.Map;
   subtype Cursor    is Maps.Cursor;
   subtype Pair_Type is Maps.Pair_Type;

   package Forward is new Conts.Cursors.Forward_Cursors
     (Container_Type => Map'Class,
      Cursor_Type    => Cursor,
      First          => Maps.First,
      Next           => Maps.Next,
      Has_Element    => Maps.Has_Element);

end Conts.Maps.Cursors;
