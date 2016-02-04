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
   type Map (<>) is new Maps.Map with private;
package Conts.Maps.Cursors with SPARK_Mode is
   --  Convenient package for creating the cursors traits for a Map.
   --  These cursor traits cannot be instantiated in Generic_Maps itself,
   --  since the Map type is frozen too late.
   --  We also assume that Map might be a child of Maps.Map, not the
   --  same type directly, so we need to have proxies for the cursor
   --  subprograms

   subtype Cursor is Maps.Cursor;
   subtype Pair is Maps.Pair;

   function Cursors_First (Self : Map'Class) return Cursor
      is (Maps.First (Self)) with Inline;
   function Cursors_Element
      (Self : Map'Class; Position : Cursor) return Pair
      is (Maps.Element (Self, Position))
      with Pre => Maps.Has_Element (Self, Position),
           Inline => True;
   function Cursors_Has_Element
      (Self : Map'Class; Position : Cursor) return Boolean
      is (Maps.Has_Element (Self, Position))
      with Inline => True;
   function Cursors_Next
      (Self : Map'Class; Position : Cursor) return Cursor
      is (Maps.Next (Self, Position))
      with Pre => Maps.Has_Element (Self, Position),
           Inline => True;

   package Constant_Forward is
      new Conts.Cursors.Constant_Forward_Traits
         (Container    => Map'Class,
          Cursor       => Cursor,
          Return_Type  => Pair,
          First        => Cursors_First,
          Next         => Cursors_Next,
          Has_Element  => Cursors_Has_Element,
          Element      => Cursors_Element);

end Conts.Maps.Cursors;
