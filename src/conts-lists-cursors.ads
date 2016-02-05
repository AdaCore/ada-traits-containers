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

--  A generic and general list implementation
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
with Conts.Lists.Generics;

generic
   with package Lists is new Conts.Lists.Generics (<>);
   type List_Type (<>) is new Lists.List with private;
package Conts.Lists.Cursors with SPARK_Mode is
   --  Convenient package for creating the cursors traits for a list.
   --  These cursor traits cannot be instantiated in Generic_Lists itself,
   --  since the List type is frozen too late.
   --  We also assume that List might be a child of Lists.List, not the
   --  same type directly, so we need to have proxies for the cursor
   --  subprograms

   subtype List is List_Type;
   subtype Cursor is Lists.Cursor;
   subtype Element is Lists.Nodes.Elements.Element;
   subtype Returned is Lists.Nodes.Elements.Returned;

   function Cursors_First (Self : List'Class) return Cursor
      is (Lists.First (Self)) with Inline;
   function Cursors_Element
      (Self : List'Class; Position : Cursor) return Returned
      is (Lists.Element (Self, Position))
      with Pre => Lists.Has_Element (Self, Position),
           Inline => True;
   function Cursors_Has_Element
      (Self : List'Class; Position : Cursor) return Boolean
      is (Lists.Has_Element (Self, Position))
      with Inline => True;
   function Cursors_Next
      (Self : List'Class; Position : Cursor) return Cursor
      is (Lists.Next (Self, Position))
      with Pre => Lists.Has_Element (Self, Position),
           Inline => True;
   function Cursors_Previous
      (Self : List'Class; Position : Cursor) return Cursor
      is (Lists.Previous (Self, Position))
      with Pre => Lists.Has_Element (Self, Position),
           Inline => True;

   package Constant_Bidirectional is
      new Conts.Cursors.Constant_Bidirectional_Traits
         (Container_Type => List'Class,
          Cursor_Type    => Cursor,
          Returned_Type  => Returned,
          First          => Cursors_First,
          Next           => Cursors_Next,
          Has_Element    => Cursors_Has_Element,
          Element        => Cursors_Element,
          Previous       => Cursors_Previous);
   package Constant_Forward renames Constant_Bidirectional.Constant_Forward;

end Conts.Lists.Cursors;
