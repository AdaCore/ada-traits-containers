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

--  Convenience package for creating the cursors traits for a list.
--  These cursor traits cannot be instantiated in Generic_Lists itself,
--  since the List type is frozen too late.

pragma Ada_2012;
with Conts.Cursors;
with Conts.Lists.Generics;

generic
   with package Lists is new Conts.Lists.Generics (<>);
package Conts.Lists.Cursors with SPARK_Mode is
   package Constant_Bidirectional is
      new Conts.Cursors.Constant_Bidirectional_Traits
         (Container_Type => Lists.List'Class,
          Cursor_Type    => Lists.Cursor,
          Returned_Type  => Lists.Nodes.Elements.Returned,
          First          => Lists.First,
          Next           => Lists.Next,
          Has_Element    => Lists.Has_Element,
          Element        => Lists.Element,
          Previous       => Lists.Previous);
   package Constant_Forward renames Constant_Bidirectional.Constant_Forward;
end Conts.Lists.Cursors;
