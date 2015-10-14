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

--  This package provides signature packages that describe how to iterate over
--  containers, or to point to containers objects.

pragma Ada_2012;

package Conts.Cursors with SPARK_Mode is

   -----------------------------
   -- Constant_Forward_Traits --
   -----------------------------

   generic
      type Container (<>) is limited private;
      type Cursor is private;
      type Element_Type (<>) is private;
      with function First (Self : Container) return Cursor is <>;
      with function Element (Self : Container; Pos : Cursor)
         return Element_Type is <>;
      with function Has_Element (Self : Container; Pos : Cursor)
         return Boolean is <>;
      with function Next (Self : Container; Pos : Cursor) return Cursor is <>;
   package Constant_Forward_Traits is

      --  A package that describes how to use forward cursors.  Each container
      --  for which this is applicable provides an instance of this package,
      --  and algorithms should take this package as a generic parameter.

   end Constant_Forward_Traits;

   -----------------------------------
   -- Constant_Bidirectional_Traits --
   -----------------------------------

   generic
      type Container (<>) is limited private;
      type Cursor is private;
      type Element_Type (<>) is private;
      with function First (Self : Container) return Cursor is <>;
      with function Element (Self : Container; Pos : Cursor)
         return Element_Type is <>;
      with function Has_Element (Self : Container; Pos : Cursor)
         return Boolean is <>;
      with function Next (Self : Container; Pos : Cursor) return Cursor is <>;
      with function Previous (Self : Container; Pos : Cursor)
         return Cursor is <>;

   package Constant_Bidirectional_Traits is

      --  A bidirectional cursor is also a forward cursor
      package Constant_Forward is new Constant_Forward_Traits
         (Container, Cursor, Element_Type);

   end Constant_Bidirectional_Traits;

end Conts.Cursors;
