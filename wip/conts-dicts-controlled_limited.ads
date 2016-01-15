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
with Ada.Finalization; use Ada.Finalization;

package Conts.Dicts.Controlled_Limited is

   generic
      with package Parameters is new Dict_Traits (<>);

   package Generic_Limited_Dicts is

      package Dicts is new Generic_Dicts (Parameters);

      type Dict is limited private;

      subtype Index_Type is Dicts.Dicts.Index_Type;
      subtype Cursor is Dicts.Cursor;
      subtype Value_Type is Dicts.Value_Type;
      subtype Key_Type is Dicts.Key_Type;

      procedure Allocate (Self : in out Dict; Min_Size : Index_Type := 0)
        with Inline;

      procedure Release (Self : in out Dict)
        with Inline;

      function Element (Self : Dict; Key : Key_Type) return Cursor
        with Inline;

      function Has_Element (Self : Dict; Position : Cursor) return Boolean
        with Inline;

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Key      : Key_Type;
                     Value    : Value_Type;
                     Enable_Realloc : Boolean := True)
        with Inline;

      function Value (Self : Dict; Position : Cursor) return Value_Type
        with Inline;
   private
      type Dict is limited record
         D : Dicts.Dict;
      end record;

   end Generic_Limited_Dicts;

   generic
      with package Parameters is new Dict_Traits (<>);

   package Generic_Controlled_Dicts is

      package Dicts is new Generic_Dicts (Parameters);

      type Dict is private;

   private
      type Dict is new Controlled with record
         D : Dicts.Dict;
      end record;
   end Generic_Controlled_Dicts;
end Conts.Dicts.Controlled_Limited;
