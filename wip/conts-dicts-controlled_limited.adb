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

package body Conts.Dicts.Controlled_Limited is

   package body Generic_Limited_Dicts is

      procedure Allocate (Self : in out Dict; Min_Size : Index_Type := 0) is
      begin
         Dicts.Allocate (Self.D);
      end Allocate;

      function Element (Self : Dict; Key : Key_Type) return Cursor is
      begin
         return Dicts.Element (Self.D, Key);
      end Element;

      function Has_Element (Self : Dict; Position : Cursor) return Boolean is
      begin
         return Dicts.Has_Element (Self.D, Position);
      end Has_Element;

      procedure Release (Self : in out Dict) is
      begin
         Dicts.Release (Self.D);
      end Release;

      procedure Set (Self     : in out Dict;
                     Position : Cursor;
                     Key      : Key_Type;
                     Value    : Value_Type;
                     Enable_Realloc : Boolean := True) is
      begin
         Dicts.Set (Self.D, Position, Key, Value, Enable_Realloc);
      end Set;

      function Value (Self : Dict; Position : Cursor) return Value_Type is
      begin
         return Dicts.Value (Self.D, Position);
      end Value;

   end Generic_Limited_Dicts;

end Conts.Dicts.Controlled_Limited;
