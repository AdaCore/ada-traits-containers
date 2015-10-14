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

package Conts.Dicts.Strings is

   type Stored_String is private;

   function Convert_From (E : String) return Stored_String;

   function Convert_To (E : Stored_String) return String;

   function Get_Reference (E : Stored_String) return String;
   --  how to implement this in this context

   procedure Release (E : in out Stored_String);

   function Hash (Str : String) return Unsigned_32;

private

   type String_Access is access String;
   type String_Storage_Kind is (Short_String, Long_String);
   Short_String_Size : constant := 12;

   type Stored_String (Kind : String_Storage_Kind := Short_String) is record
      case Kind is
         when Short_String =>
            Short_Value  : String (1 .. Short_String_Size);
            Short_Length : Integer;
         when Long_String =>
            Long_Value   : String_Access := null;
      end case;
   end record;
end Conts.Dicts.Strings;
