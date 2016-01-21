------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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

package body Conts.Vectors is

   --------------
   -- Grow_1_5 --
   --------------

   function Grow_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type is
   begin
      if Current_Size < Min_Expected then
         return Count_Type'Max
           (Min_Expected, Count_Type'Max (4, Current_Size * 3 / 2));
      else
         return Current_Size;
      end if;
   end Grow_1_5;

   ----------------
   -- Shrink_1_5 --
   ----------------

   function Shrink_1_5
     (Current_Size, Min_Expected : Count_Type) return Count_Type
   is
      Tmp, Result : Count_Type;
   begin
      if Min_Expected = 0 then
         return 0;
      else
         Tmp    := Count_Type (Current_Size * 2 / 3);
         Result := Current_Size;
         while Tmp >= Min_Expected loop
            Result := Tmp;
            Tmp := Count_Type (Result * 2 / 3);
         end loop;
         return Result;
      end if;
   end Shrink_1_5;

end Conts.Vectors;
