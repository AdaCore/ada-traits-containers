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

package body Conts.Algorithms is

   --------------
   -- Count_If --
   --------------

   function Count_If
      (Self      : Cursors.Container;
       Predicate : access function (E : Cursors.Return_Type) return Boolean)
      return Natural
   is
      C : Cursors.Cursor := Cursors.First (Self);
      Count : Natural := 0;
   begin
      while Cursors.Has_Element (Self, C) loop
         if Predicate (Cursors.Element (Self, C)) then
            Count := Count + 1;
         end if;
         C := Cursors.Next (Self, C);
      end loop;
      return Count;
   end Count_If;

end Conts.Algorithms;
