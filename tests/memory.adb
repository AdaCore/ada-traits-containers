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

package body Memory is

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Current := (Total_Allocated => 0,
                  Allocs => 0,
                  Frees => 0,
                  Reallocs => 0);
   end Reset;

   ---------
   -- "-" --
   ---------

   function "-" (M1, M2 : Mem_Info) return Mem_Info is
   begin
      return (Total_Allocated => M1.Total_Allocated - M2.Total_Allocated,
              Allocs          => M1.Allocs - M2.Allocs,
              Frees           => M1.Frees - M2.Frees,
              Reallocs        => M1.Reallocs - M2.Reallocs);
   end "-";

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin
      Paused := True;
   end Pause;

   -------------
   -- Unpause --
   -------------

   procedure Unpause is
   begin
      Paused := False;
   end Unpause;
end Memory;
