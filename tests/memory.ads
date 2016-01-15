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

package Memory is

   type Mem_Info is record
      Total_Allocated : Integer := 0;
      Allocs          : Integer := 0;
      Frees           : Integer := 0;
      Reallocs        : Integer := 0;
   end record;

   Current : Mem_Info;

   function "-" (M1, M2 : Mem_Info) return Mem_Info;
   --  Compute the delta between two memory usage

   Paused : Boolean := False;

   procedure Reset;

   procedure Pause;
   --  Stop counting allocs and frees

   procedure Unpause;
   --  Resume counting
end Memory;
