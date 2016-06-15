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

with Ada.Text_IO;   use Ada.Text_IO;
with Conts;         use Conts;

procedure Main is
   type My_Subtype is new Integer range 10 .. 20;
   package Rand is new Conts.Default_Random (My_Subtype);

   Gen : Rand.Traits.Generator;

   Total : Long_Float := 0.0;
   Val  : My_Subtype;

   Items_Count : constant := 200_000;
   Mean : Long_Float;

begin
   Rand.Reset (Gen);

   for Count in 1 .. Items_Count loop
      Rand.Traits.Rand (Gen, Val);
      Total := Total + Long_Float (Val);
   end loop;

   Mean := Total / Long_Float (Items_Count);
   if abs (Mean - 15.0) > 0.2 then
      Put_Line ("Standard random numbers");
      Put_Line ("Mean =" & Long_Float'Image (Mean));
   end if;

   declare
      procedure Ranged is new Conts.Ranged_Random (Rand.Traits, 12, 14);
   begin
      Total := 0.0;
      for Count in 1 .. Items_Count loop
         Ranged (Gen, Val);
         Total := Total + Long_Float (Val);
      end loop;

      Mean := Total / Long_Float (Items_Count);
      if abs (Mean - 13.0) > 0.2 then
         Put_Line ("Ranged random numbers");
         Put_Line ("Mean =" & Long_Float'Image (Mean));
      end if;
   end;

end Main;
