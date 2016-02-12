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

procedure Test_Random is
   type My_Subtype is new Integer range 10 .. 20;
   package Rand is new Conts.Default_Random (My_Subtype);

   Gen : Rand.Traits.Generator;

   Total : Long_Float := 0.0;
   Val  : My_Subtype;

   Items_Count : constant := 20;

begin
   Rand.Reset (Gen);

   Put_Line ("Standard random numbers");
   for Count in 1 .. Items_Count loop
      Val := Rand.Traits.Rand (Gen);
      Total := Total + Long_Float (Val);
      Put_Line (Val'Img);
   end loop;
   Put_Line ("Mean =" & Long_Float'Image (Total / Long_Float (Items_Count)));

   declare
      function Ranged is new Conts.Ranged_Random (Rand.Traits, 12, 14);
   begin
      Put_Line ("Ranged random numbers");
      Total := 0.0;
      for Count in 1 .. Items_Count loop
         Val := Ranged (Gen);
         Total := Total + Long_Float (Val);
         Put_Line (Val'Img);
      end loop;
      Put_Line
         ("Mean =" & Long_Float'Image (Total / Long_Float (Items_Count)));
   end;

end Test_Random;
