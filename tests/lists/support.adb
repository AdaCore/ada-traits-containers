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
with Asserts;              use Asserts;
with Ada.Text_IO;          use Ada.Text_IO;

package body Support is

   use Lists;
   use Asserts.Booleans;
   use Asserts.Counts;

   ----------
   -- Test --
   ----------

   procedure Test (L1, L2 : in out Lists.List) is
   begin

      -----------------
      -- Empty lists --
      -----------------

      Assert (L1.Length, 0, "length of an empty list");
      Assert (L1.Is_Empty, True, "empty list is empty ?");
      L1.Clear;  --  should be safe

      for E of L1 loop
         Put_Line ("Empty list, element loop");
      end loop;
      for C in L1 loop
         Put_Line ("Empty list, cursor loop");
      end loop;

      -----------------
      -- Large lists --
      -----------------

      for E in 1 .. 4 loop
         L1.Append (E);
      end loop;

      Assert (L1.Length, 4, "length of list of 10 elements");
      Assert (L1.Is_Empty, False, "list of 10 elements is empty ?");

      for E of L1 loop
         Put_Line ("list, element loop =>" & Image (E));
      end loop;
      for C in L1 loop
         Put_Line ("list, cursor loop =>" & Image (L1.Element (C)));
      end loop;

      ------------
      -- Assign --
      ------------

      L2.Clear;
      L2.Assign (Source => L1);
      Assert (L2.Length, L1.Length, "lengths after assign");
      for E of L2 loop
         Put_Line ("assigned list, element loop =>" & Image (E));
      end loop;

   end Test;

end Support;
