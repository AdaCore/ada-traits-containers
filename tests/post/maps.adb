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

pragma Ignore_Pragma (Assertion_Policy);

with Conts; use Conts;
with Conts.Maps.Indef_Indef_Unbounded_SPARK;
with Ada.Strings.Hash;

procedure Maps is
   package My_Maps is new
     Conts.Maps.Indef_Indef_Unbounded_SPARK
       (Key_Type     => String,
        Element_Type => Integer,
        Hash         => Ada.Strings.Hash);
   use My_Maps;

   M, S : My_Maps.Map;
   C : Cursor := My_Maps.Impl.No_Element;
begin
   M.Resize (2);

   C := M.First;

   for I in 1 .. 10 loop
      M.Set (Integer'Image (I), I);
   end loop;

   M.Resize (38);
   M.Resize (11);  --  Resize to bigger than number of elements
   M.Resize (2);   --  Resize to smaller size

   M.Set (M.As_Key (M.First), 0);

   M.Delete (Integer'Image (1));

   M.Delete (Integer'Image (1));

   S.Assign (M);

   C := M.First;

   M.Clear;

end Maps;
