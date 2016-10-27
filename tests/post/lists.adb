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
with Conts.Lists.Definite_Bounded;
with Conts.Lists.Indefinite_Unbounded_SPARK;
procedure Lists is

   procedure Test_Bounded;
   --  Test the bounded definite case

   procedure Test_Unbounded;
   --  Test the unbounded indefinite case

   procedure Test_Bounded is
      package My_Bounded_Lists is new
        Conts.Lists.Definite_Bounded
          (Element_Type => Integer);
      use My_Bounded_Lists;

      S, L : My_Bounded_Lists.Lists.List (30);
      C : Cursor := My_Bounded_Lists.Lists.No_Element;
      N : Natural := 0;
   begin
      C := L.First;
      C := L.Last;

      for I in 1 .. 10 loop
         L.Append (0); -- To be removed when insert is implemented
         L.Append (I);
      end loop;

      C := L.Last;
      while L.Has_Element (C) loop
         --        L.Insert (C, 0);
         C := L.Previous (L.Previous (C));
      end loop;

      pragma Assert (L.Length = 20);

      for C in L loop
         if My_Bounded_Lists.Lists.As_Element (L, C) = 0 then
            --           L.Replace_Element (C, 1);
            N := N + 1;
         end if;
      end loop;

      C := L.Previous (L.First);
      C := L.Next (L.Last);

      S.Assign (L);

      L.Clear;
   end Test_Bounded;

   procedure Test_Unbounded is
      package My_Lists is new
        Conts.Lists.Indefinite_Unbounded_SPARK
          (Element_Type => Integer);
      use My_Lists;

      L, S : My_Lists.Lists.List;
      C : Cursor := My_Lists.Lists.No_Element;
      N : Natural := 0;
   begin
      C := L.First;
      C := L.Last;

      for I in 1 .. 10 loop
         L.Append (0); -- To be removed when insert is implemented
         L.Append (I);
      end loop;

      C := L.Last;
      while L.Has_Element (C) loop
         --        L.Insert (C, 0);
         C := L.Previous (L.Previous (C));
      end loop;

      pragma Assert (L.Length = 20);

      for C in L loop
         if My_Lists.Lists.As_Element (L, C) = 0 then
            --           L.Replace_Element (C, 1);
            N := N + 1;
         end if;
      end loop;

      C := L.Previous (L.First);
      C := L.Next (L.Last);

      S.Assign (L);

      L.Clear;
   end Test_Unbounded;
begin
   Test_Unbounded;
   Test_Bounded;
end Lists;
