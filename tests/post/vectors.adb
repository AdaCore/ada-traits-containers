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
with Conts.Vectors.Indefinite_Unbounded_SPARK;
with Conts.Vectors.Definite_Bounded;
procedure Vectors is

   procedure Test_Bounded;
   --  Test the bounded definite case

   procedure Test_Unbounded;
   --  Test the unbounded indefinite case

   procedure Test_Bounded is
      package My_Vectors is new
        Conts.Vectors.Definite_Bounded
          (Index_Type   => Positive,
           Element_Type => Integer);
      use My_Vectors;

      V, S : Vector (200);
      C : Cursor;
   begin
      C := V.First;

      for I in 1 .. 10 loop
         V.Append (I);
      end loop;

      for I in 5 .. 10 loop
         V.Insert (10, I);
      end loop;

      V.Reserve_Capacity (20);

      V.Shrink_To_Fit;

      V.Resize (20, 0);

      pragma Assert (V.Length = 20);

      V.Resize (10, 100);

      pragma Assert (V.Last_Element = 10);

      V.Replace_Element (3, 42);

      V.Swap (1, 3);

      V.Delete (1);

      pragma Assert (My_Vectors.Vectors.As_Element (V, 1) = 2);

      V.Delete_Last;

      C := V.Next (V.First);
      C := V.Previous (C);

      C := V.Previous (V.First);

      S.Assign (V);

      V.Clear;

      V.Append (10, 40);

      V.Delete (V.Last - 1, 40);

      V.Insert (15, 10, 40);

      V.Delete (15, 40);

      for E of S loop
         pragma Assert (E in 1 .. 10);
      end loop;
   end Test_Bounded;

   procedure Test_Unbounded is
      package My_Vectors is new
        Conts.Vectors.Indefinite_Unbounded_SPARK
          (Index_Type   => Positive,
           Element_Type => Integer);
      use My_Vectors;

      V : Vector;
      S : Vector;
      C : Cursor;
   begin
      C := V.First;

      for I in 1 .. 10 loop
         V.Append (I);
      end loop;

      for I in 5 .. 10 loop
         V.Insert (10, I);
      end loop;

      V.Reserve_Capacity (20);

      V.Shrink_To_Fit;

      V.Resize (20, 0);

      pragma Assert (V.Length = 20);

      V.Resize (10, 100);

      pragma Assert (V.Last_Element = 10);

      V.Replace_Element (3, 42);

      V.Swap (1, 3);

      V.Delete (1);

      pragma Assert (My_Vectors.Vectors.As_Element (V, 1) = 2);

      V.Delete_Last;

      C := V.Next (V.First);
      C := V.Previous (C);

      C := V.Previous (V.First);

      S.Assign (V);

      V.Clear;

      V.Append (10, 40);

      V.Delete (V.Last - 1, 40);

      V.Insert (15, 10, 40);

      V.Delete (15, 40);

      for E of S loop
         pragma Assert (E in 1 .. 10);
      end loop;
   end Test_Unbounded;
begin
   Test_Unbounded;
   Test_Bounded;
end Vectors;
