--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

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
