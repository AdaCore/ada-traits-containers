--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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

      S, L : My_Bounded_Lists.Lists.List (100);
      C : Cursor := My_Bounded_Lists.Lists.No_Element;
      N : Natural := 0;
   begin
      C := L.First;
      C := L.Last;

      for I in 1 .. 10 loop
         L.Append (I);
      end loop;

      C := L.Last;
      while L.Has_Element (C) loop
         L.Insert (C, 0);
         C := L.Previous (L.Previous (C));
      end loop;

      pragma Assert (L.Length = 20);

      for C in L loop
         if My_Bounded_Lists.Lists.As_Element (L, C) = 0 then
            L.Replace_Element (C, 1);
            N := N + 1;
         end if;
      end loop;

      C := L.Previous (L.First);
      C := L.Next (L.Last);

      S.Assign (L);

      L.Append (10, 40);

      declare
         Position : Cursor := L.Previous (L.Last);
      begin
         L.Delete (Position, 40);

         Position := L.Previous (L.Last);

         L.Insert (Position, 10, 40);

         L.Delete (Position, 40);
      end;

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
         L.Append (I);
      end loop;

      C := L.Last;
      while L.Has_Element (C) loop
         L.Insert (C, 0);
         C := L.Previous (L.Previous (C));
      end loop;

      pragma Assert (L.Length = 20);

      for C in L loop
         if My_Lists.Lists.As_Element (L, C) = 0 then
            L.Replace_Element (C, 1);
            N := N + 1;
         end if;
      end loop;

      C := L.Previous (L.First);
      C := L.Next (L.Last);

      S.Assign (L);

      L.Append (10, 40);

      declare
         Position : Cursor := L.Previous (L.Last);
      begin
         L.Delete (Position, 40);

         Position := L.Previous (L.Last);

         L.Insert (Position, 10, 40);

         L.Delete (Position, 40);
      end;

      L.Clear;
   end Test_Unbounded;
begin
   Test_Unbounded;
   Test_Bounded;
end Lists;
