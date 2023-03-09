--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Asserts;              use Asserts;
with Ada.Text_IO;          use Ada.Text_IO;
with System.Assertions;    use System.Assertions;

package body Support is

   use Lists;
   use Testsuite_Asserts;
   use Asserts.Booleans;
   use Asserts.Counts;

   procedure Dump (L : Lists.List; Msg : String);
   --  Display the contents of the list on stdout

   ----------
   -- Dump --
   ----------

   procedure Dump (L : Lists.List; Msg : String) is
   begin
      Put (Msg);
      Put ("=[");
      for E of L loop
         Put (Image (E));
         Put (',');
      end loop;
      Put_Line ("]");
   end Dump;

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
      Dump (L1, "element loop");
      for C in L1 loop
         Put_Line ("list, cursor loop =>" & Image (L1.Element (C)));
      end loop;

      ------------
      -- Assign --
      ------------

      L2.Clear;
      L2.Assign (Source => L1);
      Assert (L2.Length, L1.Length, "lengths after assign");
      Dump (L2, "assigned list, element loop");

      ------------
      -- Insert --
      ------------

      L2.Clear;
      L2.Insert (No_Element, 1, Count => 3);
      Assert (L2.Length, 3, "length after inserting 3 elements at tail");
      Dump (L2, "after insert in empty");

      L2.Insert (No_Element, 2, Count => 2);
      Assert (L2.Length, 5, "length after inserting 2 elements at tail");
      Dump (L2, "after insert");

      L2.Insert (L2.First, 3);
      Assert (L2.Length, 6, "length after inserting 1 elements at head");
      Dump (L2, "after insert at head");

      L2.Insert (L2.Next (L2.First), 4, Count => 2);
      Assert (L2.Length, 8, "length after inserting 2 elements in middle");
      Dump (L2, "after insert in middle");

      --  ??? What happens if we pass a cursor in the wrong list ?
      --  We currently get a contract case error, but we should be getting
      --  a better error message.
      --  L2.Insert (L1.First, 5);

      ---------------------
      -- Replace_Element --
      ---------------------

      L2.Replace_Element (L2.First, 10);
      Dump (L2, "after replace_element");

      begin
         L2.Replace_Element (No_Element, 11);
         Assert_Failed
            ("Expected precondition failure when replacing no_element");
      exception
         when Assert_Failure =>
            null;
      end;

      ------------
      -- Delete --
      ------------

      declare
         C : Cursor := L2.First;
         C2 : constant Cursor := L2.Next (C);
      begin
         L2.Delete (C, Count => 1);
         Assert (L2.Length, 7, "length after delete head");
         if C /= C2 then
            Assert_Failed ("Cursor should have moved to second element");
         end if;
         Dump (L2, "after delete head");
      end;

      declare
         C : Cursor := L2.First;
         C2 : constant Cursor := L2.Next (L2.Next (C));
      begin
         L2.Delete (C, Count => 2);
         Assert (L2.Length, 5, "length after delete 2 at head");
         if C /= C2 then
            Assert_Failed ("Cursor should have moved to third element");
         end if;
         Dump (L2, "after delete 2 at head");
      end;

      declare
         C : Cursor := L2.Last;
      begin
         L2.Delete (C, Count => 20);
         Assert (L2.Length, 4, "length after delete tail");
         if C /= No_Element then
            Assert_Failed ("Cursor should be no_element");
         end if;
         Dump (L2, "after delete at tail");
      end;
   end Test;

end Support;
