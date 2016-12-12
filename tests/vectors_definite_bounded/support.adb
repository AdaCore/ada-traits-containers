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
with System.Assertions; use System.Assertions;
with Asserts;           use Asserts;
with Ada.Text_IO;       use Ada.Text_IO;

package body Support is

   use Testsuite_Asserts, Asserts.Integers, Asserts.Booleans;
   use Asserts.Counts;

   ----------
   -- Test --
   ----------

   procedure Test (V1 : in out Vectors.Vector) is
      use Vectors;

      procedure Dump (V : Vectors.Vector; Msg : String);
      --  Dump the contents of V on stdout

      ----------
      -- Dump --
      ----------

      procedure Dump (V : Vectors.Vector; Msg : String) is
      begin
         Put (Msg);
         Put (": [");
         for E of V loop
            Put (Image (E));
            Put (", ");
         end loop;
         Put_Line ("]");
      end Dump;

      E : Integer;
   begin
      --------------------
      --  Empty vectors --
      --------------------

      Assert_Equal (V1.Length, 0, "length of empty vector");
      Assert_Equal (V1.Is_Empty, True, "empty vector is empty ?");
      Assert_Equal (V1.Last, Index_Type'First - 1, "last of empty vector");

      V1.Clear;   --  Should be safe

      begin
         E := Elements.To_Elem (V1.Last_Element);
         Assert_Failed ("last_element of empty vector");
      exception
         when Assert_Failure =>
            null;  --  expected
      end;

      Dump (V1, "element loop");
      for C in V1 loop
         Put_Line ("Empty vector, cursor loop =>" & Image (V1.Element (C)));
      end loop;

      ------------------------------
      -- Vectors with one element --
      ------------------------------

      V1.Append (1);
      Assert (V1.Length, 1, "Length of vector with one element");
      Assert (V1.Is_Empty, False, "vector of one element is empty ?");
      Assert (V1.Last, Index_Type'First, "last of one-element-vector");
      Assert (Elements.To_Elem (V1.Last_Element), 1,
              "last element of one-element-vector");

      Dump (V1, "one-element vector");
      for C in V1 loop
         Put_Line ("one-element vector, cursor loop =>"
                   & Image (V1.Element (C)));
      end loop;

      Assert (V1.Has_Element (1), True, "has element on valid index");
      Assert (V1.Has_Element (1000), False, "has element on invalid index");
      Assert (V1.Has_Element (0), False, "has element on invalid index");

      -----------------------------------------
      -- Removing one element, back to empty --
      -----------------------------------------

      V1.Delete_Last;
      Assert (V1.Length, 0, "Length after removing single element");
      Assert (V1.Is_Empty, True, "After removing single element, is empty ?");

      ------------------
      -- Large vector --
      ------------------

      for E2 in 1 .. 10 loop
         V1.Append (E2);
      end loop;

      Assert (V1.Length, 10);
      Assert (V1.Is_Empty, False);
      Assert (V1.Last, Index_Type'First + 9, "last of large vector");
      Assert (Elements.To_Elem (V1.Last_Element), 10,
              "last element of large vector");

      Dump (V1, "after append");

      -----------------------------
      -- Deleting a few elements --
      -----------------------------

      V1.Delete (1);   --  removes "1", first element
      V1.Delete (3);   --  removes "4"
      V1.Delete (4);   --  removes "6"
      V1.Delete (7);   --  removes "10", last element
      Assert (V1.Length, 6, "length after delete");
      Assert (V1.Is_Empty, False, "is_empty after delete");
      Assert (Elements.To_Elem (V1.Last_Element), 9,
              "last element after delete");
      Dump (V1, "after delete");

      begin
         V1.Delete (10);  --  invalid
         Assert_Failed ("should not delete element at invalid index");
      exception
         when Assert_Failure =>
            null;   --  expected
      end;

      V1.Delete (5, Count => 10);
      Assert (V1.Length, 4, "after delete 10 elements at end");
      Dump (V1, "delete 10 elements at end");

      V1.Append (8);
      V1.Append (9);

      -----------------------
      -- Swapping elements --
      -----------------------

      V1.Swap (1, 6);
      Assert (V1.Length, 6, "length after swap");
      Assert (V1.Is_Empty, False, "is_empty after swap");
      Assert (Elements.To_Elem (V1.Last_Element), 2,
              "last_element after swap");
      Dump (V1, "after swap");

      V1.Swap (1, 1);    --  swapping same element
      Assert (V1.Length, 6, "length after swap same element");
      Assert (V1.Is_Empty, False, "is_empty after swap same element");
      Assert (Elements.To_Elem (V1.Last_Element), 2,
              "last_element after swap same element");
      Dump (V1, "after swap same element");

      begin
         V1.Swap (10000, 1);     --  invalid first index
         Assert_Failed ("Swap with invalid first index");
      exception
         when Assert_Failure =>
            null;   --  expected
      end;

      begin
         V1.Swap (1, 10000);     --  invalid second index
         Assert_Failed ("Swap with invalid second index");
      exception
         when Assert_Failure =>
            null;   --  expected
      end;

      ------------------------
      -- Replacing elements --
      ------------------------

      V1.Replace_Element (1, 100);
      V1.Replace_Element (6, 600);
      Assert (V1.Length, 6, "length after replace");
      Assert (V1.Is_Empty, False, "is_empty after replace");
      Assert (Elements.To_Elem (V1.Last_Element), 600,
              "last_element after replace");
      Dump (V1, "after replace");

      begin
         V1.Replace_Element (10000, 20);
         Assert_Failed ("Replace at invalid index");
      exception
         when Assert_Failure =>
            null;   --  expected
      end;

      ------------------------------------------
      -- Clearing large vector, back to empty --
      ------------------------------------------

      V1.Clear;
      Assert (V1.Length, 0, "length after clear");
      Assert (V1.Is_Empty, True, "is_empty, after clear");

      -----------------------
      -- Resizing a vector --
      -----------------------

      --  resize from empty
      V1.Resize (Length => 4, Element => 1);
      Assert (V1.Length, 4, "length after resize");
      Assert (V1.Is_Empty, False, "is_empty after resize");
      Assert (Elements.To_Elem (V1.Last_Element), 1,
              "last_element after resize");
      Dump (V1, "after resize");

      --  resize to bigger size
      V1.Resize (Length => 6, Element => 2);
      Assert (V1.Length, 6, "length after resize2");
      Assert (Elements.To_Elem (V1.Last_Element), 2,
              "last_element after resize2");
      Dump (V1, "after resize (2)");

      --  resize to smaller size
      V1.Resize (Length => 3, Element => 3);
      Assert (V1.Length, 3, "length after resize3");
      Assert (Elements.To_Elem (V1.Last_Element), 1,
              "last_element after resize3");
      Dump (V1, "after resize (3)");

      --  resize to 0
      V1.Resize (Length => 0, Element => 3);
      Assert (V1.Length, 0, "length after resize4");
      Dump (V1, "after resize (4)");

      ---------------
      -- Shrinking --
      ---------------

      V1.Clear;
      for E in 1 .. 20 loop
         V1.Append (E);
      end loop;

      V1.Shrink_To_Fit;
      Assert (V1.Length, 20, "length after shrink");
      Assert (Elements.To_Elem (V1.Last_Element), 20,
              "last_element after shrink");

      ------------
      -- Insert --
      ------------

      V1.Clear;

      V1.Insert (Before => No_Element, Element => 1, Count => 3);
      Assert (V1.Length, 3, "length after insert in empty");
      Dump (V1, "after insert in empty");

      V1.Insert (Before => No_Element, Element => 4, Count => 2);
      Assert (V1.Length, 5, "length after insert at end");
      Dump (V1, "after insert at end");

      V1.Insert (Before => 1, Element => 2, Count => 2);
      Assert (V1.Length, 7, "length after insert at head");
      Dump (V1, "after insert at head");

      begin
         V1.Insert (Before => 60, Element => 3);
         Assert_Failed ("Can't insert at invalid location");
      exception
         when Assert_Failure =>
            null;
      end;
   end Test;

end Support;
