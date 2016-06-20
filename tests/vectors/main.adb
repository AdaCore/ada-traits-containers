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
with Ada.Finalization;
with System.Assertions; use System.Assertions;
with Conts.Vectors.Definite_Unbounded;
with Asserts;           use Asserts;
with Ada.Text_IO;       use Ada.Text_IO;

procedure Main is
   use Testsuite_Asserts, Asserts.Integers, Asserts.Booleans;
   use Asserts.Counts;

   subtype Index_Type is Positive;

   package Int_Vecs is new Conts.Vectors.Definite_Unbounded
      (Index_Type, Integer, Ada.Finalization.Controlled);
   use Int_Vecs;

   V1 : Vector;
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
      E := V1.Last_Element;
      Assert_Failed ("last_element of empty vector");
   exception
      when Assert_Failure =>
         null;  --  expected
   end;

   for E2 of V1 loop
      Put_Line ("Empty vector, element loop =>" & E2'Img);
   end loop;
   for C in V1 loop
      Put_Line ("Empty vector, cursor loop =>" & V1.Element (C)'Img);
   end loop;

   ------------------------------
   -- Vectors with one element --
   ------------------------------

   V1.Append (1);
   Assert (V1.Length, 1, "Length of vector with one element");
   Assert (V1.Is_Empty, False, "vector of one element is empty ?");
   Assert (V1.Last, Index_Type'First, "last of one-element-vector");
   Assert (V1.Last_Element, 1, "last element of one-element-vector");

   for E2 of V1 loop
      Put_Line ("one-element vector, element loop =>" & E2'Img);
   end loop;
   for C in V1 loop
      Put_Line ("one-element vector, cursor loop =>" & V1.Element (C)'Img);
   end loop;

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
   Assert (V1.Last_Element, 10, "last element of large vector");

   for E2 of V1 loop
      Put_Line ("vector, element loop =>" & E2'Img);
   end loop;
   for C in V1 loop
      Put_Line ("vector, cursor loop =>" & V1.Element (C)'Img);
   end loop;

   -----------------------------
   -- Deleting a few elements --
   -----------------------------

   V1.Delete (1);   --  removes "1", first element
   V1.Delete (3);   --  removes "4"
   V1.Delete (4);   --  removes "6"
   V1.Delete (7);   --  removes "10", last element
   Assert (V1.Length, 6, "length after delete");
   Assert (V1.Is_Empty, False, "is_empty after delete");
   Assert (V1.Last_Element, 9, "last element after delete");

   for E2 of V1 loop
      Put_Line ("vector after delete, element loop =>" & E2'Img);
   end loop;

   begin
      V1.Delete (10);  --  invalid
      Assert_Failed ("should not delete element at invalid index");
   exception
      when Assert_Failure =>
         null;   --  expected
   end;

   -----------------------
   -- Swapping elements --
   -----------------------

   V1.Swap (1, 6);
   Assert (V1.Length, 6, "length after swap");
   Assert (V1.Is_Empty, False, "is_empty after swap");
   Assert (V1.Last_Element, 2, "last_element after swap");
   for E2 of V1 loop
      Put_Line ("vector after swap, element loop =>" & E2'Img);
   end loop;

   V1.Swap (1, 1);    --  swapping same element
   Assert (V1.Length, 6, "length after swap same element");
   Assert (V1.Is_Empty, False, "is_empty after swap same element");
   Assert (V1.Last_Element, 2, "last_element after swap same element");
   for E2 of V1 loop
      Put_Line ("vector after swap same element, element loop =>" & E2'Img);
   end loop;

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
   Assert (V1.Last_Element, 600, "last_element after replace");

   for E2 of V1 loop
      Put_Line ("vector after replace, element loop =>" & E2'Img);
   end loop;

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
   Assert (V1.Last_Element, 1, "last_element after resize");
   for E2 of V1 loop
      Put_Line ("vector after resize, element loop =>" & E2'Img);
   end loop;

   --  resize to bigger size
   V1.Resize (Length => 6, Element => 2);
   Assert (V1.Length, 6, "length after resize2");
   Assert (V1.Last_Element, 2, "last_element after resize2");
   for E2 of V1 loop
      Put_Line ("vector after resize2, element loop =>" & E2'Img);
   end loop;

   --  resize to smaller size
   V1.Resize (Length => 3, Element => 3);
   Assert (V1.Length, 3, "length after resize3");
   Assert (V1.Last_Element, 1, "last_element after resize3");
   for E2 of V1 loop
      Put_Line ("vector after resize3, element loop =>" & E2'Img);
   end loop;

   --  resize to 0
   V1.Resize (Length => 0, Element => 3);
   Assert (V1.Length, 0, "length after resize4");
   for E2 of V1 loop
      Put_Line ("vector after resize4, element loop =>" & E2'Img);
   end loop;

   ---------------
   -- Shrinking --
   ---------------

   V1.Clear;
   for E in 1 .. 20 loop
      V1.Append (E);
   end loop;

   V1.Shrink_To_Fit;
   Assert (V1.Length, 20, "length after shrink");
   Assert (V1.Last_Element, 20, "last_element after shrink");

end Main;
