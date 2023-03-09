--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

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
