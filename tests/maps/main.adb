--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with System.Assertions;    use System.Assertions;
with Ada.Finalization;
with Conts.Maps.Indef_Def_Unbounded;
with Ada.Strings.Hash;
with Ada.Text_IO;          use Ada.Text_IO;

procedure Main is

   package Maps is new Conts.Maps.Indef_Def_Unbounded
     (Key_Type            => String,
      Element_Type        => Integer,
      Container_Base_Type => Ada.Finalization.Controlled,
      Hash                => Ada.Strings.Hash);

   M : Maps.Map;

begin
   --  Check looking for an element in an empty table
   begin
      Put_Line ("Getting element from empty table " & M.Get ("one")'Img);
   exception
      when Constraint_Error | Assert_Failure =>
         null;
   end;

   M.Set ("one", 1);
   M.Set ("two", 2);
   M.Set ("three", 3);
   M.Set ("four", 4);
   M.Set ("five", 5);
   M.Set ("six", 6);
   M.Set ("seven", 7);
   M.Set ("height", 8);
   M.Set ("nine", 9);
   M.Set ("ten", 10);

   Put_Line ("Value for one is " & M.Get ("one")'Img);
   Put_Line ("Value for four is " & M ("four")'Img);

   M.Delete ("one");
   M.Delete ("two");
   M.Delete ("three");
   M.Delete ("four");
   M.Delete ("five");
   M.Delete ("six");

   Put_Line ("Value for seven is " & M ("seven")'Img);

   begin
      Put_Line ("Value for three is " & M ("three")'Img);
      Put_Line ("Error, three should have been removed");
   exception
      when Constraint_Error | Assert_Failure =>
         null;   --  expected
   end;
end Main;
