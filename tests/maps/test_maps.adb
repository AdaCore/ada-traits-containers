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
with Conts.Elements.Definite;
with Conts.Elements.Indefinite_Ref;
with Conts.Maps;
with Ada.Strings.Hash;
with Ada.Text_IO;          use Ada.Text_IO;

procedure Test_Maps is

   package Strings is
      new Conts.Elements.Indefinite_Ref (String, Pool => Conts.Global_Pool);
   package Values  is new Conts.Elements.Definite (Integer);

   function "="
     (Left : String; Right : Strings.Traits.Stored) return Boolean
      is (Left = Right.all) with Inline;

   package Maps is new Conts.Maps.Maps
      (Keys      => Strings.Traits,
       Elements  => Values.Traits,
       Hash      => Ada.Strings.Hash,
       "="       => "=",
       Probing   => Conts.Maps.Perturbation_Probing,
       Pool      => Conts.Global_Pool,
       Base_Type => Ada.Finalization.Controlled);

   M : Maps.Map;

begin
   --  Check looking for an element in an empty table
   begin
      Put_Line ("Getting element from empty table " & M.Get ("one")'Img);
   exception
      when Constraint_Error =>
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
   Put_Line ("Value for four is " & M.Get ("four")'Img);

   M.Delete ("one");
   M.Delete ("two");
   M.Delete ("three");
   M.Delete ("four");
   M.Delete ("five");
   M.Delete ("six");

   Put_Line ("Value for seven is " & M.Get ("seven")'Img);

   begin
      Put_Line ("Value for three is " & M.Get ("three")'Img);
      Put_Line ("Error, three should have been removed");
   exception
      when Constraint_Error =>
         null;   --  expected
   end;
end Test_Maps;
