------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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
with Ada.Text_IO;        use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Memory;
with System;

package body Report is

   type Output_Access is access all Output'Class;
   pragma No_Strict_Aliasing (Output_Access);
   function To_Output is new Ada.Unchecked_Conversion
      (System.Address, Output_Access);

   procedure Start_Container_Test
      (Self : System.Address;
       Base, Elements, Nodes, Container, E_Type : chars_ptr)
       with Export, Convention => C, External_Name => "start_container_test";
   procedure End_Container_Test (Self : System.Address)
      with Export, Convention => C, External_Name => "end_container_test";
   procedure Start_Test (Self : System.Address; Name : chars_ptr)
      with Export, Convention => C, External_Name => "start_test";
   procedure End_Test (Self : System.Address)
      with Export, Convention => C, External_Name => "end_test";

   --------------------------
   -- Start_Container_Test --
   --------------------------

   procedure Start_Container_Test
      (Self       : not null access Output'Class;
       Base       : String;
       Elements   : String;
       Nodes      : String;
       Container  : String;
       E_Type     : String)
   is
   begin
      Memory.Pause;

      if Self.Global_Result = JSON_Null then
         Self.Global_Result := Create_Object;
      end if;

      Self.End_Container_Test;   --  In case one was started
      Self.Container_Test := GNATCOLL.JSON.Create_Object;
      Self.Container_Test.Set_Field ("base", Base);
      Self.Container_Test.Set_Field ("elements", Elements);
      Self.Container_Test.Set_Field ("nodes", Nodes);
      Self.Container_Test.Set_Field ("container", Container);
      Self.Container_Test.Set_Field ("elem_type", E_Type);

      Self.Tests_In_Container := Create_Object;
      Self.Container_Test.Set_Field ("tests", Self.Tests_In_Container);

      Memory.Reset;
      Memory.Unpause;
   end Start_Container_Test;

   ------------------------
   -- End_Container_Test --
   ------------------------

   procedure End_Container_Test (Self : not null access Output'Class) is
   begin
      Memory.Pause;
      Self.End_Test;  --  In case one was started

      if Self.Container_Test /= JSON_Null then
         --  Allocations include the ones done for JSON results
         Self.Container_Test.Set_Field ("allocated", Memory.Live);
         Self.Container_Test.Set_Field ("allocs", Memory.Allocs);
         Self.Container_Test.Set_Field ("reallocs", Memory.Reallocs);
         Self.Container_Test.Set_Field ("frees", Memory.Frees);

         Append (Self.All_Tests, Self.Container_Test);
         Self.Container_Test := JSON_Null;
      end if;
      Memory.Unpause;
   end End_Container_Test;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test
      (Self    : not null access Output'Class;
       Name    : String;
       Comment : String := "") is
   begin
      Memory.Pause;
      Self.End_Test;  --  In case one was started
      Self.Current_Test := GNATCOLL.JSON.Create_Object;
      Self.Tests_In_Container.Set_Field (Name, Self.Current_Test);

      if Comment /= "" then
         Self.Current_Test.Set_Field ("comment", Comment);
      end if;

      Memory.Unpause;
      Self.Start_Time := Clock;
   end Start_Test;

   --------------
   -- End_Test --
   --------------

   procedure End_Test (Self : not null access Output'Class) is
      D : constant Duration := Clock - Self.Start_Time;
   begin
      if Self.Current_Test /= JSON_Null then
         Memory.Pause;
         Self.Current_Test.Set_Field ("duration", Float (D));
         Self.Current_Test := JSON_Null;
         Memory.Unpause;
      end if;
   end End_Test;

   -------------
   -- Display --
   -------------

   procedure Display (Self : not null access Output'Class) is
      F : File_Type;
   begin
      Self.Global_Result.Set_Field ("tests", Self.All_Tests);

      Create (F, Out_File, "data.js");
      Put (F, "var data = ");
      Put (F, GNATCOLL.JSON.Write (Self.Global_Result, Compact => False));
      Put (F, ";");
      Close (F);
   end Display;

   --------------------------
   -- Start_Container_Test --
   --------------------------

   procedure Start_Container_Test
      (Self : System.Address;
       Base, Elements, Nodes, Container, E_Type : chars_ptr) is
   begin
      Start_Container_Test
         (To_Output (Self), Value (Base), Value (Elements), Value (Nodes),
          Value (Container), Value (E_Type));
   end Start_Container_Test;

   ------------------------
   -- End_Container_Test --
   ------------------------

   procedure End_Container_Test (Self : System.Address) is
   begin
      End_Container_Test (To_Output (Self));
   end End_Container_Test;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test (Self : System.Address; Name : chars_ptr) is
   begin
      Start_Test (To_Output (Self), Value (Name));
   end Start_Test;

   --------------
   -- End_Test --
   --------------

   procedure End_Test (Self : System.Address) is
   begin
      End_Test (To_Output (Self));
   end End_Test;

end Report;
