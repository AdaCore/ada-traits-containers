--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Interfaces.C;              use Interfaces.C;
with Interfaces.C.Strings;      use Interfaces.C.Strings;
with Memory;
with System;
with Perf_Support;              use Perf_Support;

package body Report is

   type Output_Access is access all Output'Class;
   pragma No_Strict_Aliasing (Output_Access);
   function To_Output is new Ada.Unchecked_Conversion
      (System.Address, Output_Access);

   procedure Start_Container_Test
      (Self : System.Address;
       Name, Category : chars_ptr;
       Favorite : Integer)
       with Export, Convention => C, External_Name => "start_container_test";
   procedure Save_Container_Size
     (Self : System.Address;
      Size : Long_Integer)
     with Export, Convention => C, External_Name => "save_container_size";
   procedure End_Container_Test
     (Self : System.Address;
      Allocated, Allocs_Count, Frees_Count : Natural)
      with Export, Convention => C, External_Name => "end_container_test";
   procedure Start_Test
      (Self        : System.Address;
       Name        : chars_ptr;
       Start_Group : Interfaces.C.int)
      with Export, Convention => C, External_Name => "start_test";
   procedure End_Test
     (Self : System.Address;
      Allocated, Allocs_Count, Frees_Count : Natural)
      with Export, Convention => C, External_Name => "end_test";
   procedure End_Test_Not_Run (Self : System.Address)
      with Export, Convention => C, External_Name => "end_test_not_run";

   --------------------------
   -- Start_Container_Test --
   --------------------------

   procedure Start_Container_Test
      (Self         : not null access Output'Class;
       Name         : String;
       Category     : String;
       Favorite     : Boolean := False) is
   begin
      if Self.Global_Result = JSON_Null then
         Memory.Pause;
         Self.Global_Result := Create_Object;
         Self.Global_Result.Set_Field ("repeat_count", Repeat_Count);
         Self.Global_Result.Set_Field ("items_count", Integer_Items_Count);
      end if;

      Self.End_Container_Test;   --  In case one was started

      Memory.Pause;

      Self.Container_Test := GNATCOLL.JSON.Create_Object;
      Append (Self.All_Tests, Self.Container_Test);
      Self.Container_Test.Set_Field ("name", Name);
      Self.Container_Test.Set_Field ("category", Category);

      if Favorite then
         Self.Container_Test.Set_Field ("favorite", Favorite);
      end if;

      Self.Tests_In_Container := Create_Object;
      Self.Container_Test.Set_Field ("tests", Self.Tests_In_Container);

      Memory.Reset;
      Memory.Unpause;
   end Start_Container_Test;

   -------------------------
   -- Save_Container_Size --
   -------------------------

   procedure Save_Container_Size
     (Self        : not null access Output'Class;
      Size        : Long_Integer) is
   begin
      Memory.Pause;
      Self.Container_Test.Set_Field ("size", Size);
      Memory.Pause;
   end Save_Container_Size;

   ------------------------
   -- End_Container_Test --
   ------------------------

   procedure End_Container_Test (Self : not null access Output'Class) is
   begin
      Memory.Pause;
      Self.End_Test;  --  In case one was started

      if Self.Container_Test /= JSON_Null then
         Self.Container_Test.Set_Field
            ("allocated", Current.Total_Allocated'Img);
         Self.Container_Test.Set_Field ("allocs", Current.Allocs);
         Self.Container_Test.Set_Field ("reallocs", Current.Reallocs);
         Self.Container_Test.Set_Field ("frees", Current.Frees);
      end if;

      Memory.Unpause;
   end End_Container_Test;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test
      (Self       : not null access Output'Class;
       Name       : String;
       Comment    : String := "";
       Start_Group : Boolean := False) is
   begin
      Self.End_Test;  --  In case one was started
      Memory.Pause;

      --  Is this a second run for this test ?
      Self.Current_Test := Self.Tests_In_Container.Get (Name);
      if Self.Current_Test = JSON_Null then
         Self.Current_Test := GNATCOLL.JSON.Create_Object;
         Self.Tests_In_Container.Set_Field (Name, Self.Current_Test);
         Self.Current_Test.Set_Field ("duration", Empty_Array);

         if Start_Group then
            Self.Current_Test.Set_Field ("group", True);
         end if;
      end if;

      if Comment /= "" then
         Self.Current_Test.Set_Field ("comment", Comment);
      end if;

      Self.At_Test_Start := Memory.Current;

      Memory.Unpause;
      Self.Start_Time := Clock;
   end Start_Test;

   ----------------------
   -- End_Test_Not_Run --
   ----------------------

   procedure End_Test_Not_Run (Self : not null access Output'Class) is
   begin
      if Self.Current_Test /= JSON_Null then
         Memory.Pause;
         Self.Current_Test.Set_Field ("duration", Empty_Array);
         Self.Current_Test := JSON_Null;
         Memory.Unpause;
      end if;
   end End_Test_Not_Run;

   --------------
   -- End_Test --
   --------------

   procedure End_Test (Self : not null access Output'Class) is
      E : constant Time := Clock;
      Info : Mem_Info;
   begin
      if Self.Current_Test /= JSON_Null then
         Memory.Pause;

         declare
            Arr : JSON_Array := Self.Current_Test.Get ("duration");
         begin
            Append (Arr, Create (Float (E - Self.Start_Time)));
            Self.Current_Test.Set_Field ("duration", Arr);
         end;

         Info := Memory.Current - Self.At_Test_Start;
         Self.Current_Test.Set_Field ("allocated", Info.Total_Allocated'Img);
         Self.Current_Test.Set_Field ("allocs", Info.Allocs);
         Self.Current_Test.Set_Field ("reallocs", Info.Reallocs);
         Self.Current_Test.Set_Field ("frees", Info.Frees);

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

      Put_Line ("Open file://" & Get_Current_Dir & "/index.html");
   end Display;

   --------------------------
   -- Start_Container_Test --
   --------------------------

   procedure Start_Container_Test
      (Self : System.Address;
       Name, Category : chars_ptr;
       Favorite : Integer) is
   begin
      Start_Container_Test
         (To_Output (Self), Value (Name),
          Value (Category), Favorite => Favorite /= 0);
   end Start_Container_Test;

   -------------------------
   -- Save_Container_Size --
   -------------------------

   procedure Save_Container_Size
     (Self : System.Address;
      Size : Long_Integer) is
   begin
      Save_Container_Size (To_Output (Self), Size);
   end Save_Container_Size;

   ------------------------
   -- End_Container_Test --
   ------------------------

   procedure End_Container_Test
     (Self                                 : System.Address;
      Allocated, Allocs_Count, Frees_Count : Natural) is
   begin
      Memory.Current := (Total_Allocated => Long_Long_Integer (Allocated),
                         Allocs          => Allocs_Count,
                         Frees           => Frees_Count,
                         Reallocs        => 0);
      End_Container_Test (To_Output (Self));
   end End_Container_Test;

   ----------------
   -- Start_Test --
   ----------------

   procedure Start_Test
      (Self        : System.Address;
       Name        : chars_ptr;
       Start_Group : Interfaces.C.int) is
   begin
      Start_Test
         (To_Output (Self), Value (Name), Start_Group => Start_Group /= 0);
   end Start_Test;

   --------------
   -- End_Test --
   --------------

   procedure End_Test
     (Self                                 : System.Address;
      Allocated, Allocs_Count, Frees_Count : Natural) is
   begin
      Memory.Current := (Total_Allocated => Long_Long_Integer (Allocated),
                         Allocs          => Allocs_Count,
                         Frees           => Frees_Count,
                         Reallocs        => 0);
      End_Test (To_Output (Self));
   end End_Test;

   ----------------------
   -- End_Test_Not_Run --
   ----------------------

   procedure End_Test_Not_Run (Self : System.Address) is
   begin
      End_Test_Not_Run (To_Output (Self));
   end End_Test_Not_Run;
end Report;
