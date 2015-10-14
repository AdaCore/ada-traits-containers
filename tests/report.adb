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
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;        use Ada.Text_IO;
with GNAT.Strings;
with Interfaces.C.Strings;
with Memory;
with Perf_Support;       use Perf_Support;
with System;

package body Report is

   function To_Output is new Ada.Unchecked_Conversion
      (System.Address, Output_Access);

   procedure Put (Self : in out Output; Str : String);
   --  Display text in the current column

   procedure Setup
      (Self           : in out Output;
       Counters_Count : Performance_Counter;
       Columns        : Columns_Array;
       Show_Percent   : Boolean := True)
   is
   begin
      Self.Ref     := new Reference_Times (1 .. Counters_Count);
      Self.Ref.all := (others => 0.0);
      Self.Columns := new Columns_Array'(Columns);
      Self.Show_Percent := Show_Percent;
   end Setup;

   procedure Reset (Self : in out Output) is
   begin
      Self.Finish_Line;
      Self.Ref.all := (others => 0.0);
   end Reset;

   procedure Print_Header (Self : in out Output) is
   begin
      Self.Finish_Line;
      Self.Current := Self.Columns'First;
      for C in Self.Columns'Range loop
         Put (Self, Self.Columns (C).Title.all);
      end loop;
      New_Line;
      Self.Current := Self.Columns'First;
   end Print_Header;

   procedure Put (Self : in out Output; Str : String) is
      W : constant Natural :=
         Self.Columns (Self.Current).Width;
   begin
      Put (Str (Str'First .. Natural'Min (Str'Last, Str'First + W - 1))
           & (Str'Length + 1 .. W => ' '));
      if not Self.Basic
         and then Self.Columns (Self.Current).Wide_Separator
      then
         Put (Character'Val (16#E2#)
              & Character'Val (16#95#)
              & Character'Val (16#91#));
      else
         Put ('|');
      end if;

      if Self.Current /= Self.Columns'Last then
         Self.Current := Column_Number'Succ (Self.Current);
      end if;
   end Put;

   procedure Start_Line
      (Self : in out Output; Title : String; Fewer_Items : Boolean := False) is
   begin
      Self.Finish_Line;
      Memory.Reset;
      Self.Current := Self.Columns'First;
      Put (Self, Title);
      Self.Fewer_Items := Fewer_Items;
   end Start_Line;

   procedure Finish_Line (Self : in out Output) is
   begin
      if Self.Current /= Self.Columns'First then
         while Self.Current /= Self.Columns'Last loop
            Put (Self, "");
         end loop;
         Put (Self, Memory.Frees'Img);
         if Items_Count /= Small_Items_Count and then Self.Fewer_Items then
            Put_Line (" fewer items");
         else
            New_Line;
         end if;
         Self.Current := Self.Columns'First;
      end if;
   end Finish_Line;

   procedure Print_Time
      (Self : in out Output; D : Duration; Extra : String := "")
   is
      Ref : Duration;
   begin
      if Self.Show_Percent then
         Ref := Self.Ref (Self.Columns (Self.Current).Ref);
         if Ref = 0.0 then
            Self.Ref (Self.Columns (Self.Current).Ref) := D;
            Ref := D;
         end if;

         declare
            S : constant String := Integer'Image
               (Integer (Float'Floor (Float (D) / Float (Ref) * 100.0))) & '%';
         begin
            Put (Self, S & Extra);
         end;

      else
         declare
            S   : constant String := D'Img;
            Sub : constant String :=
               S (S'First .. Integer'Min (S'Last, S'First + 7));
         begin
            Put (Self, Sub & Extra);
         end;
      end if;
   end Print_Time;

   -------------------
   -- Print_Not_Run --
   -------------------

   procedure Print_Not_Run (Self : in out Output; Extra : String := "") is
   begin
      Put (Self, Extra);
   end Print_Not_Run;

   ----------------
   -- Print_Size --
   ----------------

   procedure Print_Size (Self : in out Output; Size : Natural) is
      Actual_Size : constant Natural := Size + Natural (Memory.Live);
   begin
      if Actual_Size >= 1_000_000 then
         --  Approximate a kb as 1000 bytes, easier to compare
         Put (Self, Integer'Image (Actual_Size / 1000) & "kb");
      else
         Put (Self, Integer'Image (Actual_Size) & "b");
      end if;

      Put (Self, Memory.Allocs'Img);
      Put (Self, Memory.Reallocs'Img);
   end Print_Size;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests
      (Stdout      : in out Output'Class;
       Title       : String;
       Self        : in out Container;
       Fewer_Items : Boolean := False)
   is
      Start : Time;
   begin
      Stdout.Start_Line (Title, Fewer_Items => Fewer_Items);
      for Col in Stdout.Columns'First + 1 .. Stdout.Columns'Last loop
         exit when Stdout.Columns (Col).Ref = Last_Column_With_Test;
         Start := Clock;
         Run (Self, Col, Start => Start);
         if Stdout.Current = Col then
            Stdout.Print_Time (Clock - Start);
         end if;
      end loop;
      Stdout.Print_Size (Self'Size);
   end Run_Tests;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Self : in out Output) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Columns_Array, Columns_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Reference_Times, Reference_Times_Access);
   begin
      for C of Self.Columns.all loop
         Free (C.Title);
      end loop;
      Unchecked_Free (Self.Columns);
      Unchecked_Free (Self.Ref);
   end Finalize;

   ------------------
   -- Print_From_C --
   ------------------

   procedure Print_From_C
      (Stdout : System.Address; D : Interfaces.C.double);
   pragma Export (C, Print_From_C, "ada_print_time");
   procedure Print_From_C
      (Stdout : System.Address; D : Interfaces.C.double) is
   begin
      To_Output (Stdout).Print_Time (Duration (D));
   end Print_From_C;

   procedure Start_Line_C
      (Stdout : System.Address; Title : Interfaces.C.Strings.chars_ptr);
   pragma Export (C, Start_Line_C, "ada_start_line");
   procedure Start_Line_C
      (Stdout : System.Address; Title : Interfaces.C.Strings.chars_ptr) is
   begin
      To_Output (Stdout).Start_Line (Interfaces.C.Strings.Value (Title));
   end Start_Line_C;
end Report;
