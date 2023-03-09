--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Conts.Algorithms;  use Conts.Algorithms;
with Conts.Vectors.Definite_Unbounded;
with Ada.Calendar;      use Ada.Calendar;
with Ada.Text_IO;       use Ada.Text_IO;
with Asserts;           use Asserts;

procedure Main is
   use Asserts.Booleans;

   Perf : constant Boolean := Argument_Count > 0;

   Max : constant := 10_000;
   subtype Index_Type is Positive;

   package Int_Ada_Vecs is new Ada.Containers.Vectors
      (Index_Type, Integer);
   package Ada_Sort is new Int_Ada_Vecs.Generic_Sorting ("<");

   package Int_Vecs is new Conts.Vectors.Definite_Unbounded
      (Index_Type, Integer, Ada.Finalization.Controlled);
   use Int_Vecs;

   package Rand is new Conts.Default_Random (Extended_Index);

   procedure Insert_Sort is new Conts.Algorithms.Insertion_Sort
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<",
       Swap    => Int_Vecs.Swap);
   procedure Shell_Sort is new Conts.Algorithms.Shell_Sort
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<",
       Swap    => Int_Vecs.Swap);
   procedure Shell2_Sort is new Conts.Algorithms.Shell_Sort
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<",
       Swap    => Int_Vecs.Swap,
       Gaps    => Sedgewick_Gaps);
   procedure Quicksort is new Conts.Algorithms.Quicksort
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<",
       Swap    => Int_Vecs.Swap);
   procedure Quicksort_Pure is new Conts.Algorithms.Quicksort
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<",
       Threshold => 0,
       Swap    => Int_Vecs.Swap);
   function Is_Sorted is new Conts.Algorithms.Is_Sorted
      (Cursors => Int_Vecs.Cursors.Forward,
       Getters => Int_Vecs.Maps.Element,
       "<"     => "<");

   procedure Dump (V : Vector; Msg : String);
   --  Print the contents of V on stdout

   generic
      with procedure Sort
         (Self : in out Int_Vecs.Cursors.Random_Access.Container);
      Algo : String;
   procedure Do_Sort
      (V    : Vector;
       Msg  : String);
   --  Apply one sort algorithm to a copy of V

   procedure Test_Sort (V : Vector; Msg : String);
   --  Apply multiple sort algorithms

   procedure Ada_Test (V : Int_Ada_Vecs.Vector; Msg : String);
   --  Test sorting on a standard Ada array

   ----------
   -- Dump --
   ----------

   procedure Dump (V : Vector; Msg : String) is
   begin
      Put (Msg);
      Put (": [");
      for E of V loop
         Put (E'Img);
         Put (", ");
      end loop;
      Put_Line ("]");
   end Dump;

   -------------
   -- Do_Sort --
   -------------

   procedure Do_Sort
      (V    : Vector;
       Msg  : String)
   is
      V2    : Vector := V;
      Start : constant Time := Clock;
      Dur   : Duration;
   begin
      Sort (V2);
      Dur := Clock - Start;
      Assert (Is_Sorted (V2), True, Algo & " failed for " & Msg);
      if Max <= 10 then
         Dump (V2, Msg);
      end if;
      if Perf then
         Put_Line (Msg & " " & Algo & " =>" & Dur'Img & "s");
      end if;
   end Do_Sort;

   procedure Do_Sort_Insert     is new Do_Sort (Insert_Sort, "insertion-sort");
   procedure Do_Sort_Shell      is new Do_Sort (Shell_Sort,  "shell-sort    ");
   procedure Do_Sort_Shell2     is new Do_Sort (Shell2_Sort, "shell2-sort   ");
   procedure Do_Sort_Quick      is new Do_Sort (Quicksort,   "quicksort     ");
   procedure Do_Sort_Quick_Pure is
      new Do_Sort (Quicksort_Pure, "quicksort_pure");

   ---------------
   -- Test_Sort --
   ---------------

   procedure Test_Sort (V : Vector; Msg : String) is
   begin
      --  Too slow otherwise
      if Max <= 10_000 then
         Do_Sort_Insert (V, Msg);
         Do_Sort_Shell  (V, Msg);
         Do_Sort_Shell2 (V, Msg);
      end if;

      Do_Sort_Quick      (V, Msg);
      Do_Sort_Quick_Pure (V, Msg);
   end Test_Sort;

   --------------
   -- Ada_Test --
   --------------

   procedure Ada_Test (V : Int_Ada_Vecs.Vector; Msg : String) is
      V2    : Int_Ada_Vecs.Vector := V;
      Start : constant Time := Clock;
      Dur   : Duration;
   begin
      Ada_Sort.Sort (V2);
      Dur := Clock - Start;
      if Perf then
         Put_Line (Msg & " standard ada   =>" & Dur'Img & "s");
      end if;
   end Ada_Test;

   V     : Vector;
   V2    : Int_Ada_Vecs.Vector;
   Val   : Extended_Index;
   G     : Rand.Generator;

begin
   Rand.Reset (G);

   for J in 1 .. Max loop
      V.Append (J);
      V2.Append (J);
   end loop;
   Test_Sort (V,  "sorted array  ");
   Ada_Test (V2,  "sorted array  ");

   if Perf then
      Put_Line ("--");
   end if;
   V.Clear;
   V2.Clear;
   for J in reverse 1 .. Max loop
      V.Append (J);
      V2.Append (J);
   end loop;
   Test_Sort (V, "reversed array");
   Ada_Test (V2, "reversed array");

   if Perf then
      Put_Line ("--");
   end if;
   V.Clear;
   V2.Clear;
   for J in reverse 1 .. Max loop
      V.Append (10);
      V2.Append (10);
   end loop;
   Test_Sort (V, "constant array");
   Ada_Test (V2, "constant array");

   if Perf then
      Put_Line ("--");
   end if;
   V.Clear;
   V2.Clear;
   for J in 1 .. Max loop
      Rand.Random (G, Val);
      V.Append (Val);
      V2.Append (Val);
   end loop;
   Test_Sort (V,  "random array  ");
   Ada_Test (V2,  "random array  ");

end Main;
