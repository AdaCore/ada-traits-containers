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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;  use Ada.Calendar;
with Ref_Support;   use Ref_Support;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Containers.Indefinite_Holders;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with GNATCOLL.Traces;
with Report;        use Report;
with Ref_Support;   use Ref_Support;
with System;

procedure Main is
   Column_Title  : constant Column_Number := 1;
   Column_Set    : constant Column_Number := 2;
   Column_Assign : constant Column_Number := 3;
   Column_Get    : constant Column_Number := 4;
   Column_Ref    : constant Column_Number := 5;
   Column_Allocate : constant Column_Number := Column_Ref + 1;
   Column_Allocs   : constant Column_Number := Column_Allocate + 1;
   Column_Reallocs : constant Column_Number := Column_Allocs + 1;
   Column_Frees    : constant Column_Number := Column_Reallocs + 1;

   Ref_None      : constant Performance_Counter := 1;
   Ref_Set       : constant Performance_Counter := 2;
   Ref_Assign    : constant Performance_Counter := 3;
   Ref_Get       : constant Performance_Counter := 4;

   Stdout        : aliased Output;

   procedure Test_Int_Pointers_Unsafe;
   procedure Test_Int_Pointers;
   procedure Test_Int_Reference;
   procedure Test_Gnatcoll_Int;
   procedure Test_Obj;
   procedure Test_Obj_Free;
   procedure Test_Gnatcoll_Obj;
   procedure Test_Obj_Holders;
   procedure Test_Strings;
   procedure Test_Gnatcoll_Strings;
   procedure Test_Weak_Ref;

   procedure Test_Cpp_Int (Stdout : System.Address);
   pragma Import (C, Test_Cpp_Int, "test_shared_int");
   procedure Test_Cpp_Str (Stdout : System.Address);
   pragma Import (C, Test_Cpp_Str, "test_shared_str");

   type NR is null record;

   procedure Test_Int_Pointers_Unsafe is
      N : NR;
      R : Int_Pointers_Unsafe.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         pragma Unreferenced (V);
         R2 : Int_Pointers_Unsafe.Ref;
         Int   : Integer;
         pragma Volatile (Int);  --  Can't be optimized away
         pragma Unreferenced (Int);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (C);
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  Int := R.Unchecked_Get.all;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  Int := R.Get;
               end loop;
               Stdout.Print_Time (Clock - Start, "(1)");

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "sharedptr unsafe", N);
   end Test_Int_Pointers_Unsafe;

   procedure Test_Int_Pointers is
      N : NR;
      R : Int_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : Int_Pointers.Ref;
         Int   : Integer;
         pragma Volatile (Int);  --  Can't be optimized away
         pragma Unreferenced (V, Int);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (C);
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  Int := R.Unchecked_Get.all;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  Int := R.Get;
               end loop;
               Stdout.Print_Time (Clock - Start, "(1)");

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "sharedptr", N);
   end Test_Int_Pointers;

   procedure Test_Strings is
      N : NR;
      R : String_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : String_Pointers.Ref;
         pragma Unreferenced (V);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set ("Foo");
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

               if R.Unchecked_Get.all /= "Foo" then
                  raise Program_Error with "Got '" & R.Unchecked_Get.all & "'";
               end if;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  if R.Unchecked_Get.all /= "Foo" then
                     raise Program_Error
                        with "Got '" & R.Unchecked_Get.all & "'";
                  end if;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  if R.Get /= "Foo" then
                     raise Program_Error with "Got '" & R.Get & "'";
                  end if;
               end loop;
               Stdout.Print_Time (Clock - Start, "(1)");

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "sharedptr", N);
   end Test_Strings;

   procedure Test_Int_Reference is
      N : NR;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         Int   : Integer;
         pragma Volatile (Int);  --  Can't be optimized away
         pragma Unreferenced (Start, V, Int);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  declare
                     R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (C);
                     pragma Unreferenced (R);
                  begin
                     null;
                  end;
               end loop;

            when Column_Assign =>
               declare
                  R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (2);
               begin
                  for C in 1 .. Count_Smart loop
                     declare
                        R2 : Int_Pointers_Ref.Ref := R;
                        pragma Unreferenced (R2);
                     begin
                        null;
                     end;
                  end loop;
               end;

            when Column_Ref =>
               declare
                  R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (2);
               begin
                  for C in 1 .. Count_Smart loop
                     Int := R;
                  end loop;
               end;

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "As Reftype", N);
   end Test_Int_Reference;

   procedure Test_Gnatcoll_Strings is
      N : NR;
      R : String_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : String_Pointers_Gnatcoll.Ref;
         pragma Unreferenced (Start, V);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (String_Object'
                          (Refcounted with Str => new String'("Foo")));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  if R.Get.Str.all /= "Foo" then
                     raise Program_Error with "Got " & R.Get.Str.all;
                  end if;
               end loop;

            when others => Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "smartptr", N);
   end Test_Gnatcoll_Strings;

   procedure Test_Gnatcoll_Int is
      N : NR;
      R : Int_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : Int_Pointers_Gnatcoll.Ref;
         Int   : Integer;
         pragma Volatile (Int);  --  Can't be optimized away
         pragma Unreferenced (Start, V, Int);
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (Integer_Object'(Refcounted with Value => C));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  Int := R.Get.Value;
               end loop;

            when others => Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "smartptr", N);
   end Test_Gnatcoll_Int;

   procedure Test_Obj is
      N : NR;
      R : Obj_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         pragma Unreferenced (V);
         R2 : Obj_Pointers.Ref;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (Child'(Object with null record));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Unchecked_Get.all;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Get;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;
               Stdout.Print_Time (Clock - Start, "(1)");

            when others => null;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "sharedptr", N);
   end Test_Obj;

   procedure Test_Obj_Free is
      N : NR;
      R : Obj_Pointers_Free.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         pragma Unreferenced (Start, V);
         R2 : Obj_Pointers_Free.Ref;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (Child'(Object with null record));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Unchecked_Get.all;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Get;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "sharedptr Free", N);
   end Test_Obj_Free;

   procedure Test_Obj_Holders is
      package Holders is new Ada.Containers.Indefinite_Holders (Object'Class);
      use Holders;

      N : NR;
      R : Holder;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         pragma Unreferenced (Start, V);
         R2 : Holder;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R := To_Holder (Child'(Object with null record));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Reference;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others =>
               Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "Holders", N);
   end Test_Obj_Holders;

   procedure Test_Gnatcoll_Obj is
      N : NR;
      R : Obj_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         pragma Unreferenced (Start, V);
         R2 : Obj_Pointers_Gnatcoll.Ref;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (Child2'(Object2 with null record));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object2'Class := R.Get.all;
                     pragma Volatile (C); --  Don't optimize away
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others => Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests (Stdout, "smartptr", N);
   end Test_Gnatcoll_Obj;

   procedure Test_Weak_Ref is
      use Int_Pointers;
   begin
      --  First scenario: weak ref outlives the ref

      declare
         W : Int_Pointers.Weak_Ref;
      begin
         declare
            R, R2 : Int_Pointers.Ref;
         begin
            R.Set (999);
            W := R.Weak;

            R2.Set (W);
            if R2.Get /= 999 then
               Put_Line ("Expected 999 from a weak ref");
            end if;
         end;

         if not W.Was_Freed then
            Put_Line ("Weak ref should have been freed");
         end if;

         declare
            R2 : Int_Pointers.Ref;
         begin
            R2.Set (W);
            if R2 /= Null_Ref then
               Put_Line ("Expected a null ref from a weak ref");
            end if;
         end;
      end;

      --  Second scenario: ref outlives the weak ref

      declare
         R : Ref;
      begin
         R.Set (999);

         declare
            W : Weak_Ref := R.Weak;
            R2 : Ref;
         begin
            R2.Set (W);
            if R2.Get /= 999 then
               Put_Line ("Scenario2: expected 999");
            end if;
         end;
      end;
   end Test_Weak_Ref;

begin
   Stdout.Setup
      (Counters_Count => 4,
       Show_Percent   => True,
       Columns  =>
          (Column_Title  => (new String'(""),          16, True, Ref_None),
           Column_Set    => (new String'("Set"),       7,  True, Ref_Set),
           Column_Assign => (new String'("Assign"),    7,  True, Ref_Assign),
           Column_Get    => (new String'("Get"),       7,  False, Ref_Get),
           Column_Ref    => (new String'("Reference"), 9,  True, Ref_Get),
           Column_Allocate => (new String'("Allocate"), 8, False,
                             Last_Column_With_Test),
           Column_Allocs   => (new String'("Allocs"), 8, False, Ref_None),
           Column_Reallocs => (new String'("reall"),  5, False, Ref_None),
           Column_Frees    => (new String'("frees"),  8, True, Ref_None)));

   Put_Line ("Storing integers");
   Stdout.Print_Header;
   Test_Gnatcoll_Int;
   Test_Cpp_Int (To_Address (Stdout'Unchecked_Access));
   Test_Int_Pointers_Unsafe;
   Test_Int_Pointers;
   Test_Int_Reference;

   Stdout.Reset;
   New_Line;
   Put_Line ("Storing class wide");
   Test_Gnatcoll_Obj;
   Test_Obj_Holders;
   Test_Obj;
   Test_Obj_Free;

   Stdout.Reset;
   New_Line;
   Put_Line ("Storing unconstrained array");
   Test_Gnatcoll_Strings;
   Test_Cpp_Str (To_Address (Stdout'Unchecked_Access));
   Test_Strings;
   Stdout.Finish_Line;

   New_Line;
   Put_Line ("'Unsafe': use standard int operations, not atomic");
   Put_Line ("'Free': a non-null operation to free the object");
   Put_Line
      ("'as reftype' is when the smart pointer itself is a reference type");
   Put_Line
      ("   current this doesn't work well since this is unconstrained type");
   Put_Line ("(1): limited reference_type are slow, see O521-013");
   Put_Line ("     Seems to be doing 1562 malloc calls");

   Test_Weak_Ref;

   --  for the sake of valgrind
   Stdout.Finalize;
   GNATCOLL.Traces.Finalize;

end Main;
