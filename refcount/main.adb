with Ada.Calendar;  use Ada.Calendar;
with Ref_Support;   use Ref_Support;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Containers.Indefinite_Holders;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with GNATCOLL.Traces;
with GNAT.Strings;  use GNAT.Strings;
with Report;        use Report;
with Ref_Support;   use Ref_Support;

procedure Main is
   Column_Title  : constant Column_Number := 1;
   Column_Set    : constant Column_Number := 2;
   Column_Assign : constant Column_Number := 3;
   Column_Get    : constant Column_Number := 4;
   Column_Ref    : constant Column_Number := 5;
   Column_Allocate : constant Column_Number := 6;
   Column_Allocs   : constant Column_Number := 7;
   Column_Reallocs : constant Column_Number := 8;
   Column_Frees    : constant Column_Number := 9;

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

   procedure Test_Shared_Int;
   pragma Import (C, Test_Shared_Int, "test_shared_int");
   procedure Test_Shared_Str;
   pragma Import (C, Test_Shared_Str, "test_shared_str");

   type NR is null record;

   procedure Test_Int_Pointers_Unsafe is
      N : NR;
      R : Int_Pointers_Unsafe.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : Int_Pointers_Unsafe.Ref;
         Int   : Integer;
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
                  Int := R.Get.all;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  Int := R.Reference;
               end loop;

            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Std unsafe", N);
   end Test_Int_Pointers_Unsafe;

   procedure Test_Int_Pointers is
      N : NR;
      R : Int_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : Int_Pointers.Ref;
         Int   : Integer;
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
                  Int := R.Get.all;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  Int := R.Reference;
               end loop;
               
            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Std", N);
   end Test_Int_Pointers;

   procedure Test_Strings is
      N : NR;
      R : String_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : String_Pointers.Ref;
         Str   : access String;
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

               if R.Get.all /= "Foo" then
                  raise Program_Error with "Got '" & R.Get.all & "'";
               end if;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  Str := R.Get;
               end loop;

            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Std", N);
   end Test_Strings;

   procedure Test_Int_Reference is
      N : NR;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         Int   : Integer;
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

            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("As Reftype", N);
   end Test_Int_Reference;

   procedure Test_Gnatcoll_Strings is
      N : NR;
      R : String_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : String_Pointers_Gnatcoll.Ref;
         Str   : access String;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (String_Object'
                          (RefCounted with Str => new String'("foo")));
               end loop;

            when Column_Assign =>
               for C in 1 .. Count_Smart loop
                  R2 := R;
               end loop;

            when Column_Get =>
               for C in 1 .. Count_Smart loop
                  Str := R.Get.Str;
               end loop;

            when others => Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests ("GNATCOLL str", N);
   end Test_Gnatcoll_Strings;

   procedure Test_Gnatcoll_Int is
      N : NR;
      R : Int_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
         R2 : Int_Pointers_Gnatcoll.Ref;
         Int   : Integer;
      begin
         case Col is
            when Column_Set =>
               for C in 1 .. Count_Smart loop
                  R.Set (Integer_Object'(RefCounted with Value => C));
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
      All_Tests ("GNATCOLL int", N);
   end Test_Gnatcoll_Int;

   procedure Test_Obj is
      N : NR;
      R : Obj_Pointers.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
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
                     C : Object'Class := R.Get.all;
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Reference;
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Std", N);
   end Test_Obj;

   procedure Test_Obj_Free is
      N : NR;
      R : Obj_Pointers_Free.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
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
                     C : Object'Class := R.Get.all;
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when Column_Ref =>
               for C in 1 .. Count_Smart loop
                  declare
                     C : Object'Class := R.Reference;
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;
               
            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Std Free", N);
   end Test_Obj_Free;

   procedure Test_Obj_Holders is
      package Holders is new Ada.Containers.Indefinite_Holders (Object'Class);
      use Holders;

      N : NR;
      R : Holder;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
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
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others => null;
         end case;
      end Run;
   begin
      All_Tests ("Holders", N);
   end Test_Obj_Holders;

   procedure Test_Gnatcoll_Obj is
      N : NR;
      R : Obj_Pointers_Gnatcoll.Ref;
      procedure Run (V : in out NR; Col : Column_Number; Start : Time);
      procedure All_Tests is new Run_Tests (NR);

      procedure Run (V : in out NR; Col : Column_Number; Start : Time) is
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
                     pragma Unreferenced (C);
                  begin
                     null;
                  end;
               end loop;

            when others => Stdout.Print_Not_Run;
         end case;
      end Run;
   begin
      All_Tests ("GNATCOLL obj", N);
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

            R2 := W.Get;
            if R2.Get.all /= 999 then
               Put_Line ("Expected 999 from a weak ref");
            end if;
         end;

         if not W.Was_Freed then
            Put_Line ("Weak ref should have been freed");
         end if;

         declare
            R2 : Int_Pointers.Ref := W.Get;
         begin
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
         begin
            if W.Get.Get.all /= 999 then
               Put_Line ("Scenario2: expected 999");
            end if;
         end;
      end;
   end Test_Weak_Ref;

   Ref_None      : constant Performance_Counter := 1;
   Ref_Set       : constant Performance_Counter := 2;
   Ref_Assign    : constant Performance_Counter := 3;
   Ref_Get       : constant Performance_Counter := 4;

begin
   Stdout.Setup
      (Counters_Count => 4,
       Columns  =>
          (Column_Title  => (new String'(""),          13, True, Ref_None),
           Column_Set    => (new String'("Set"),       7,  True, Ref_Set),
           Column_Assign => (new String'("Assign"),    7,  True, Ref_Assign),
           Column_Get    => (new String'("Get"),       7,  False, Ref_Get),
           Column_Ref    => (new String'("Reference"), 9,  True, Ref_Get),
           Column_Allocate => (new String'("Allocate"), 8, False,
                             Last_Column_With_Test),
           Column_Allocs   => (new String'("Allocs"), 8, False, Ref_None),
           Column_Reallocs => (new String'("reall"),  5, False, Ref_None),
           Column_Frees    => (new String'("frees"),  8, True, Ref_None)));
   --  Stdout.Show_Percent := False;

   Put_Line ("Storing integers");
   Stdout.Print_Header;
   Test_Gnatcoll_Int;
   Test_Shared_Int;
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
   Test_Shared_Str;
   Test_Strings;

   New_Line;
   Put_Line ("'Unsafe': use standard int operations, not atomic");
   Put_Line ("'Free': a non-null operation to free the object");
   Put_Line
      ("'as reftype' is when the smart pointer itself is a reference type");
   Put_Line
      ("   current this doesn't work well since this is unconstrained type");

   Test_Weak_Ref;

   --  for the sake of valgrind
   Stdout.Finalize;
   GNATCOLL.Traces.Finalize;

end Main;
