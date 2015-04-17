with Ada.Calendar;  use Ada.Calendar;
with Support;       use Support;
with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Containers.Indefinite_Holders;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with GNATCOLL.Traces;
with GNAT.Strings;  use GNAT.Strings;

procedure Main is
   Count : constant := 10; --  1_000_000;
   Start : Time;

   type Time_Ref is (Ref_None, Ref_Set, Ref_Assign, Ref_Get);
   type Time_Ref_Array is array (Time_Ref) of Duration;
   All_Refs : Time_Ref_Array := (others => 0.0);

   type Column_Descr is record
      Title : String_Access;
      Width : Natural;
      Ref   : Time_Ref;
   end record;
   type Columns_Array is array (Natural range <>) of Column_Descr;
   Columns : Columns_Array :=
      (1 => (Width => 13,  Ref => Ref_None,   Title => new String'("")),
       2 => (Width => 7,   Ref => Ref_Set,    Title => new String'("Set")),
       3 => (Width => 7,   Ref => Ref_Assign, Title => new String'("Assign")),
       4 => (Width => 7,   Ref => Ref_Get,    Title => new String'("Get")),
       5 => (Width => 9,   Ref => Ref_Get, Title => new String'("Reference")));
   Current : Natural := Columns'First;

   procedure Print_Header;
   procedure Reset;
   procedure Print (Str : String);
   procedure Print_Time;
   procedure Finish_Line;

   procedure Print_Header is
   begin
      for C of Columns loop
         Print (C.Title.all);
      end loop;
      Finish_Line;
   end Print_Header;

   procedure Reset is
   begin
      New_Line;
      All_Refs := (others => 0.0);
   end Reset;

   procedure Print (Str : String) is
   begin
      Put (Str & (Str'Length + 1 .. Columns (Current).Width => ' ') & '|');
      Current := Current + 1;
   end Print;

   procedure Print_Time is
      D : constant Duration := Clock - Start;
   begin
      if All_Refs (Columns (Current).Ref) = 0.0 then
         All_Refs (Columns (Current).Ref) := D;
      end if;

      if All_Refs (Columns (Current).Ref) = 0.0 then
         Print ("");
      else
         declare
            S : constant String := Integer'Image
               (Integer
                  (Float'Floor (Float (D)
                      / Float (All_Refs (Columns (Current).Ref))
                   * 100.0)))
                  & '%';
         begin
            Print (S);
         end;
      end if;
   end Print_Time;

   procedure Finish_Line is
   begin
      New_Line;
      Current := Columns'First;
   end Finish_Line;

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

   procedure Test_Int_Pointers_Unsafe is
      R, R2 : Int_Pointers_Unsafe.Ref;
      Int   : Integer;
   begin
      Print ("Std Unsafe");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (C);
      end loop;
      Print_Time;  --  Set

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;  --  Assign

      Start := Clock;
      for C in 1 .. Count loop
         Int := R.Get.all;
      end loop;
      Print_Time;  --  Get

      Start := Clock;
      for C in 1 .. Count loop
         Int := R.Reference;
      end loop;
      Print_Time;  --  Reference

      Finish_Line;
   end Test_Int_Pointers_Unsafe;

   procedure Test_Int_Pointers is
      R, R2 : Int_Pointers.Ref;
      Int   : Integer;
   begin
      Print ("Std");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (C);
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         Int := R.Get.all;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         Int := R.Reference;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Int_Pointers;

   procedure Test_Strings is
      R, R2 : String_Pointers.Ref;
      Str   : access String;
   begin
      Print ("Std");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set ("Foo");
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      if R.Get.all /= "Foo" then
         raise Program_Error with "Got '" & R.Get.all & "'";
      end if;

      Start := Clock;
      for C in 1 .. Count loop
         Str := R.Get;
      end loop;
      Print_Time;

      Start := Clock;
      Print_Time;

      Finish_Line;
   end Test_Strings;

   procedure Test_Int_Reference is
      Int   : Integer;
   begin
      Print ("As Reftype");
      Start := Clock;
      for C in 1 .. Count loop
         declare
            R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (C);
            pragma Unreferenced (R);
         begin
            null;
         end;
      end loop;
      Print_Time;  --  Set

      declare
         R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (2);
      begin
         Start := Clock;
         for C in 1 .. Count loop
            declare
               R2 : Int_Pointers_Ref.Ref := R;
               pragma Unreferenced (R2);
            begin
               null;
            end;
         end loop;
         Print_Time;  --  Assign

         Start := Clock;
         Print_Time;  --  Get

         Start := Clock;
         for C in 1 .. Count loop
            Int := R;
         end loop;
         Print_Time;  -- Reference
      end;

      Finish_Line;
   end Test_Int_Reference;

   procedure Test_Gnatcoll_Strings is
      R, R2 : String_Pointers_Gnatcoll.Ref;
      Str   : access String;
   begin
      Print ("GNATCOLL");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (String_Object'(Refcounted with Str => new String'("foo")));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         Str := R.Get.Str;
      end loop;
      Print_Time;

      Start := Clock;
      Print_Time;

      Finish_Line;
   end Test_Gnatcoll_Strings;

   procedure Test_Gnatcoll_Int is
      R, R2 : Int_Pointers_Gnatcoll.Ref;
      Int   : Integer;
   begin
      Print ("GNATCOLL");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (Integer_Object'(Refcounted with Value => C));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         Int := R.Get.Value;
      end loop;
      Print_Time;

      Start := Clock;
      Print_Time;

      Finish_Line;
   end Test_Gnatcoll_Int;

   procedure Test_Obj is
      R, R2 : Obj_Pointers.Ref;
   begin
      Print ("Std");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (Child'(Object with null record));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object'Class := R.Get.all;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object'Class := R.Reference;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Obj;

   procedure Test_Obj_Free is
      R, R2 : Obj_Pointers_Free.Ref;
   begin
      Print ("Std Free");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (Child'(Object with null record));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object'Class := R.Get.all;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object'Class := R.Reference;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Obj_Free;

   procedure Test_Obj_Holders is
      package Holders is new Ada.Containers.Indefinite_Holders (Object'Class);
      use Holders;
      R, R2 : Holder;
   begin
      Print ("Holders");
      Start := Clock;
      for C in 1 .. Count loop
         R := To_Holder (Child'(Object with null record));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      Print_Time;  --  Get

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object'Class := R.Reference;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Obj_Holders;

   procedure Test_Gnatcoll_Obj is
      R, R2 : Obj_Pointers_Gnatcoll.Ref;
   begin
      Print ("GNATCOLL");
      Start := Clock;
      for C in 1 .. Count loop
         R.Set (Child2'(Object2 with null record));
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         R2 := R;
      end loop;
      Print_Time;

      Start := Clock;
      for C in 1 .. Count loop
         declare
            C : Object2'Class := R.Get.all;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Start := Clock;
      Print_Time;

      Finish_Line;
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

begin
   Put_Line ("Storing integers");
   Print_Header;
   Test_Gnatcoll_Int;
   Test_Int_Pointers_Unsafe;
   Test_Int_Pointers;
   Test_Int_Reference;

   Reset;
   Put_Line ("Storing class wide");
   Test_Gnatcoll_Obj;
   Test_Obj_Holders;
   Test_Obj;
   Test_Obj_Free;

   Reset;
   Put_Line ("Storing unconstrained array");
   Test_Gnatcoll_Strings;
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
   for C of Columns loop
      Free (C.Title);
   end loop;
   GNATCOLL.Traces.Finalize;

end Main;
