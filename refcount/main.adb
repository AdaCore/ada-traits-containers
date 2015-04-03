with Ada.Calendar;  use Ada.Calendar;
with Support;       use Support;
with Ada.Text_IO;   use Ada.Text_IO;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;
with GNATCOLL.Traces;
with GNAT.Strings;  use GNAT.Strings;

procedure Main is
   Count : constant := 1_000_000;
   Start : Time;

   type Column_Descr is record
      Title : String_Access;
      Width : Natural;
      Ref   : Duration;
   end record;
   type Columns_Array is array (Natural range <>) of Column_Descr;
   Columns : Columns_Array :=
      (1 => (Width => 17,  Ref => 0.0,  Title => new String'("")),
       2 => (Width => 10,  Ref => 0.0,  Title => new String'("Set")),
       3 => (Width => 10,  Ref => 0.0,  Title => new String'("Assign")),
       4 => (Width => 10,  Ref => 0.0,  Title => new String'("Ref")));
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
      for C of Columns loop
         C.Ref := 0.0;
      end loop;
   end Reset;

   procedure Print (Str : String) is
   begin
      Put (Str & (Str'Length + 1 .. Columns (Current).Width => ' ') & '|');
      Current := Current + 1;
   end Print;

   procedure Print_Time is
      D : constant Duration := Clock - Start;
   begin
      if Columns (Current).Ref = 0.0 then
         Columns (Current).Ref := D;
      end if;

      declare
         S : constant String := Integer'Image
            (Integer
               (Float'Floor (Float (D) / Float (Columns (Current).Ref)
                * 100.0)))
               & '%';
      begin
         Print (S);
      end;
   end Print_Time;

   procedure Finish_Line is
   begin
      New_Line;
      Current := Columns'First;
   end Finish_Line;

   procedure Test_Int_Pointers_Unsafe;
   procedure Test_Int_Pointers;
   procedure Test_Int_Pointers_Traits_Unsafe;
   procedure Test_Int_Pointers_Traits;
   procedure Test_Int_Ref;
   procedure Test_Int_Reference;
   procedure Test_Gnatcoll_Int;
   procedure Test_Obj;
   procedure Test_Obj_Free;
   procedure Test_Obj_Traits;
   procedure Test_Obj_Traits_Free;
   procedure Test_Gnatcoll_Obj;
   procedure Test_Obj_Ref_Free;

   procedure Test_Int_Pointers_Unsafe is
      R, R2 : Int_Pointers_Unsafe.Ref;
      Int   : Integer;
   begin
      Print ("Std Unsafe");
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

      Finish_Line;
   end Test_Int_Pointers;

   procedure Test_Int_Pointers_Traits_Unsafe is
      R, R2 : Int_Pointers_Traits_Unsafe.Ref;
      Int   : Integer;
   begin
      Print ("Traits Unsafe");
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
         Int := R.Get;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Int_Pointers_Traits_Unsafe;

   procedure Test_Int_Pointers_Traits is
      R, R2 : Int_Pointers_Traits.Ref;
      Int   : Integer;
   begin
      Print ("Traits");
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
         Int := R.Get;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Int_Pointers_Traits;

   procedure Test_Int_Ref is
      R, R2 : Int_Pointers_Traits.Ref;
      Int   : Integer;
   begin
      Print ("Reftype");
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
         Int := R.Reference;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Int_Ref;

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
      Print_Time;

      Start := Clock;
      --  for C in 1 .. Count loop
      --     R2 := R;
      --  end loop;
      Print_Time;

      Start := Clock;
      declare
         R : Int_Pointers_Ref.Ref := Int_Pointers_Ref.Set (2);
      begin
         for C in 1 .. Count loop
            Int := R;
         end loop;
      end;
      Print_Time;

      Finish_Line;
   end Test_Int_Reference;

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

      Finish_Line;
   end Test_Obj_Free;

   procedure Test_Obj_Traits is
      R, R2 : Obj_Pointers_Traits.Ref;
   begin
      Print ("Traits");
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

      Finish_Line;
   end Test_Obj_Traits;

   procedure Test_Obj_Ref_Free is
      R, R2 : Obj_Pointers_Free_Traits.Ref;
   begin
      Print ("Reftype Free");
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
            C : Object'Class := R.Reference;
            pragma Unreferenced (C);
         begin
            null;
         end;
      end loop;
      Print_Time;

      Finish_Line;
   end Test_Obj_Ref_Free;

   procedure Test_Obj_Traits_Free is
      R, R2 : Obj_Pointers_Free_Traits.Ref;
   begin
      Print ("Traits Free");
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

      Finish_Line;
   end Test_Obj_Traits_Free;

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

      Finish_Line;
   end Test_Gnatcoll_Obj;

begin
   Put_Line ("Storing integers");
   Print_Header;
   Test_Gnatcoll_Int;
   Test_Int_Pointers_Unsafe;
   Test_Int_Pointers;
   Test_Int_Pointers_Traits_Unsafe;
   Test_Int_Pointers_Traits;
   Test_Int_Ref;
   Test_Int_Reference;

   Reset;
   Put_Line ("Storing strings");
   Test_Gnatcoll_Obj;
   Test_Obj;
   Test_Obj_Free;
   Test_Obj_Traits;
   Test_Obj_Traits_Free;
   Test_Obj_Ref_Free;

   New_Line;
   Put_Line ("'Unsafe': use standard int operations, not atomic");
   Put_Line ("'Free': a non-null operation to free the object");
   Put_Line
      ("Traits possibly avoids one malloc compared to Std, using"
       & " Elements_Traits");
   Put_Line
      ("Reftype is similar to Traits, except Get returns a Reference_Type");
   Put_Line
      ("'as reftype' is when the smart pointer itself is a reference type");
   Put_Line
      ("   current this doesn't work well since this is unconstrained type");
   Put_Line
      ("Getting value for int is much slower than GNATCOLL -- not clear why"
       & " (cache ?)");

   --  for the sake of valgrind
   for C of Columns loop
      Free (C.Title);
   end loop;
   GNATCOLL.Traces.Finalize;

end Main;
