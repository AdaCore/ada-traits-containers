with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Text_IO;        use Ada.Text_IO;
with Conts.Lists.Definite_Unbounded;
with Conts.Lists.Indefinite_Unbounded;
with Conts.Lists.Indefinite_Unbounded_SPARK;
with Conts.Lists.Definite_Bounded;
with Conts.Lists.Definite_Bounded_Limited;
with Conts.Algorithms;
with Conts.Adaptors;     use Conts.Adaptors;
with Taggeds;
with Interfaces.C.Strings;

--  The tests all use a subprogram with a class-wide parameter, to force the
--  use of dynamic dispatching and simulate real applications.

package body Perf_Support is

   function Greater_Than_3 (P : Integer) return Boolean is (P > 3)
      with Inline => True;

   function Starts_With_Str (S : String) return Boolean is
      (S (S'First) = 's');
   pragma Inline (Starts_With_Str);

   ------------
   -- Output --
   ------------

   procedure Reset (Self : in out Output) is
   begin
      Self.Finish_Line;
      Self.Fill_Ref := 0.0;
      Self.Loop_Ref := 0.0;
   end Reset;

   procedure Start_Line
      (Self : in out Output; Title : String; Fewer_Items : Boolean := False) is
   begin
      Self.Finish_Line;
      Put (Title & (Title'Length + 1 .. 18 => ' ') & '|');
      Self.Fewer_Items := Fewer_Items;
      Self.Column := 2;
   end Start_Line;

   procedure Finish_Line (Self : in out Output) is
   begin
      if Self.Column /= -1 then
         Self.Column := -1;
         if Items_Count /= Small_Items_Count and then Self.Fewer_Items then
            Put_Line (" fewer items");
         else
            New_Line;
         end if;
      end if;
   end Finish_Line;

   procedure Print_Time
      (Self : in out Output; D : Duration; Extra : String := "")
   is
      Ref : Duration;
   begin
      if Self.Show_Percent then
         if Self.Column = 2 then
            if Self.Fill_Ref = 0.0 then
               Self.Fill_Ref := D;
            end if;
            Ref := Self.Fill_Ref;
         else
            if Self.Loop_Ref = 0.0 then
               Self.Loop_Ref := D;
            end if;
            Ref := Self.Loop_Ref;
         end if;

         declare
            S : constant String := Integer'Image
               (Integer (Float'Floor (Float (D) / Float (Ref) * 100.0))) & '%';
         begin
            Put (S & Extra & (S'Length + Extra'Length + 1 .. 14 => ' ') & '|');
         end;

      else
         declare
            S   : constant String := D'Img;
            Sub : constant String :=
               S (S'First .. Integer'Min (S'Last, S'First + 7));
         begin
            Put (Sub & Extra
                 & (Sub'Length + Extra'Length + 1 .. 14 => ' ') & '|');
         end;
      end if;

      if Self.Column = 2 then
         Put ('|');
      end if;

      Self.Column := Self.Column + 1;
   end Print_Time;

   procedure Print_Not_Run (Self : in out Output; Extra : String := "") is
   begin
      Put (' ' & Extra & (Extra'Length + 1 .. 13 => ' ') & '|');
      if Self.Column = 2 then
         Put ('|');
      end if;

      Self.Column := Self.Column + 1;
   end Print_Not_Run;

   procedure Print_From_C (D : Interfaces.C.double);
   pragma Export (C, Print_From_C, "_ada_print_time");
   procedure Print_From_C (D : Interfaces.C.double) is
   begin
      Stdout.Print_Time (Duration (D));
   end Print_From_C;

   procedure Start_Line_C (Title : Interfaces.C.Strings.chars_ptr);
   pragma Export (C, Start_Line_C, "_ada_start_line");
   procedure Start_Line_C (Title : Interfaces.C.Strings.chars_ptr) is
   begin
      Stdout.Start_Line (Interfaces.C.Strings.Value (Title));
   end Start_Line_C;

   -------------------------------
   -- Test_Lists_Int_Indefinite --
   -------------------------------

   procedure Test_Lists_Int_Indefinite is
      package Lists is new Conts.Lists.Indefinite_Unbounded
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-i-c)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V2 loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;

      V2 : Lists.List := V;   --  Check this is not limited type
      pragma Unreferenced (V2);
   begin
      Do_Test (V);
   end Test_Lists_Int_Indefinite;

   -------------------------------------
   -- Test_Lists_Int_Indefinite_SPARK --
   -------------------------------------

   procedure Test_Lists_Int_Indefinite_SPARK is
      package Lists is new Conts.Lists.Indefinite_Unbounded_SPARK
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-i-s)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V2 loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;

   begin
      Do_Test (V);
   end Test_Lists_Int_Indefinite_SPARK;

   --------------------
   -- Test_Lists_Int --
   --------------------

   procedure Test_Lists_Int is
      package Lists is new Conts.Lists.Definite_Unbounded
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Lists, Lists.Lists;   --  second is for Ada95 notation
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-d-c)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);    --  testing withe prefix notation
         Append (V2, 6);   --  testing with Ada95 notation
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V2 loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;

      V2 : Lists.List := V;   --  Check this is not limited type
      pragma Unreferenced (V2);
   begin
      Do_Test (V);
   end Test_Lists_Int;

   --------------------------------
   -- Test_Lists_Bounded_Limited --
   --------------------------------

   procedure Test_Lists_Bounded_Limited is
      package Lists is new Conts.Lists.Definite_Bounded_Limited
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (b-d-l)", Fewer_Items => True);

         Start := Clock;
         for C in 1 .. Small_Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V2 loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List (Capacity => Small_Items_Count + 2);
      --  V2 : Lists.List := V;   --  Check this is limited type
   begin
      Do_Test (V);
   end Test_Lists_Bounded_Limited;

   ------------------------
   -- Test_Lists_Bounded --
   ------------------------

   procedure Test_Lists_Bounded is
      package Lists is new Conts.Lists.Definite_Bounded
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (b-d-c)", Fewer_Items => True);

         Start := Clock;
         for C in 1 .. Small_Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V2 loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List (Capacity => Small_Items_Count + 2);

      pragma Warnings (Off);
      V2 : Lists.List := V;   --  Check this is not limited type
      pragma Warnings (On);
   begin
      Do_Test (V);
   end Test_Lists_Bounded;

   ---------------------------
   -- Test_Lists_Str_Access --
   ---------------------------

   procedure Test_Lists_Str_Access is
      package Lists is new Conts.Lists.Indefinite_Unbounded
         (Element_Type   => String,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors_Access);

      function Starts_With_Str (S : Lists.Element_Access) return Boolean is
         (S (S'First) = 's');
      pragma Inline (Starts_With_Str);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It    : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-i-c) (3)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append ("str1");
         end loop;
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if Starts_With_Str (V2.Stored_Element (It).all) then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         --  Start := Clock;
         --  Co := 0;
         --  for E of V2 loop  -- GNAT: unconstrained subtype not allowed
         --     if Starts_With_Str (E) then
         --        Co := Co + 1;
         --     end if;
         --  end loop;
         --  Print_Time (Clock - Start);
         --  if Co /= Items_Count then
         --     raise Program_Error;
         --  end if;
         Stdout.Print_Not_Run ("(2)");

         Start := Clock;
         Co := Count_If (V2, Starts_With_Str'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Str_Access;

   ------------------------------
   -- Test_Lists_Str_Reference --
   ------------------------------

   procedure Test_Lists_Str_Reference is
      package Lists is new Conts.Lists.Indefinite_Unbounded
         (Element_Type   => String,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors_Reference);

      function Ref_Starts_With_Str (S : Lists.Reference_Type) return Boolean is
         (S (S.E'First) = 's');
      pragma Inline (Ref_Starts_With_Str);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It    : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-i-c) (4)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append ("str1");
         end loop;
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if Ref_Starts_With_Str (V2.Reference (It)) then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         --  Start := Clock;
         --  Co := 0;
         --  for E of V2 loop  -- GNAT: unconstrained subtype not allowed
         --     if Starts_With_Str (E) then
         --        Co := Co + 1;
         --     end if;
         --  end loop;
         --  Print_Time (Clock - Start);
         --  if Co /= Items_Count then
         --     raise Program_Error;
         --  end if;
         Stdout.Print_Not_Run ("(2)");

         Start := Clock;
         Co := Count_If (V2, Ref_Starts_With_Str'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Str_Reference;

   --------------------
   -- Test_Lists_Str --
   --------------------

   procedure Test_Lists_Str is
      package Lists is new Conts.Lists.Indefinite_Unbounded
         (Element_Type   => String,
          Enable_Asserts => False);
      use Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Lists.List'Class);
      procedure Do_Test (V2 : in out Lists.List'Class) is
         It    : Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Lists (u-i-c)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append ("str1");
         end loop;
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if Starts_With_Str (V2.Element (It)) then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         --  Start := Clock;
         --  Co := 0;
         --  for E of V2 loop  -- GNAT: unconstrained subtype not allowed
         --     if Starts_With_Str (E) then
         --        Co := Co + 1;
         --     end if;
         --  end loop;
         --  Print_Time (Clock - Start);
         --  if Co /= Items_Count then
         --     raise Program_Error;
         --  end if;
         Stdout.Print_Not_Run ("(2)");

         Start := Clock;
         Co := Count_If (V2, Starts_With_Str'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Str;

   ----------------------
   -- Test_Ada2012_Str --
   ----------------------

   procedure Test_Ada2012_Str is
      package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
         (String);
      use Lists;
      package Adaptors is new Indefinite_List_Adaptors (Lists);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Adaptors.Forward_Cursors);

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         It    : Lists.Cursor;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Ada indefinite");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append ("str1");
         end loop;
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Starts_With_Str (Element (It)) then  --  secondary stack
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V loop
            if Starts_With_Str (E) then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         Start := Clock;
         --  ??? Why do we need a cast here
         Co := Count_If (List (V), Starts_With_Str'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;
      end Do_Test;

      V  : Lists.List;
   begin
      Do_Test (V);
   end Test_Ada2012_Str;

   ---------------------
   -- Test_Arrays_Int --
   ---------------------

   procedure Test_Arrays_Int is
      type Int_Array is array (Integer range <>) of Integer;
      package Adaptors is new Array_Adaptors
         (Index_Type   => Integer,
          Element_Type => Integer,
          Array_Type   => Int_Array);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Adaptors.Forward_Cursors);

      V     : Int_Array (1 .. Small_Items_Count + 2);
      Start : Time;
      Co    : Natural;
   begin
      Stdout.Start_Line ("Array", Fewer_Items => True);

      Start := Clock;
      for C in 1 .. Small_Items_Count loop
         V (C) := 2;
      end loop;
      V (V'Last - 1) := 5;
      V (V'Last) := 6;
      Stdout.Print_Time (Clock - Start);

      Start := Clock;
      Co := 0;
      for It in V'Range loop
         if V (It) > 3 then
            Co := Co + 1;
         end if;
      end loop;
      Stdout.Print_Time (Clock - Start);
      if Co /= 2 then
         raise Program_Error;
      end if;

      Start := Clock;
      Co := 0;
      for E of V loop
         if E > 3 then
            Co := Co + 1;
         end if;
      end loop;
      Stdout.Print_Time (Clock - Start);
      if Co /= 2 then
         raise Program_Error;
      end if;

      Start := Clock;
      Co := Count_If (V, Greater_Than_3'Access);
      Stdout.Print_Time (Clock - Start);
      if Co /= 2 then
         raise Program_Error;
      end if;
   end Test_Arrays_Int;

   ----------------------
   -- Test_Ada2012_Int --
   ----------------------

   procedure Test_Ada2012_Int is
      package Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);
      use Lists;
      package Adaptors is new List_Adaptors (Lists);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Adaptors.Forward_Cursors);

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         It    : Lists.Cursor;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Ada (definite)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Element (It) > 3 then
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (List (V), Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V  : Lists.List;
   begin
      Do_Test (V);
   end Test_Ada2012_Int;

   ---------------------------------
   -- Test_Ada2012_Int_Indefinite --
   ---------------------------------

   procedure Test_Ada2012_Int_Indefinite is
      package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
         (Integer);
      use Lists;
      package Adaptors is new Indefinite_List_Adaptors (Lists);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Adaptors.Forward_Cursors);

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         It    : Lists.Cursor;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Ada (indefinite)");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Element (It) > 3 then
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := 0;
         for E of V loop
            if E > 3 then
               Co := Co + 1;
            end if;
         end loop;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (List (V), Greater_Than_3'Access);
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V  : Lists.List;
   begin
      Do_Test (V);
   end Test_Ada2012_Int_Indefinite;

   ---------------------
   -- Test_Tagged_Int --
   ---------------------

   procedure Test_Tagged_Int is
      package Lists is new Taggeds (Integer);
      use Lists;

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         Co    : Natural;
      begin
         Stdout.Start_Line ("Tagged types");

         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Stdout.Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         declare
            It  : Lists.List_Cursor'Class := List_Cursor (V.First);
            --  Casting to List_Cursor here halves the time to run the
            --  loop by avoiding dynamic dispatching.
         begin
            while It.Has_Element loop
               if It.Element > 3 then
                  Co := Co + 1;
               end if;
               It.Next;
            end loop;
         end;
         Stdout.Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Tagged_Int;

end Perf_Support;
