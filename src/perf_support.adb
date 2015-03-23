with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Text_IO;        use Ada.Text_IO;
with Conts.Lists;
with Conts.Indefinite_Lists;
with Conts.Bounded_Lists;
with Conts.Algorithms;
with Conts.Adaptors;     use Conts.Adaptors;
with Taggeds;

--  The tests all use a subprogram with a class-wide parameter, to force the
--  use of dynamic dispatching and simulate real applications.

package body Perf_Support is

   function Greater_Than_3 (P : Integer) return Boolean is (P > 3)
      with Inline => True;

   function Starts_With_Str (S : String) return Boolean is
      (S (S'First) = 's');
   pragma Inline (Starts_With_Str);

   procedure Print_Time (D : Duration; Extra : String := "");
   procedure Print_Time (D : Duration; Extra : String := "") is
      S : constant String := D'Img;
      Sub : constant String :=
         S (S'First .. Integer'Min (S'Last, S'First + 7));
   begin
      if D = 0.0 then Put ((Extra'Length .. 12 => ' ') & Extra & '|');
      else
         Put (Sub & Extra & (Sub'Length + Extra'Length .. 12 => ' ') & '|');
      end if;
   end Print_Time;

   -------------------------------
   -- Test_Lists_Int_Indefinite --
   -------------------------------

   procedure Test_Lists_Int_Indefinite is
      package Conts_Int_Lists is new Conts.Indefinite_Lists
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Conts_Int_Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Conts_Int_Lists.List'Class);
      procedure Do_Test (V2 : in out Conts_Int_Lists.List'Class) is
         It : Conts_Int_Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Conts_Int_Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Int_Indefinite;

   --------------------
   -- Test_Lists_Int --
   --------------------

   procedure Test_Lists_Int is
      package Conts_Int_Lists is new Conts.Lists
         (Element_Type   => Integer,
          Enable_Asserts => False);
      use Conts_Int_Lists;
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);

      procedure Do_Test (V2 : in out Conts_Int_Lists.List'Class);
      procedure Do_Test (V2 : in out Conts_Int_Lists.List'Class) is
         It : Conts_Int_Lists.Cursor;
         Start : Time;
         Co    : Natural;
      begin
         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Conts_Int_Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Int;

   ------------------------
   -- Test_Lists_Bounded --
   ------------------------

   procedure Test_Lists_Bounded is
      package Lists is new Conts.Bounded_Lists
         (Element_Type   => Integer,
          Capacity       => Small_Items_Count + 2,
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
         Start := Clock;
         for C in 1 .. Small_Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if V2.Element (It) > 3 then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start, Extra => "(1)");
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (V2, Greater_Than_3'Access);
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Bounded;

   ---------------------------
   -- Test_Lists_Str_Access --
   ---------------------------

   procedure Test_Lists_Str_Access is
      package Lists is new Conts.Indefinite_Lists
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append ("str1");
         end loop;
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if Starts_With_Str (V2.Stored_Element (It).all) then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (0.0, Extra => "(2)");

         Start := Clock;
         Co := Count_If (V2, Starts_With_Str'Access);
         Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Lists_Str_Access;

   --------------------
   -- Test_Lists_Str --
   --------------------

   procedure Test_Lists_Str is
      package Lists is new Conts.Indefinite_Lists
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append ("str1");
         end loop;
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V2.First;
         while V2.Has_Element (It) loop
            if Starts_With_Str (V2.Element (It)) then
               Co := Co + 1;
            end if;
            It := V2.Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (0.0, Extra => "(2)");

         Start := Clock;
         Co := Count_If (V2, Starts_With_Str'Access);
         Print_Time (Clock - Start);
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append ("str1");
         end loop;
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Starts_With_Str (Element (It)) then  --  secondary stack
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start);
         if Co /= Items_Count then
            raise Program_Error;
         end if;

         Start := Clock;
         --  ??? Why do we need a cast here
         Co := Count_If (List (V), Starts_With_Str'Access);
         Print_Time (Clock - Start);
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
      Start := Clock;
      for C in 1 .. Small_Items_Count loop
         V (C) := 2;
      end loop;
      V (V'Last - 1) := 5;
      V (V'Last) := 6;
      Print_Time (Clock - Start);

      Start := Clock;
      Co := 0;
      for It in V'Range loop
         if V (It) > 3 then
            Co := Co + 1;
         end if;
      end loop;
      Print_Time (Clock - Start);
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
      Print_Time (Clock - Start);
      if Co /= 2 then
         raise Program_Error;
      end if;

      Start := Clock;
      Co := Count_If (V, Greater_Than_3'Access);
      Print_Time (Clock - Start);
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Element (It) > 3 then
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (List (V), Greater_Than_3'Access);
         Print_Time (Clock - Start);
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Print_Time (Clock - Start);

         Start := Clock;
         Co := 0;
         It := V.First;
         while Has_Element (It) loop
            if Element (It) > 3 then
               Co := Co + 1;
            end if;
            Next (It);
         end loop;
         Print_Time (Clock - Start);
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
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;

         Start := Clock;
         Co := Count_If (List (V), Greater_Than_3'Access);
         Print_Time (Clock - Start);
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
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         Print_Time (Clock - Start);

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
         Print_Time (Clock - Start);
         if Co /= 2 then
            raise Program_Error;
         end if;
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Tagged_Int;

end Perf_Support;
