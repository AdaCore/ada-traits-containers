with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Text_IO;        use Ada.Text_IO;
with Conts.Lists;
with Conts.Algorithms;
with Conts.Adaptors;     use Conts.Adaptors;
with Taggeds;

package body Perf_Support is

   function Greater_Than_3 (P : Integer) return Boolean is (P > 3)
      with Inline => True;

   ----------------------
   -- Test_Conts_Lists --
   ----------------------

   procedure Test_Conts_Lists is
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
         D     : Duration;
         Co    : Natural;
      begin
         Start := Clock;
         for C in 1 .. Items_Count loop
            V2.Append (2);
         end loop;
         V2.Append (5);
         V2.Append (6);
         D := Clock - Start;
         Ada.Text_IO.Put_Line ("Conts.Lists.List, Fill => " & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            It := V2.First;
            while V2.Has_Element (It) loop
               if V2.Element (It) > 3 then
                  Co := Co + 1;
               end if;
               It := V2.Next (It);
            end loop;
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with explicit loop => "
            & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            for E of V2 loop
               if E > 3 then
                  Co := Co + 1;
               end if;
            end loop;
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with for..of loop => "
            & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            Co := Co + Count_If (V2, Greater_Than_3'Access);
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with count_if => "
             & D'Img);
      end Do_Test;

      V : Conts_Int_Lists.List;
   begin
      Do_Test (V);
   end Test_Conts_Lists;

   ------------------
   -- Test_Ada2012 --
   ------------------

   procedure Test_Ada2012 is
      package Lists is new Ada.Containers.Doubly_Linked_Lists (Integer);
      use Lists;
      package Adaptors is new List_Adaptors (Lists);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Adaptors.Forward_Cursors);

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         D     : Duration;
         It    : Lists.Cursor;
         Co    : Natural;
      begin
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         D := Clock - Start;
         Ada.Text_IO.Put_Line ("Ada2012, Fill => " & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            It := V.First;
            while Has_Element (It) loop
               if Element (It) > 3 then
                  Co := Co + 1;
               end if;
               Next (It);
            end loop;
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Ada2012, Count with explicit loop => " & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            for E of V loop
               if E > 3 then
                  Co := Co + 1;
               end if;
            end loop;
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Ada2012, Count with for..of loop => " & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
            Co := Co + Count_If (List (V), Greater_Than_3'Access);
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Ada2012, Count with count_if => " & D'Img);
      end Do_Test;

      V  : Lists.List;
   begin
      Do_Test (V);
   end Test_Ada2012;

   -----------------
   -- Test_Tagged --
   -----------------

   procedure Test_Tagged is
      package Lists is new Taggeds (Integer);
      use Lists;

      procedure Do_Test (V : in out Lists.List'Class);
      procedure Do_Test (V : in out Lists.List'Class) is
         Start : Time;
         D     : Duration;
         Co    : Natural;
      begin
         Start := Clock;
         for C in 1 .. Items_Count loop
            V.Append (2);
         end loop;
         V.Append (5);
         V.Append (6);
         D := Clock - Start;
         Ada.Text_IO.Put_Line ("Tagged type, Fill => " & D'Img);

         Start := Clock;
         Co := 0;
         for Dummy in 1 .. Repeat loop
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
         end loop;
         D := Clock - Start;
         if Co /= 20 then
            raise Program_Error;
         end if;
         Ada.Text_IO.Put_Line ("Tagged type, explicit loop => " & D'Img);
      end Do_Test;

      V : Lists.List;
   begin
      Do_Test (V);
   end Test_Tagged;

end Perf_Support;
