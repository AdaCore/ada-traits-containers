with Perf_Support;  use Perf_Support;
with Ada.Text_IO;
with Ada.Calendar;  use Ada.Calendar;
with Mylists;       use Mylists;
with Conts.Lists;
with Conts.Algorithms;

procedure Perf is
   Repeat : constant := 10;

   use Integer_Lists;
   V : Integer_Lists.List;
   MV : My_Integer_Lists.List;
   Start : Time;
   D : Duration;
   Co : Natural;
begin

   declare
      package Conts_Int_Lists is new Conts.Lists
         (Element_Type   => Integer,
          Enable_Asserts => True);
      use Conts_Int_Lists;
      package Forward_Cursors is new Conts.Forward_Cursors_Traits
        (Container    => Conts_Int_Lists.List,
         Cursor       => Conts_Int_Lists.Cursor,
         Element_Type => Integer);
      function Count_If is new Conts.Algorithms.Count_If
         (Cursors => Forward_Cursors);
      V2 : Conts_Int_Lists.List;
      It : Conts_Int_Lists.Cursor;
   begin
      Start := Clock;
      for C in 1 .. 10_000_000 loop
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
      Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with explicit loop = "
         & Co'Img & " => " & D'Img);

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
      Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with for..of loop = "
         & Co'Img & " => " & D'Img);

      Start := Clock;
      Co := 0;
      for Dummy in 1 .. Repeat loop
         Co := Co + Count_If (V2, Greater_Than_3'Access);
      end loop;
      D := Clock - Start;
      Ada.Text_IO.Put_Line ("Conts.Lists.List, Count with count_if = "
         & Co'Img & " => " & D'Img);
   end;

   ---------
   -- C++ --
   ---------

   declare
      procedure Test_C;
      pragma Import (C, Test_C, "test_c");
   begin
      Test_C;
   end;

   Start := Clock;
   for C in 1 .. 10_000_000 loop
      V.Append (2);
   end loop;
   V.Append (5);
   V.Append (6);
   D := Clock - Start;
   Ada.Text_IO.Put_Line ("Fill V         => " & D'Img);

   Start := Clock;
   for C in 1 .. 10_000_000 loop
      MV.Append (2);
   end loop;
   MV.Append (5);
   MV.Append (6);
   D := Clock - Start;
   Ada.Text_IO.Put_Line ("Fill Mylists.V => " & D'Img);

   --  Version of count when the Ada05 container is instantiated in the same
   --  package as Count.

   Co := 0;
   Start := Clock;
   for Dummy in 1 .. Repeat loop
      Co := Co + Count (V, Greater_Than_3'Access);
   end loop;
   D := Clock - Start;
   Ada.Text_IO.Put_Line
      ("Count (access to function as param)   =" & Co'Img & " =>" & D'Img);

   Co := 0;
   Start := Clock;
   for Dummy in 1 .. Repeat loop
      Co := Co + Count_With_Equal (V, Greater_Than_3'Access);
   end loop;
   D := Clock - Start;
   Ada.Text_IO.Put_Line
      ("Count (with equal)                    =" & Co'Img & " =>" & D'Img);

   declare
      function CG is new Count_With_Generic_Func (Greater_Than_3);
   begin
      Co := 0;
      Start := Clock;
      for Dummy in 1 .. Repeat loop
         Co := Co + CG (MV);
      end loop;
      D := Clock - Start;
      Ada.Text_IO.Put_Line
         ("Count (with generic func, other pkg)  =" & Co'Img & " =>" & D'Img);
   end;

   declare
      type Greater_Than_Funcs is new Unary_Predicates.Obj with record
         Val : Integer;
      end record;
      overriding function Call
         (self : Greater_Than_Funcs; P : Integer) return Boolean
         is (P > Self.Val);
      function Greater_Than (Val : Integer) return Greater_Than_Funcs'Class
         is (Greater_Than_Funcs'(Val => Val));

      function Count is new Count_With_Functor (Unary_Predicates);
   begin
      Co := 0;
      Start := Clock;
      for Dummy in 1 .. Repeat loop
         Co := Co + Count (V, Greater_Than (3));
      end loop;
      D := Clock - Start;
      Ada.Text_IO.Put_Line
         ("Count (with functor)                  =" & Co'Img & " =>" & D'Img);
   end;

   declare
      package Predicate_Funcs is new Unary_Functions
         (Unary_Predicates, Greater_Than_3);
      function Count is new Count_With_Functor (Predicate_Funcs.Functors);
   begin
      Co := 0;
      Start := Clock;
      for Dummy in 1 .. Repeat loop
         Co := Co + Count (V, Predicate_Funcs.Make);
      end loop;
      D := Clock - Start;
      Ada.Text_IO.Put_Line
         ("Count (with function object)         =" & Co'Img & " =>" & D'Img);
   end;

   --  Inline (ie no separate function for the predicate) version of Count,
   --  where the instance of the container is in a separate package.

   declare
      C : Integer_Lists.Cursor;
      Result : Natural := 0;
   begin
      Start := Clock;

      for Dummy in 1 .. Repeat loop
         C := V.First;
         while Has_Element (C) loop
            if Element (C) > 3 then
               Result := Result + 1;
            end if;
            Next (C);
         end loop;
      end loop;

      D := Clock - Start;
      Ada.Text_IO.Put_Line
         ("Count (predicate is inlined)          =" & Result'Img & " =>" & D'Img);
   end;

   --  Version of count when the Ada05 container is instantiated in another
   --  package

   Co := 0;
   Start := Clock;
   for Dummy in 1 .. Repeat loop
      Co := Co + Count_Separate_Pkg (MV, Greater_Than_3'Access);
   end loop;
   D := Clock - Start;
   Ada.Text_IO.Put_Line
      ("Count (lists in separate package)     ="
       & Co'Img & " =>" & D'Img);

   --  Version of count using Ada12 iterators
   Co := 0;
   Start := Clock;
   for Dummy in 1 .. Repeat loop
      Co := Co + Count_With_Iterator (V, Greater_Than_3'Access);
   end loop;
   D := Clock - Start;
   Ada.Text_IO.Put_Line
      ("Count (ada2012 iterators)             =" & Co'Img & " =>" & D'Img);

   -- Version using generics

   --  declare
   --     package Greater_Than_3_Funcs is new Unary_Functors
   --        (Param_Type => Integer, Return_Type => Boolean,
   --         Call => Greater_Than_3);
   --     function Count is new Generic_Count
   --        (List_Iterators, Greater_Than_3_Funcs);
   --  begin
   --     Co := 0;
   --     Start := Clock;
   --     for Dummy in 1 .. Repeat loop
   --        Co := Co + Count (V.First);
   --     end loop;
   --     D := Clock - Start;
   --     Ada.Text_IO.Put_Line
   --       ("Count (generics)                      =" & Co'Img & " =>" & D'Img);
   --  end;

   -- Version using generics and binders

   --  declare
   --     package Greater_Than_3 is new Binder2nd
   --        (Integer, Integer, 3, Boolean, Greater_Than);
   --     function Count is new Generic_Count
   --        (List_Iterators, Greater_Than_3.Funcs);
   --  begin
   --     Co := 0;
   --     Start := Clock;
   --     for Dummy in 1 .. Repeat loop
   --        Co := Co + Count (V.First);
   --     end loop;
   --     D := Clock - Start;
   --     Ada.Text_IO.Put_Line
   --       ("Count (generics and binders)          =" & Co'Img & " =>" & D'Img);
   --  end;

   --  Version using a hierarchy of containers and interfaces

   declare
      package My_Containers is new Containers_Hierarchy (Integer);
      function Count_Virtual is new Generic_Count_Virtual
         (My_Containers, Greater_Than_3);
      VV : My_Containers.List;
   begin
      Start := Clock;
      for C in 1 .. 10_000_000 loop
         VV.Append (2);
      end loop;
      VV.Append (5);
      VV.Append (6);
      D := Clock - Start;
      Ada.Text_IO.Put_Line ("Tagged type, Fill => " & D'Img);

      Start := Clock;

      Co := 0;
      for Dummy in 1 .. Repeat loop
         Co := Co + Count_Virtual (VV.First);
      end loop;

      D := Clock - Start;
      Ada.Text_IO.Put_Line
         ("Tagged type, Count (virtual methods) =" & Co'Img & " =>" & D'Img);
   end;

end Perf;
