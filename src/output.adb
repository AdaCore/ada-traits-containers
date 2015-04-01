with Ada.Text_IO;        use Ada.Text_IO;
with GNAT.Strings;
with Interfaces.C.Strings;
with Memory;
with Perf_Support;       use Perf_Support;

package body Output is

   procedure Put (Self : in out Output; Str : String);
   --  Display text in the current column

   type Column_Descriptor is record
      Title          : GNAT.Strings.String_Access;
      Width          : Natural;
      Wide_Separator : Boolean;
      Ref            : Time_Ref;
   end record;
   Columns : array (All_Cols) of Column_Descriptor :=
      (Column_Title    => (new String'(""),         10, True,  Ref_None),
       Column_Fill     => (new String'("fill"),     6,  False, Ref_Fill),
       Column_Copy     => (new String'("copy"),     6,  True,  Ref_Fill),
       Column_Loop     => (new String'("loop"),     5,  False, Ref_Loop),
       Column_For_Of   => (new String'("for..of"),  8,  False, Ref_Loop),
       Column_Count_If => (new String'("count"),    5,  True,  Ref_Loop),
       Column_Allocate => (new String'("allocate"), 8,  False, Ref_None),
       Column_Allocs   => (new String'("allocs"),   8,  False, Ref_None),
       Column_Reallocs => (new String'("real"),     4,  False, Ref_None),
       Column_Frees    => (new String'("frees"),    8,  True,  Ref_None));

   procedure Reset (Self : in out Output) is
   begin
      Self.Finish_Line;
      Self.Ref := (others => 0.0);
   end Reset;

   procedure Print_Header (Self : in out Output) is
   begin
      Self.Finish_Line;
      Self.Current := Column_Title;
      for C in Columns'Range loop
         Put (Self, Columns (C).Title.all);
      end loop;
      New_Line;
      Self.Current := Column_Title;
   end Print_Header;

   procedure Put (Self : in out Output; Str : String) is
   begin
      Put (Str & (Str'Length + 1 .. Columns (Self.Current).Width => ' '));
      if not Self.Basic and then Columns (Self.Current).Wide_Separator then
         Put (Character'Val (16#E2#)
              & Character'Val (16#95#)
              & Character'Val (16#91#));
      else
         Put ('|');
      end if;

      if Self.Current /= All_Cols'Last then
         Self.Current := All_Cols'Succ (Self.Current);
      end if;
   end Put;

   procedure Start_Line
      (Self : in out Output; Title : String; Fewer_Items : Boolean := False) is
   begin
      Self.Finish_Line;
      Memory.Reset;
      Self.Current := Column_Title;
      Put (Self, Title);
      Self.Fewer_Items := Fewer_Items;
   end Start_Line;

   procedure Finish_Line (Self : in out Output) is
   begin
      if Self.Current /= Column_Title then
         while Self.Current /= Column_Frees loop
            Put (Self, "");
         end loop;
         Put (Self, Memory.Frees'Img);
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
         Ref := Self.Ref (Columns (Self.Current).Ref);
         if Ref = 0.0 then
            Self.Ref (Columns (Self.Current).Ref) := D;
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

   procedure Print_Not_Run (Self : in out Output; Extra : String := "") is
   begin
      Put (Self, Extra);
   end Print_Not_Run;

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
end Output;
