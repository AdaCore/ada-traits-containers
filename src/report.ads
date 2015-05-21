--  Support for output of the tests

with Ada.Unchecked_Conversion;
with Ada.Calendar;    use Ada.Calendar;
with GNAT.Strings;    use GNAT.Strings;
with System;

package Report is

   type Performance_Counter is new Integer;
   type Reference_Times is array (Performance_Counter range <>) of Duration;
   type Reference_Times_Access is access Reference_Times;
   --  An enumeration type that lists the various counters needed to
   --  compare execution times. For instance, one entry would match
   --  all the "loops" tests (the first test executed in that category
   --  is set to 100%, and all others are displayed relative to this one)

   Last_Column_With_Test : constant Performance_Counter := -1;
   --  Tests execute until the first column whose reference counter is this
   --  value (or until the last column)

   type Column_Number is new Natural;
   type Column_Descriptor is record
      Title          : GNAT.Strings.String_Access;
      Width          : Natural;  --  in characters
      Wide_Separator : Boolean;
      Ref            : Performance_Counter;
   end record;
   type Columns_Array is array (Column_Number range <>) of Column_Descriptor;
   type Columns_Array_Access is access Columns_Array;
   --  First column must always be for the title of the row

   type Output is tagged private;

   procedure Setup
      (Self           : in out Output;
       Counters_Count : Performance_Counter;
       Columns        : Columns_Array;
       Show_Percent   : Boolean := True);
   procedure Print_Header (Self : in out Output);
   procedure Reset (Self : in out Output);
   procedure Start_Line
      (Self : in out Output; Title : String; Fewer_Items : Boolean := False);
   procedure Print_Time
      (Self : in out Output; D : Duration; Extra : String := "");
   procedure Print_Not_Run (Self : in out Output; Extra : String := "");
   procedure Print_Size (Self : in out Output; Size : Natural);
   procedure Finish_Line (Self : in out Output);

   procedure Finalize (Self : in out Output);

   generic
      type Container (<>) is limited private;
      with procedure Run
         (Self : in out Container; Col : Column_Number; Start : Time) is <>;
   procedure Run_Tests
      (Stdout      : in out Output'Class;
       Title       : String;
       Self        : in out Container;
       Fewer_Items : Boolean := False);
   --  For each column defined in Stdout and associated with a test, executes
   --  Run. Run can either print some output via Stdout.Print_Line (for
   --  instance), or let this procedure print the time on its own.

   type Output_Access is access all Output'Class;
   pragma No_Strict_Aliasing (Output_Access);
   function To_Address is new Ada.Unchecked_Conversion
      (Output_Access, System.Address);
   --  Interface to C++

private
   type Output is tagged record
      Columns : Columns_Array_Access;
      Ref     : Reference_Times_Access;

      Show_Percent : Boolean := True;

      Basic        : Boolean := False;
      --  If true, only ASCII characters are used

      Current      : Column_Number := 1;

      Fewer_Items  : Boolean;
      --  Whether the test is run on fewer items
   end record;

end Report;
