--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Support for output of the tests

with Ada.Unchecked_Conversion;
with Ada.Calendar;    use Ada.Calendar;
with GNATCOLL.JSON;   use GNATCOLL.JSON;
with Memory;          use Memory;

package Report is

   type Output is tagged private;

   procedure Start_Container_Test
      (Self       : not null access Output'Class;
       Name       : String;
       Category   : String;    --  "integer list", "string list", ...
       Favorite   : Boolean := False);
   --  If Favorite is true, the container will be highlighted in the results

   procedure Save_Container_Size
     (Self        : not null access Output'Class;
      Size        : Long_Integer);
   --  Saves the size of the container in the output (for information only)

   procedure End_Container_Test (Self : not null access Output'Class);
   --  A new set of tests is started, for a specific container.
   --  This automatically counts the number of allocation and
   --  deallocations done by that test.

   procedure Start_Test
      (Self        : not null access Output'Class;
       Name        : String;
       Comment     : String := "";
       Start_Group : Boolean := False);
   procedure End_Test (Self : not null access Output'Class);
   --  A test on the current container is executed. These procedures measure
   --  the execution time. Calling End_Test is optional if you are calling
   --  Start_Test immediately.
   --  You can run the same test multiple times after calling
   --  Start_Container_Test. All timings will be recorded.
   --  Tests are grouped, so that the first test run in a group, for the
   --  first container, is displayed as "100%", and other tests in the same
   --  group are displayed relative to this one.
   --  Set Start_Group to True to start a new group. All following tests
   --  will belong to the same group, until a test that also sets Start_Group
   --  to True.

   procedure End_Test_Not_Run (Self : not null access Output'Class);
   --  Same as End_Test, but mark the test as "NOT RUN".

   procedure Display (Self : not null access Output'Class);
   --  Outputs the results to a JSON file

   --  generic
   --     type Container (<>) is limited private;
   --     with procedure Run
   --    (Self : in out Container; Col : Column_Number; Start : Time) is <>;
   --  procedure Run_Tests
   --     (Stdout      : in out Output'Class;
   --      Title       : String;
   --      Self        : in out Container;
   --      Fewer_Items : Boolean := False);
   --  For each column defined in Stdout and associated with a test, executes
   --  Run. Run can either print some output via Stdout.Print_Line (for
   --  instance), or let this procedure print the time on its own.

private
   type Output is tagged record
      Global_Result  : JSON_Value := JSON_Null;
      All_Tests      : JSON_Array;

      Container_Test : JSON_Value := JSON_Null;
      Tests_In_Container : JSON_Value;

      Current_Test   : JSON_Value := JSON_Null;
      At_Test_Start  : Mem_Info;
      Start_Time     : Ada.Calendar.Time;
   end record;

end Report;
