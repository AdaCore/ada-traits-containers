------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Support for output of the tests

with Ada.Unchecked_Conversion;
with Ada.Calendar;    use Ada.Calendar;
with GNATCOLL.JSON;   use GNATCOLL.JSON;

package Report is

   type Output is tagged private;

   procedure Start_Container_Test
      (Self       : not null access Output'Class;
       Base       : String;    --  "controlled", "limited", ...
       Elements   : String;    --  "definite", "indefinite", ...
       Nodes      : String;    --  "bounded", "unbounded", ...
       Container  : String;    --  "list", "vector",..
       E_Type     : String);   --  "integer", "string", ...

   procedure End_Container_Test (Self : not null access Output'Class);
   --  A new set of tests is started, for a specific container.
   --  This automatically starts measuring the number of allocation and
   --  deallocations done by that test.

   procedure Start_Test
      (Self    : not null access Output'Class;
       Name    : String;
       Comment : String := "");
   procedure End_Test (Self : not null access Output'Class);
   --  A test on the current container is executed. These procedures measure
   --  the execution time. Calling End_Test is optional if you are calling
   --  Start_Test immediately.

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
      Start_Time     : Ada.Calendar.Time;
   end record;

end Report;
