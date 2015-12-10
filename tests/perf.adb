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

with Perf_Support;    use Perf_Support;
with QGen;            use QGen;
with Report;          use Report;
with Ada.Text_IO;     use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with List_Ada12_Def_Unbounded_Integer;
with List_Ada12_Def_Bounded_Integer;
with List_Ada12_No_Checks_Def_Unbounded_Integer;
with List_Ada12_Indef_Unbounded_Integer;
with List_Controlled_Indef_Unbounded_Integer;
with List_Controlled_Def_Unbounded_Integer;
with List_Controlled_Def_Bounded_Integer;
with List_Limited_Def_Bounded_Integer;
with List_Limited_Indef_Spark_Unbounded_Spark_Integer;
with List_Ada12_Indef_Unbounded_String;
with List_Ada12_No_Checks_Indef_Unbounded_String;
with List_Controlled_Indef_Unbounded_String;
with List_Controlled_Indef_Unbounded_Ref_String;
with List_Controlled_Def_Unbounded_Unbounded_String;
with List_Controlled_Arrays_Unbounded_String;
with Map_Ada12_ordered_Indef_Indef_Unbounded_StrStr;
with Map_Ada12_hashed_Indef_Indef_Unbounded_StrStr;

procedure Perf is
   Stdout : aliased Output;
   S      : constant access Output'Class := Stdout'Access;
begin
   Test_Cpp_Int_List (Stdout'Address);
   Test_Arrays_Int (S);
   List_Ada12_Def_Bounded_Integer (S);
   List_Ada12_Def_Unbounded_Integer (S);
   List_Ada12_No_Checks_Def_Unbounded_Integer (S);
   List_Ada12_Indef_Unbounded_Integer (S);
   List_Controlled_Indef_Unbounded_Integer (S);
   List_Controlled_Def_Unbounded_Integer (S);
   List_Controlled_Def_Bounded_Integer (S);
   List_Limited_Def_Bounded_Integer (S);
   List_Limited_Indef_Spark_Unbounded_Spark_Integer (S);

   Test_Cpp_Str_List (Stdout'Address);
   List_Ada12_Indef_Unbounded_String (S);
   List_Ada12_No_Checks_Indef_Unbounded_String (S);
   List_Controlled_Indef_Unbounded_String (S);
   List_Controlled_Indef_Unbounded_Ref_String (S);
   List_Controlled_Def_Unbounded_Unbounded_String (S);
   List_Controlled_Arrays_Unbounded_String (S);

   Test_Cpp_Str_Str_Map (Stdout'Address);
   Test_Cpp_Str_Str_Unordered_Map (Stdout'Address);
   Map_Ada12_ordered_Indef_Indef_Unbounded_StrStr (S);
   Map_Ada12_hashed_Indef_Indef_Unbounded_StrStr (S);

   Test_QGen;
   Stdout.Display;

   Put_Line ("open file://" & Get_Current_Dir & "/index.html");
end Perf;
