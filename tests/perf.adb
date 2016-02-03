------------------------------------------------------------------------------
--                     Copyright (C) 2015-2016, AdaCore                     --
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

pragma Ada_2012;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.Strings;     use GNAT.Strings;
with GNATCOLL.Utils;   use GNATCOLL.Utils;
with Perf_Support;     use Perf_Support;
with QGen;             use QGen;
with Report;           use Report;
with System;

--  Integer lists

with List_Ada12_Def_Unbounded_Integer;
with List_Ada12_Def_Bounded_Integer;
with List_Ada12_No_Checks_Def_Unbounded_Integer;
with List_Ada12_Indef_Unbounded_Integer;
with List_Controlled_Indef_Unbounded_Integer;
with List_Controlled_Def_Unbounded_Integer;
with List_Controlled_Def_Bounded_Integer;
with List_Limited_Def_Bounded_Integer;
with List_Limited_Indef_Spark_Unbounded_Spark_Integer;

--  String lists

with List_Ada12_Indef_Unbounded_String;
with List_Ada12_No_Checks_Indef_Unbounded_String;
with List_Controlled_Indef_Unbounded_String;
with List_Controlled_Indef_Unbounded_Ref_String;
with List_Controlled_Def_Unbounded_Unbounded_String;
with List_Controlled_Strings_Specific_Unbounded_String;

--  Integer vectors

with Vector_Ada12_Def_Unbounded_Integer;
with Vector_Ada12_Def_Bounded_Integer;
with Vector_Ada12_No_Checks_Def_Unbounded_Integer;
with Vector_Ada12_Indef_Unbounded_Integer;
with Vector_Controlled_Indef_Unbounded_Integer;
with Vector_Controlled_Def_Unbounded_Integer;
with Vector_Controlled_Def_Bounded_Integer;
with Vector_Limited_Def_Bounded_Integer;

--  String vectors

with Vector_Ada12_Indef_Unbounded_String;
with Vector_Ada12_No_Checks_Indef_Unbounded_String;
with Vector_Controlled_Indef_Unbounded_String;
with Vector_Controlled_Indef_Unbounded_Ref_String;

--  Integer-Integer maps

with Map_Ada12_ordered_Def_Def_Unbounded_IntInt;
with Map_Ada12_hashed_Def_Def_Unbounded_IntInt;
with Map_hashed_Def_Def_Unbounded_IntInt;

--  String-String maps

with Map_Ada12_ordered_Indef_Indef_Unbounded_StrStr;
with Map_Ada12_hashed_Indef_Indef_Unbounded_StrStr;
with Map_hashed_Indef_Indef_Unbounded_StrStr;

--  Graphs

with Custom_Graph;

procedure Perf is
   procedure Test_Cpp_Graph (Stdout : System.Address)
      with Import, Convention => C, External_Name => "test_cpp_graph";

   Test_Name : String_Access;
   Stdout : aliased Output;

   type CPP_Test is not null access procedure (S : System.Address)
      with Convention => C;

   procedure Run_Test
      (Name : String;
       Proc : not null access procedure (S : not null access Output'Class));
   procedure Run_Test (Name : String; Proc : CPP_Test);
   --  Run a test if the command line arguments allow it

   procedure Run_Test
      (Name : String;
       Proc : not null access procedure (S : not null access Output'Class)) is
   begin
      if Test_Name = null
         or else Starts_With (Name, Test_Name.all)
      then
         Put_Line ("Run " & Name);
         Proc (Stdout'Access);
      end if;
   end Run_Test;

   procedure Run_Test (Name : String; Proc : CPP_Test) is
   begin
      if Test_Name = null
         or else Starts_With (Name, Test_Name.all)
      then
         Put_Line ("Run " & Name);
         Proc (Stdout'Address);
      end if;
   end Run_Test;

begin
   if Ada.Command_Line.Argument_Count >= 1 then
      Test_Name := new String'(Ada.Command_Line.Argument (1));
   end if;

   Run_Test ("int_list_c++", Test_Cpp_Int_List'Access);
   Run_Test ("int_list_ada_def_bounded",
             List_Ada12_Def_Bounded_Integer'Access);
   Run_Test ("int_list_ada_def_unbounded",
             List_Ada12_Def_Unbounded_Integer'Access);
   Run_Test ("int_list_ada_def_unbounded_nochecks",
             List_Ada12_No_Checks_Def_Unbounded_Integer'Access);
   Run_Test ("int_list_ada_indef_unbounded",
             List_Ada12_Indef_Unbounded_Integer'Access);
   Run_Test ("int_list_controlled_indef_unbounded",
             List_Controlled_Indef_Unbounded_Integer'Access);
   Run_Test ("int_list_controlled_def_unbounded",
             List_Controlled_Def_Unbounded_Integer'Access);
   Run_Test ("int_list_controlled_def_bounded",
             List_Controlled_Def_Bounded_Integer'Access);
   Run_Test ("int_list_limited_def_bounded",
             List_Limited_Def_Bounded_Integer'Access);
   Run_Test ("int_list_spark_indef_unbounded",
             List_Limited_Indef_Spark_Unbounded_Spark_Integer'Access);

   Run_Test ("str_list_c++", Test_Cpp_Str_List'Access);
   Run_Test ("str_list_ada_indef_unbounded",
             List_Ada12_Indef_Unbounded_String'Access);
   Run_Test ("str_list_ada_indef_unbounded_nochecks",
             List_Ada12_No_Checks_Indef_Unbounded_String'Access);
   Run_Test ("str_list_controlled_indef_unbounded",
             List_Controlled_Indef_Unbounded_String'Access);
   Run_Test ("str_list_controlled_indef_unbounded_ref",
             List_Controlled_Indef_Unbounded_Ref_String'Access);
   Run_Test ("str_list_controlled_def_unbounded_ustr",
             List_Controlled_Def_Unbounded_Unbounded_String'Access);

   if False then
      --  Valgrind errors
      Run_Test ("str_list_controlled_strings_specific_ustr",
                List_Controlled_Strings_Specific_Unbounded_String'Access);
   end if;

   Run_Test ("int_vector_c++", Test_Cpp_Int_Vector'Access);
   Run_Test ("int_vector_ada_arrays", Test_Arrays_Int'Access);
   Run_Test ("int_vector_ada_def_bounded",
             Vector_Ada12_Def_Bounded_Integer'Access);
   Run_Test ("int_vector_ada_def_unbounded",
             Vector_Ada12_Def_Unbounded_Integer'Access);
   Run_Test ("int_vector_ada_def_unbounded_nochecks",
             Vector_Ada12_No_Checks_Def_Unbounded_Integer'Access);
   Run_Test ("int_vector_ada_indef_unbounded",
             Vector_Ada12_Indef_Unbounded_Integer'Access);
   Run_Test ("int_vector_controlled_indef_unbounded",
             Vector_Controlled_Indef_Unbounded_Integer'Access);
   Run_Test ("int_vector_controlled_def_unbounded",
             Vector_Controlled_Def_Unbounded_Integer'Access);
   Run_Test ("int_vector_controlled_def_bounded",
             Vector_Controlled_Def_Bounded_Integer'Access);
   Run_Test ("int_vector_limited_def_bounded",
             Vector_Limited_Def_Bounded_Integer'Access);

   Run_Test ("str_vector_c++", Test_Cpp_Str_Vector'Access);
   Run_Test ("str_vector_ada_indef_unbounded",
             Vector_Ada12_Indef_Unbounded_String'Access);
   Run_Test ("str_vector_ada_indef_unbounded_nochecks",
             Vector_Ada12_No_Checks_Indef_Unbounded_String'Access);
   Run_Test ("str_vector_controlled_indef_unbounded",
             Vector_Controlled_Indef_Unbounded_String'Access);
   Run_Test ("str_vector_controlled_indef_unbounded_ref",
             Vector_Controlled_Indef_Unbounded_Ref_String'Access);

   Run_Test ("intint_map_c++_unordered",
             Test_Cpp_Int_Int_Unordered_Map'Access);
   Run_Test ("intint_map_c++", Test_Cpp_Int_Int_Map'Access);
   Run_Test ("intint_map_ada_ordered_indef_indef",
             Map_Ada12_ordered_Def_Def_Unbounded_IntInt'Access);
   Run_Test ("intint_map_ada_hashed_indef_indef",
             Map_Ada12_hashed_Def_Def_Unbounded_IntInt'Access);
   Run_Test ("intint_map_hashed_indef_indef",
             Map_hashed_Def_Def_Unbounded_IntInt'Access);

   Run_Test ("strstr_map_c++_unordered",
             Test_Cpp_Str_Str_Unordered_Map'Access);
   Run_Test ("strstr_map_c++", Test_Cpp_Str_Str_Map'Access);
   Run_Test ("strstr_map_ada_ordered_indef_indef",
             Map_Ada12_ordered_Indef_Indef_Unbounded_StrStr'Access);
   Run_Test ("strstr_map_ada_hashed_indef_indef",
             Map_Ada12_hashed_Indef_Indef_Unbounded_StrStr'Access);
   Run_Test ("strstr_map_hashed_indef_indef",
             Map_hashed_Indef_Indef_Unbounded_StrStr'Access);

   Run_Test ("graph_c++", Test_Cpp_Graph'Access);
   Run_Test ("graph_ada_custom", Custom_Graph.Test_Custom'Access);
   Run_Test ("graph_ada_adjacency_list",
             Custom_Graph.Test_Adjacency_List'Access);

   Test_QGen;

   Stdout.Display;

   Free (Test_Name);
end Perf;
