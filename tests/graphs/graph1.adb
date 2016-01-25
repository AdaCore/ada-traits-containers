------------------------------------------------------------------------------
--                     Copyright (C) 2016-2016, AdaCore                     --
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
with Graph1_Support;     use Graph1_Support;
with Conts.Graphs.DFS;
with Report;             use Report;
with Perf_Support;
with System;

procedure Graph1 is
   procedure DFS is new Conts.Graphs.DFS.Search
      (Custom_Graphs, My_Visitor, Color_Maps);

   procedure Recursive_DFS is new Conts.Graphs.DFS.Recursive_Search
      (Custom_Graphs, My_Visitor2, Color_Maps);
   procedure DFS is new Conts.Graphs.DFS.Search
      (Custom_Graphs, My_Visitor2, Color_Maps);

   procedure Test_Cpp_Graph (Stdout : System.Address)
      with Import, Convention => C, External_Name => "test_cpp_graph";

   V      : My_Visitor;
   V2     : My_Visitor2;
   Stdout : aliased Report.Output;
begin
   Test_Cpp_Graph (Stdout'Address);

   Stdout.Start_Container_Test
      (Base     => "controlled",
       Elements => "definite",
       Nodes    => "unbounded",
       Category => "Graph");

   for C in 1 .. Perf_Support.Repeat_Count loop
      declare
         G     : Graph;
      begin
         Stdout.Save_Container_Size (G'Size / 8);
         Stdout.Start_Test ("dfs, no visitor", Start_Group => True);
         DFS (G, V, 3);
         Stdout.End_Test;

         Stdout.Start_Test ("dfs, visitor");
         DFS (G, V2);
         Stdout.End_Test;

         Stdout.Start_Test ("dfs-recursive, visitor");
         Recursive_DFS (G, V2);
         Stdout.End_Test;
      end;
   end loop;

   Stdout.End_Container_Test;

   Stdout.Display;
end Graph1;
