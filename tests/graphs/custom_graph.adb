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
with Ada.Finalization;
with Graph1_Support;                use Graph1_Support;
with Conts.Elements.Null_Elements;  use Conts.Elements.Null_Elements;
with Conts.Graphs.Adjacency_List;
with Conts.Graphs.Components;       use Conts.Graphs.Components;
with Report;                        use Report;
with Perf_Support;
with Ada.Text_IO;                   use Ada.Text_IO;

package body Custom_Graph is

   Debug : constant Boolean := False;

   -------------------------
   -- Test_Adjacency_List --
   -------------------------

   procedure Test_Adjacency_List (Stdout : not null access Output'Class) is
      package Graphs is new Conts.Graphs.Adjacency_List
        (Vertex_Type       => Positive,
         Vertex_Properties => Conts.Elements.Null_Elements.Traits,
         Edge_Properties   => Conts.Elements.Null_Elements.Traits,
         Base_Type         => Ada.Finalization.Controlled);
      use Graphs;

      type My_Visitor is new Graphs.Traits.DFS_Visitor with null record;
      overriding procedure Finish_Vertex
        (Self : in out My_Visitor; G : Graphs.Graph; V : Graphs.Vertex);

      overriding procedure Finish_Vertex
        (Self : in out My_Visitor; G : Graphs.Graph; V : Graphs.Vertex)
      is
         pragma Unreferenced (Self, G);
      begin
         if Debug then
            Put_Line ("Finish vertex " & V'Img);
         end if;
      end Finish_Vertex;

      procedure DFS is new Graphs.DFS.Search (My_Visitor);
      procedure Strong is new Strongly_Connected_Components
         (Graphs.Traits, Graphs.Integer_Maps.As_Map);

      subtype Vertex is Graphs.Vertex;
      use type Vertex;

   begin
      Stdout.Start_Container_Test
        (Base     => "adjacency list",
         Elements => "",
         Nodes    => "",
         Category => "Graph");

      for C in 1 .. Perf_Support.Repeat_Count loop
         declare
            G     : Graphs.Graph;
            Vis   : My_Visitor;
         begin
            Stdout.Save_Container_Size (G'Size / 8);

            Stdout.Start_Test ("fill", Start_Group => True);
            G.Add_Vertices (No_Element, Count => Perf_Support.Items_Count);

            for V in Vertex'First
              .. Vertex'First + Vertex (Perf_Support.Items_Count) - 2
            loop
               G.Add_Edge (V, V + 1, No_Element);
            end loop;
            Stdout.End_Test;

            Stdout.Start_Test ("dfs, no visitor", Start_Group => True);
            DFS (G, Vis);
            Stdout.End_Test;

            declare
               M     : Graphs.Integer_Maps.Map :=
                  Graphs.Integer_Maps.Create_Map (G);
               Count : Integer;
            begin
               G.Add_Edge (Perf_Support.Items_Count / 10 + 1, 4, No_Element);
               G.Add_Edge (2 * Perf_Support.Items_Count / 10 + 1,
                           Perf_Support.Items_Count, No_Element);

               Stdout.Start_Test ("scc", Start_Group => True);
               --  M.Values.Reserve_Capacity (G.Length);
               Strong (G, M, Components_Count => Count);
               Stdout.End_Test;
            end;

            G.Clear;
         end;
      end loop;

      Stdout.End_Container_Test;
   end Test_Adjacency_List;

   -----------------
   -- Test_Custom --
   -----------------

   procedure Test_Custom (Stdout : not null access Output'Class) is
      procedure Search is new Graph1_Support.DFS.Search (My_Visitor);
      procedure Search is new Graph1_Support.DFS.Search (My_Visitor2);
      procedure Recursive is
        new Graph1_Support.DFS.Search_Recursive (My_Visitor2);

   begin
      Stdout.Start_Container_Test
         (Base     => "custom graph",
          Elements => "",
          Nodes    => "",
          Category => "Graph");

      for C in 1 .. Perf_Support.Repeat_Count loop
         declare
            G     : Graph;
            V     : My_Visitor;
            V2    : My_Visitor2;
         begin
            Stdout.Save_Container_Size (G'Size / 8);

            Stdout.Start_Test ("dfs, no visitor", Start_Group => True);
            Search (G, V);
            Stdout.End_Test;

            Stdout.Start_Test ("dfs, visitor");
            Search (G, V2);
            Stdout.End_Test;

            Stdout.Start_Test ("dfs-recursive, visitor");
            Recursive (G, V2);
            Stdout.End_Test;
         end;
      end loop;

      Stdout.End_Container_Test;
   end Test_Custom;

end Custom_Graph;
