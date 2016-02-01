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
with Conts.Elements.Null_Elements;  use Conts.Elements.Null_Elements;
with Conts.Graphs.Adjacency_List;
with Conts.Graphs.Connected_Components; use Conts.Graphs.Connected_Components;
with Ada.Text_IO;                   use Ada.Text_IO;

procedure Test_Components is

   package Graphs is new Conts.Graphs.Adjacency_List
     (Vertex_Properties => Conts.Elements.Null_Elements.Traits,
      Edge_Properties   => Conts.Elements.Null_Elements.Traits,
      Base_Type         => Ada.Finalization.Controlled);
   use Graphs;

   procedure Strong is new Strongly_Connected_Components
      (Graphs.Incidence_Traits, Graphs.Integer_Maps.As_Exterior);

   G : Graphs.Graph;
   M : Graphs.Integer_Maps.Map;
   Count : Positive;
begin
   G.Add_Vertices (No_Element, Count => 8);

   G.Add_Edge (1, 2, No_Element);
   G.Add_Edge (2, 3, No_Element);
   G.Add_Edge (3, 1, No_Element);
   G.Add_Edge (4, 2, No_Element);
   G.Add_Edge (4, 3, No_Element);
   G.Add_Edge (4, 5, No_Element);
   G.Add_Edge (5, 4, No_Element);
   G.Add_Edge (5, 6, No_Element);
   G.Add_Edge (6, 3, No_Element);
   G.Add_Edge (6, 7, No_Element);
   G.Add_Edge (7, 6, No_Element);
   G.Add_Edge (8, 7, No_Element);
   G.Add_Edge (8, 6, No_Element);
   G.Add_Edge (8, 8, No_Element);

   Strong (G, M, Components_Count => Count);
   Put_Line ("Found" & Count'Img & " components");

   for V in Graphs.Vertex'(1) .. 8 loop
      Put_Line ("Component for" & V'Img
         & " is" & Graphs.Integer_Maps.Get (M, V)'Img);
   end loop;
end Test_Components;
