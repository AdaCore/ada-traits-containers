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
with Conts;                         use Conts;
with Conts.Elements.Null_Elements;  use Conts.Elements.Null_Elements;
with Conts.Graphs.Adjacency_List;
with Conts.Graphs.Components; use Conts.Graphs.Components;
with Ada.Text_IO;             use Ada.Text_IO;

procedure Main is

   type Vertex_With_Null is (Null_V, A, B, C, D, E, F, G, H);
   subtype Vertex is Vertex_With_Null range A .. Vertex_With_Null'Last;

   package Graphs is new Conts.Graphs.Adjacency_List
     (Vertex_Type         => Vertex,
      Vertex_Properties   => Conts.Elements.Null_Elements.Traits,
      Edge_Properties     => Conts.Elements.Null_Elements.Traits,
      Container_Base_Type => Ada.Finalization.Controlled);
   use Graphs;

   procedure Strong is new Strongly_Connected_Components
      (Graphs.Traits, Graphs.Integer_Maps.As_Map);

   Gr  : Graphs.Graph;
   Map : Graphs.Integer_Maps.Map;
   Count : Positive;
begin
   Gr.Add_Vertices
      (No_Element,
       Count => Vertex'Pos (Vertex'Last) - Vertex'Pos (Vertex'First) + 1);

   Gr.Add_Edge (A, B, No_Element);
   Gr.Add_Edge (B, C, No_Element);
   Gr.Add_Edge (C, A, No_Element);
   Gr.Add_Edge (D, B, No_Element);
   Gr.Add_Edge (D, C, No_Element);
   Gr.Add_Edge (D, E, No_Element);
   Gr.Add_Edge (E, D, No_Element);
   Gr.Add_Edge (E, F, No_Element);
   Gr.Add_Edge (F, C, No_Element);
   Gr.Add_Edge (F, G, No_Element);
   Gr.Add_Edge (G, F, No_Element);
   Gr.Add_Edge (H, G, No_Element);
   Gr.Add_Edge (H, F, No_Element);
   Gr.Add_Edge (H, H, No_Element);

   Strong (Gr, Map, Components_Count => Count);
   Put_Line ("Found" & Count'Img & " components");

   for V in Vertex loop
      Put_Line ("Component for " & V'Img
         & " is" & Graphs.Integer_Maps.Get (Map, V)'Img);
   end loop;
end Main;
