--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
