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
with Conts.Elements.Definite;
with Conts.Graphs;      use Conts.Graphs;

package Graph1_Support is

   ------------
   -- Graphs --
   ------------

   type Vertex is new Integer;
   package Vertices is new Conts.Elements.Definite (Vertex);

   type Edge is record
      Source, Target : Vertex;
   end record;

   type Color_Map is array (Vertex range <>) of Color;

   type Graph is record
      Colors : Color_Map (1 .. 1_000_000);
   end record;

   function Get_Target (G : Graph; E : Edge) return Vertex;
   package Base_Graphs is new Conts.Graphs.Traits
      (Graph       => Graph,
       Vertices    => Vertices.Traits,
       Null_Vertex => -1,
       Edge        => Edge);

   procedure Set_Color (G : in out Graph; V : Vertex; C : Color);
   function Get_Color (G : Graph; V : Vertex) return Color;

   package Color_Maps is new Base_Graphs.Color_Property_Maps.Interior
      (Set_Color, Get_Color);

   --------------------
   -- Vertex_Cursors --
   --------------------

   type Vertex_Cursor is new Integer;
   function First (G : Graph) return Vertex_Cursor with Inline;
   function Element (G : Graph; C : Vertex_Cursor) return Vertex with Inline;
   function Has_Element
      (G : Graph; C : Vertex_Cursor) return Boolean with Inline;
   function Next
      (G : Graph; C : Vertex_Cursor) return Vertex_Cursor with Inline;

   package Custom_Vertices is new Base_Graphs.Vertex_Cursors (Vertex_Cursor);

   ------------------
   -- Edge_Cursors --
   ------------------
   --  Not a very interesting graph
   --     1 -> 2 -> 3 -> 4 -> 5 -> ...

   type Edge_Cursor is new Integer;
   function First (G : Graph; V : Vertex) return Edge_Cursor with Inline;
   function Element (G : Graph; C : Edge_Cursor) return Edge with Inline;
   function Has_Element
      (G : Graph; C : Edge_Cursor) return Boolean with Inline;
   function Next
      (G : Graph; C : Edge_Cursor) return Edge_Cursor with Inline;

   package Custom_Edges is new Base_Graphs.Edge_Cursors (Edge_Cursor);

   ----------------------
   -- Incidence_Graphs --
   ----------------------

   package Custom_Graphs is new Incidence_Graph_Traits
      (Base_Graphs, Custom_Vertices, Custom_Edges);

   ----------------
   -- Algorithms --
   ----------------

   type My_Visitor is new Base_Graphs.DFS_Visitor with null record;

   type My_Visitor2 is new Base_Graphs.DFS_Visitor with null record;
   overriding procedure Initialize_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex);
   overriding procedure Start_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex);
   overriding procedure Finish_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex);
   overriding procedure Discover_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex);

end Graph1_Support;
