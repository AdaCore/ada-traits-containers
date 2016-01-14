------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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

--  This package provides Depth-First-Search algorithms on graphs
--
--  A Depth First Search visits all vertices in a graph exactly once.
--  This algorithm always choses to go deeper into the graph by looking
--  at the next adjacent undiscovered vertex until reaching a vertex that
--  has no undiscovered adjacent vertice. It then backtraces to the
--  previous vertex.
--  After visiting all vertices reachable from a source vertex, it then
--  moves on to one of the remaining unvisited vertices, and searches
--  again.
--  This generates a set of trees.
--
--  Depth First Search is a basic block for a lot of other algorithms.
--  However, it is also useful on its own:
--    * compute whether a vertex is reachable from another vertex
--    * detect cycles in a graph
--
--  This algorithm needs to mark visited vertices with colors.
--  Complexity:  O( |edges| + |vertices| )
--
--  This algorithm returns nothing. All actions happen through a visitor.

pragma Ada_2012;

generic
   with package Graphs is new Conts.Graphs.Traits (<>);
package Conts.Graphs.DFS is
   use Graphs;

   -----------------
   -- DFS_Visitor --
   -----------------

   type DFS_Visitor is interface;
   --  Used to insert actions at various points in the execution of the
   --  Depth-First-Search algorithm.
   --  Thanks to generics, there is only an impact on performance for the
   --  subprograms you actually override.

   procedure Initialize_Vertex
      (Self : in out DFS_Visitor; G : Graph; V : Vertex) is null;
   --  Called on every vertex before the start of the search

   procedure Start_Vertex
      (Self : in out DFS_Visitor; G : Graph; V : Vertex) is null;
   --  Called on a source vertex once before the start of the search.
   --  All vertices reachable from the source will not be source vertices
   --  themselves, so will not be called for Start_Vertex.

   procedure Finish_Vertex
      (Self : in out DFS_Visitor; G : Graph; V : Vertex) is null;
   --  Called on every vertex after all its out edges have been added to the
   --  search tree and its adjacent vertices have been visited.

   procedure Discover_Vertex
      (Self : in out DFS_Visitor; G : Graph; V : Vertex) is null;
   --  Called when a vertex is encountered the first time.

   procedure Examine_Edge
      (Self : in out DFS_Visitor; G : Graph; E : Edge) is null;
   --  Called for every out edge of every vertex, after it is discovered.

   procedure Tree_Edge
      (Self : in out DFS_Visitor; G : Graph; E : Edge) is null;
   --  Called on each edge when it becomes a member of the edges that form
   --  a spanning tree (i.e. for out edges that do not lead to an already
   --  visited vertex)

   procedure Back_Edge
      (Self : in out DFS_Visitor; G : Graph; E : Edge) is null;
   --  Called on the back edges of the graph.
   --  These are the edges for which Tree_Edge is not called.
   --  For an undirected graph, there is an ambiguity between Back_Edge and
   --  Tree_Edge, so both are called.

   procedure Forward_Or_Cross_Edge
      (Self : in out DFS_Visitor; G : Graph; E : Edge) is null;
   --  Called on forward or cross edges, unused for undirected

   procedure Finish_Edge
      (Self : in out DFS_Visitor; G : Graph; E : Edge) is null;
   --  Called when the algorithm finishes processing an edge

   ------------------------
   -- Depth First Search --
   ------------------------

   function Never_Stop (G : Graph; V : Vertex) return Boolean
       is (False) with Inline;

   generic
      type Visitor (<>) is new DFS_Visitor with private;
      with package Maps is new Color_Property_Maps.Traits (<>);
      with function Terminator (G : Graph; V : Vertex) return Boolean
         is Never_Stop;
   procedure Search_From_Vertex_With_Map
      (G : Graph; Visit : in out Visitor; Map : in out Maps.Map; V : Vertex);
   --  Depth-First-Search
   --  This version stores the vertices colors in an external map.
   --  It starts with vertex V.

   generic
      type Visitor (<>) is new DFS_Visitor with private;
      with package Maps is new Color_Property_Maps.Traits (<>);
      with function Terminator (G : Graph; V : Vertex) return Boolean
         is Never_Stop;
   procedure Search_With_Map
      (G : Graph; Visit : in out Visitor; Map : in out Maps.Map)
      with Inline;
   --  Depth-First-Search
   --  This version stores the vertices colors in an external map.
   --  It starts with a random vertex.

   generic
      type Visitor is new DFS_Visitor with private;
      with procedure Set_Color (G : in out Graph; V : Vertex; C : Color) is <>;
      with function Get_Color (G : Graph; V : Vertex) return Color is <>;
      with function Terminator (G : Graph; V : Vertex) return Boolean
         is Never_Stop;
   procedure Search (G : in out Graph; Visit : in out Visitor)
      with Inline;
   --  Depth-First-Search
   --  This version stores the vertices colors in the graph itself.
   --  It starts with a random vertex.

   generic
      type Visitor is new DFS_Visitor with private;
      with procedure Set_Color (G : in out Graph; V : Vertex; C : Color) is <>;
      with function Get_Color (G : Graph; V : Vertex) return Color is <>;
      with function Terminator (G : Graph; V : Vertex) return Boolean
         is Never_Stop;
   procedure Search_From_Vertex
      (G : in out Graph; Visit : in out Visitor; V : Vertex)
      with Inline;
   --  Depth-First-Search
   --  This version stores the vertices colors in the graph itself.
   --  It starts with vertex V.

end Conts.Graphs.DFS;
