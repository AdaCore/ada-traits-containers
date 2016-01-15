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

pragma Ada_2012;

with Conts.Cursors;

package Conts.Graphs is

   type Color is (White, Gray, Black);
   --  Used to mark vertices during several algorithms.

   -------------------
   -- Property Maps --
   -------------------
   --  This package describes how to associate values with edges or vertices
   --  or a graph.
   --  In some implementations of graph, the map could be the graph itself,
   --  and the information stored directly in the vertex for instance. In
   --  other cases, one would chose to store this information in vectors
   --  indexed by a unique integer id associated with vertices or edges. In
   --  yet other cases, one could use a map from a vertex to the value.

   generic
      type Key (<>) is private;
      type Value (<>) is private;
   package Property_Maps is
      generic
         type Map (<>) is private;
         with procedure Set (M : in out Map; K : Key; V : Value) is <>;
         with function Get (M : Map; K : Key) return Value is <>;
      package Traits is
      end Traits;
   end Property_Maps;

   ------------
   -- Traits --
   ------------
   --  Abstract description of a graph.
   --  Such a graph has no requirement on iterators, so is not useful for
   --  algorithms.

   generic
      type Graph (<>) is private;
      type Vertex (<>) is private;
      type Edge (<>) is private;
      Null_Vertex : Vertex;

      with function Get_Target (G : Graph; E : Edge) return Vertex is <>;
      --  Return the target of the edge.

   package Traits is
      package Color_Property_Maps is new Property_Maps (Vertex, Color);

      function Never_Stop (G : Graph; V : Vertex) return Boolean
         is (False) with Inline_Always;
      --  A function used to indicate that an algorithm should run until
      --  completion. Various algorithms have a callback that can be used
      --  to stop them before the end.

      generic
         type Cursor is private;
         with function First (G : Graph) return Cursor is <>;
         with function Element (G : Graph; C : Cursor) return Vertex is <>;
         with function Has_Element
            (G : Graph; C : Cursor) return Boolean is <>;
         with function Next (G : Graph; C : Cursor) return Cursor is <>;
      package Vertex_Cursors is
      end Vertex_Cursors;
      --  Iterates over all vertices in the graph

      generic
         type Cursor is private;
         with function First (G : Graph; V : Vertex) return Cursor is <>;
         with function Element (G : Graph; C : Cursor) return Edge is <>;
         with function Has_Element
            (G : Graph; C : Cursor) return Boolean is <>;
         with function Next (G : Graph; C : Cursor) return Cursor is <>;
      package Edge_Cursors is
      end Edge_Cursors;
      --  Iterates over edges in or out of a vertex

      -----------------
      -- DFS_Visitor --
      -----------------

      type DFS_Visitor is interface;
      --  Used to insert actions at various points in the execution of the
      --  Depth-First-Search algorithm.

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

   end Traits;

   ----------------------------
   -- Incidence_Graph_Traits --
   ----------------------------
   --  A graph that provides two iterators: one to find all vertices in the
   --  graph, and another one to find all out edges from these vertices.

   generic
      with package Graphs is new Traits (<>);
      with package Vertices is new Graphs.Vertex_Cursors (<>);
      with package Out_Edges is new Graphs.Edge_Cursors (<>);
   package Incidence_Graph_Traits is
      use Graphs;

      function Default_Start_Vertex (G : Graph) return Vertex;
      --  The default vertex, as a starting point for some algorithms. This
      --  assumes the graph is not empty.

      package Cursors is new Conts.Cursors.Constant_Forward_Traits
         (Container   => Graph,
          Cursor      => Vertices.Cursor,
          Return_Type => Graphs.Vertex,
          First       => Vertices.First,
          Next        => Vertices.Next,
          Has_Element => Vertices.Has_Element,
          Element     => Vertices.Element);
      --  A cursor traits package, that can be used with non-graph specific
      --  algorithms. It iterates over vertices.

   end Incidence_Graph_Traits;

end Conts.Graphs;
