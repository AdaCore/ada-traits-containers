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
with Conts.Elements;
with Conts.Properties;

package Conts.Graphs is

   type Color is (White, Gray, Black);
   --  Used to mark vertices during several algorithms.

   -------------------
   -- Edges cursors --
   -------------------
   --  Iterates on edges in or out of a vertex

   generic
      type Container_Type (<>) is limited private;
      with package Vertices is new Conts.Elements.Traits (<>);
      type Edge_Type (<>) is private;
      type Cursor_Type is private;
      with function First
         (G : Container_Type; V : Vertices.Element_Type)
         return Cursor_Type is <>;
      with function Element (G : Container_Type; C : Cursor_Type)
         return Edge_Type is <>;
      with function Has_Element (G : Container_Type; C : Cursor_Type)
         return Boolean is <>;
      with function Next (G : Container_Type; C : Cursor_Type)
         return Cursor_Type is <>;
   package Edge_Cursors is
      subtype Container is Container_Type;
      subtype Edge      is Edge_Type;
      subtype Cursor    is Cursor_Type;
   end Edge_Cursors;

   ------------
   -- Traits --
   ------------
   --  Abstract description of a graph.
   --  All algorithms need to iterate on all vertices of the graph, and at
   --  least on the out edges of a given vertex, so these two cursors are also
   --  part of these requirements.
   --  Such a traits package can be instantatied for your own data structures
   --  that might have an implicit graph somewhere, even if you do not use an
   --  explicit graph type anywhere.

   generic
      type Graph_Type (<>) is limited private;
      with package Vertices is new Conts.Elements.Traits (<>);
      type Edge_Type (<>) is private;
      Null_Vertex : Vertices.Element;

      with function Get_Target
        (G : Graph_Type; E : Edge_Type) return Vertices.Element is <>;
      --  Return the target of the edge.

      with package Vertex_Cursors is new Conts.Cursors.Forward_Cursors
        (Container_Type => Graph_Type,
         others         => <>);
      with package Vertex_Maps is new Conts.Properties.Read_Only_Maps
        (Key_Type     => Vertex_Cursors.Cursor,
         Element_Type => Vertices.Element,
         Map_Type     => Graph_Type,
         others       => <>);
      --  Iterate on all vertices of the graph

      with package Out_Edges_Cursors is new Edge_Cursors
        (Container_Type => Graph_Type,
         Vertices       => Vertices,
         Edge_Type      => Edge_Type,
         others         => <>);
      --  Iterate on all out-edges of a given vertex.

   package Traits is

      subtype Graph  is Graph_Type;
      subtype Vertex is Vertices.Element;
      subtype Edge   is Edge_Type;
      function Get_Edge_Target
        (G : Graph; E : Edge) return Vertices.Element renames Get_Target;

      -----------------
      -- DFS_Visitor --
      -----------------

      type DFS_Visitor is interface;
      --  Used to insert actions at various points in the execution of the
      --  Depth-First-Search algorithm.

      procedure Should_Stop
        (Self : DFS_Visitor; G : Graph; V : Vertex;
         Stop : in out Boolean) is null;
      --  Whether to stop iterating after discovering vertex V.
      --  If iteration should stop, this procedure should set Stop to True (its
      --  initial value is always False).

      procedure Vertices_Initialized
        (Self  : in out DFS_Visitor;
         G     : Graph;
         Count : Count_Type) is null;
      --  Provide the number of vertices in the graph. This might be used to
      --  reserve_capacity for some internal data for instance.
      --  This is called after all vertices have been initialized via
      --  Initialiez_Vertex.

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

end Conts.Graphs;
