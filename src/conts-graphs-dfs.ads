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

--  This package provides various algorithms that do a single depth first
--  search traversal, and return various results.
--
--  The three packages below behave the same (and in fact share implementation)
--  but they apply to different kinds of property maps. So only the first
--  package is fully documented.

pragma Ada_2012;
with Conts.Properties;

package Conts.Graphs.DFS is

   --  Depth-First-Search
   --  The map is given explicitly as a parameter, so that we do not need the
   --  Create_Map function and can use this for both interior and exterior
   --  maps, or with types of maps that cannot easily be returned from a
   --  function.

   generic
      with package Graphs is new Conts.Graphs.Traits (<>);
      with package Color_Maps is new Conts.Properties.Maps
        (Key_Type => Graphs.Vertex, Element_Type => Color, others => <>);
   package With_Map is

      generic
         type Visitor (<>) is new Graphs.DFS_Visitor with private;
      procedure Search
        (G      : Graphs.Graph;
         Visit  : in out Visitor;
         Colors : out Color_Maps.Map;
         V      : Graphs.Vertex := Graphs.Null_Vertex);
      --  A depth first search is a traversal of the graph that always chooses
      --  to go deeper in the graph when possible, by looking at the next
      --  adjacent undiscovered vertex until reaching a vertex that has no
      --  undiscovered adjacent vertice. It then backtracks to the previous
      --  vertex.
      --
      --  All vertices and edges are visited exactly once.
      --
      --  Searching starts at V, if specified, but all vertices are eventually
      --  visited, unless Visit.Should_Stop returns True before then.
      --
      --  Depth First Search is a basic block for a lot of other algorithms.
      --  However, it is also useful on its own:
      --    * compute whether a vertex is reachable from another vertex
      --    * detect cycles in a graph
      --
      --  This algorithm needs to mark visited vertices with colors, using the
      --  provided map.
      --
      --  Complexity:  O( |edges| + |vertices| )
      --
      --  This algorithm returns nothing. All actions happen through a visitor.
      --  Calls to the visitor's operations are not dispatching, so that they
      --  can be inlined by the compiler and provide maximum performance.

      generic
         type Visitor (<>) is new Graphs.DFS_Visitor with private;
      procedure Search_Recursive
        (G      : Graphs.Graph;
         Visit  : in out Visitor;
         Colors : out Color_Maps.Map;
         V      : Graphs.Vertex := Graphs.Null_Vertex);
      --  A recursive version of the DFS algorithm.
      --  It is fractionally faster on small graph, and is not compatible with
      --  large graphs (since the depth of recursion with blow the stack).

      function Is_Acyclic
        (G      : Graphs.Graph;
         Colors : out Color_Maps.Map) return Boolean;
      --  Whether the graph has no cycles

      generic
         with procedure Callback (V : Graphs.Vertex);
      procedure Reverse_Topological_Sort
        (G     : Graphs.Graph;
         Colors : out Color_Maps.Map)
        with Pre => Is_Acyclic (G, Colors);
      --  Return the vertices in reverse topological order.
      --
      --  Topological order:
      --  If the graph contains an edge (u-->v), then v will always be finished
      --  first, i.e. the visitor's Finish_Vertex operation will be called on v
      --  before it is called on u.

   end With_Map;

   -------------------
   -- Exterior maps --
   -------------------
   --  Depth-First-Search with exterior color maps.
   --  The map is automatically created and cleared by the algorithm.

   generic
      with package Graphs is new Conts.Graphs.Traits (<>);
      with package Color_Maps is new Conts.Properties.Maps
        (Key_Type => Graphs.Vertex, Element_Type => Color, others => <>);
      with function Create_Map (G : Graphs.Graph) return Color_Maps.Map;
   package Exterior is

      generic
         type Visitor (<>) is new Graphs.DFS_Visitor with private;
      procedure Search
        (G     : Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Vertex := Graphs.Null_Vertex);

      function Is_Acyclic (G : Graphs.Graph) return Boolean;

      generic
         with procedure Callback (V : Graphs.Vertex);
      procedure Reverse_Topological_Sort (G : Graphs.Graph);

   end Exterior;

   -------------------
   -- Interior maps --
   -------------------
   --  Depth-First-Search
   --  This version stores the vertices colors in the graph itself.
   --  This version will not, in general, support simultaneous runs of the
   --  algorithms, since they would interfer in the color map.

   generic
      with package Graphs is new Conts.Graphs.Traits (<>);
      with package Color_Maps is new Conts.Properties.Maps
        (Map_Type => Graphs.Graph, Key_Type => Graphs.Vertex,
         Element_Type => Color, others => <>);
   package Interior is

      generic
         type Visitor (<>) is new Graphs.DFS_Visitor with private;
      procedure Search
        (G     : in out Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Vertex := Graphs.Null_Vertex);

      generic
         type Visitor (<>) is new Graphs.DFS_Visitor with private;
      procedure Search_Recursive
        (G     : in out Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Vertex := Graphs.Null_Vertex);

      function Is_Acyclic (G : in out Graphs.Graph) return Boolean;

      generic
         with procedure Callback (V : Graphs.Vertex);
      procedure Reverse_Topological_Sort (G : in out Graphs.Graph);

   end Interior;

end Conts.Graphs.DFS;
