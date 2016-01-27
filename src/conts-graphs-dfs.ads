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
--  Calls to the visitor's operations are not dispatching, so that they
--  can be inlined by the compiler and provide maximum performance.
--
--  When Terminator returns True, the algorithm stops executing.

pragma Ada_2012;

package Conts.Graphs.DFS is

   --  Depth-First-Search with exterior color maps

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Exterior (<>);
      with function Create_Map (G : Graphs.Graphs.Graph) return Maps.Map;
   package Exterior is

      generic
         type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      procedure Search
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);
      --  This version stores the vertices colors in an external map.
      --  It starts with vertex V (if specified), or with any vertex
      --  otherwise.
      --  Stops iterating when Visit.Should_Stop returns True

      function Is_Acyclic (G : Graphs.Graphs.Graph) return Boolean;
      --  Whether the graph has no cycles

   end Exterior;

   --  Depth-First-Search
   --  This version stores the vertices colors in the graph itself.
   --  This version will not, in general, permit simultaneous runs of the
   --  algorithms, since they would interfer in the color map.

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Interior (<>);
   package Interior is

      generic
         type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      procedure Search
        (G     : in out Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);

      generic
         type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      procedure Search_Recursive
        (G     : in out Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);
      --  A recursive version of the DFS algorithm.
      --  It is fractionally faster on small graph, and is not compatible with
      --  large graphs (since the depth of recursion with blow the stack).

      function Is_Acyclic (G : in out Graphs.Graphs.Graph) return Boolean;

   end Interior;

   --  Depth-First-Search
   --  The map is given explicitly as a parameter, so that we do not need the
   --  Create_Map function and can use this for both interior and exterior
   --  maps, or with types of maps that cannot easily be returned from a
   --  function.

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Exterior (<>);
   package With_Map is

      generic
         type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      procedure Search
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         Map   : in out Maps.Map;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);

      generic
         type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      procedure Search_Recursive
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         Map   : in out Maps.Map;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);

      procedure Is_Acyclic
        (G       : Graphs.Graphs.Graph;
         Map     : in out Maps.Map;
         Acyclic : out Boolean);
   end With_Map;

end Conts.Graphs.DFS;
