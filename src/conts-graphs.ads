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

      with procedure For_Each_Vertex
         (G : Graph;
          Callback : not null access procedure (V : Vertex)) is <>;
      --  Calls Callback for each vertex in G.
      --  As much as possible, the actual parameter should be declared as
      --  inline. We chose to use a procedure rather than an explicit
      --  iterator, since in some cases the implementation might be recursive
      --  and this is not convenient to implement with iterators.

      with procedure For_Each_Out_Edge
         (G : Graph;
          V : Vertex;
          Callback : not null access procedure (E : Edge)) is <>;
      --  Calls Callback for each edge out of V.

      with function Get_Target (G : Graph; E : Edge) return Vertex is <>;
      --  Return the target of the edge.

      with function Default_Start_Vertex (G : Graph) return Vertex is <>;
      --  ??? Should be computed from For_Each_Vertex, but we can't stop the
      --  iteration there (perhaps we should use iterators).

   package Traits is
      package Color_Property_Maps is new Property_Maps (Vertex, Color);
   end Traits;

   ----------------------------
   -- Incidence_Graph_Traits --
   ----------------------------
   --  A graph that provides two iterators: one to find all vertices in the
   --  graph, and another one to find all out edges from these vertices.

end Conts.Graphs;
