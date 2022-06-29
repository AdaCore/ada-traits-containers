--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Conts.Properties;

package Conts.Graphs.Components is

   generic
      with package Graphs is new Conts.Graphs.Traits (<>);
      with package Component_Maps is new Conts.Properties.Maps
        (Key_Type => Graphs.Vertex, Element_Type => Integer, others => <>);
   procedure Strongly_Connected_Components
     (G                : Graphs.Graph;
      Components       : out Component_Maps.Map;
      Components_Count : out Positive);
   --  Compute the strongly components of the graph:
   --  These are maximal sets of vertices such that for every pair of
   --  vertices u and v in the set, there exists a path from u to v and
   --  a path from v to u.
   --  Each vertex belongs to one, and only one, such component. This
   --  algorithm sets the index of that component in the Components map,
   --  and returns the number of components that were found. In the
   --  Components, the indexes are in the range 1 .. Components_Count.
   --
   --  Each vertex that is not part of a vertex forms its own component.
   --
   --  The implementation uses the Cheriyan-Mehlhorn-Gabow algorithm.
   --  Complexity is O( |edges| + |vertices| )

end Conts.Graphs.Components;
