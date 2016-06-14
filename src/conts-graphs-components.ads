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
