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

with Conts.Graphs.DFS;

package Conts.Graphs.Connected_Components is

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
   procedure Connected_Components (G : Graphs.Graphs.Graph);
   --  ??? Unimplemented yet.
   --  This algorithm uses no Pre or Post conditions (see below)

   -------------------------------
   -- Algorithm with assertions --
   -------------------------------

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Exterior (<>);
      with function Create_Map (G : Graphs.Graphs.Graph) return Maps.Map;
      with package DFS is
         new Conts.Graphs.DFS.Exterior (Graphs, Maps, Create_Map);
   procedure Connected_Components_With_Pre
      (G     : Graphs.Graphs.Graph)
       with Pre => (DFS.Is_Acyclic (G));
   --  Same as above, but with preconditions.
   --  To provide these preconditions, we need to be able to execute a generic
   --  algorith. Since we cannot do the instantiation in the precondition
   --  itself, we pass is explicitly as a formal parameter (along with any
   --  other parameter necessary for its instantiation).

end Conts.Graphs.Connected_Components;
