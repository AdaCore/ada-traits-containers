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

package body Conts.Graphs.DFS is

   ---------------------------------
   -- Search_From_Vertex_With_Map --
   ---------------------------------

   procedure Search_From_Vertex_With_Map
      (G : Graph; Visit : in out Visitor; Map : in out Maps.Map; V : Vertex)
   is
      procedure Impl (Current : Vertex) with Inline;
      procedure Init (V : Vertex) with Inline;
      procedure Find_Source (V : Vertex) with Inline;
      procedure On_Edge (E : Edge) with Inline;

      procedure Init (V : Vertex) is
      begin
         Maps.Set (Map, V, White);
         Visit.Initialize_Vertex (G, V);
      end Init;

      procedure On_Edge (E : Edge) is
         Target : constant Vertex := Get_Target (G, E);
      begin
         Visit.Examine_Edge (G, E);

         case Maps.Get (Map, Target) is
            when White =>
               Visit.Tree_Edge (G, E);
               Impl (Target);
            when Gray =>
               Visit.Back_Edge (G, E);
            when Black =>
               Visit.Forward_Or_Cross_Edge (G, E);
         end case;

         Visit.Finish_Edge (G, E);
      end On_Edge;

      procedure Impl (Current : Vertex) is
      begin
         Maps.Set (Map, Current, Gray);
         Visit.Discover_Vertex (G, Current);

         if not Terminator (G, Current) then
            For_Each_Out_Edge (G, Current, On_Edge'Access);
         end if;

         Maps.Set (Map, Current, Black);
         Visit.Finish_Vertex (G, Current);
      end Impl;

      procedure Find_Source (V : Vertex) is
      begin
         if Maps.Get (Map, V) = White then
            Impl (V);
         end if;
      end Find_Source;

   begin
      --  When For_Each_Vertex was declared as inline by the application,
      --  this generates no subprogram call to For_Each_Vertex but it
      --  does generate subprogram call to Init.
      For_Each_Vertex (G, Init'Access);

      Visit.Start_Vertex (G, V);
      Impl (V);

      For_Each_Vertex (G, Find_Source'Access);
   end Search_From_Vertex_With_Map;

   ---------------------
   -- Search_With_Map --
   ---------------------

   procedure Search_With_Map
      (G : Graphs.Graph; Visit : in out Visitor; Map : in out Maps.Map)
   is
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map, Default_Start_Vertex (G));
   end Search_With_Map;

   ------------
   -- Search --
   ------------

   procedure Search (G : in out Graphs.Graph; Visit : in out Visitor) is
      package Maps is new Graphs.Color_Property_Maps.Traits
         (Map => Graphs.Graph,
          Set => Set_Color,
          Get => Get_Color);
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map => G, V => Default_Start_Vertex (G));
   end Search;

   ------------------------
   -- Search_From_Vertex --
   ------------------------

   procedure Search_From_Vertex
      (G : in out Graph; Visit : in out Visitor; V : Vertex)
   is
      package Maps is new Graphs.Color_Property_Maps.Traits
         (Map => Graphs.Graph,
          Set => Set_Color,
          Get => Get_Color);
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map => G, V => V);
   end Search_From_Vertex;

end Conts.Graphs.DFS;
