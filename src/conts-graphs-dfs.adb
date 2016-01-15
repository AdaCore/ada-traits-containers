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
      procedure Impl (Current : Vertex);

      procedure Impl (Current : Vertex) is
         EC : Graphs.Out_Edges.Cursor;
      begin
         Maps.Set (Map, Current, Gray);
         Visit.Discover_Vertex (G, Current);

         if not Terminator (G, Current) then
            EC := Graphs.Out_Edges.First (G, Current);
            while Graphs.Out_Edges.Has_Element (G, EC) loop
               declare
                  E      : Edge renames Graphs.Out_Edges.Element (G, EC);
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
               end;

               EC := Graphs.Out_Edges.Next (G, EC);
            end loop;
         end if;

         Maps.Set (Map, Current, Black);
         Visit.Finish_Vertex (G, Current);
      end Impl;

      VC : Graphs.Vertices.Cursor;
   begin
      --  Initialize

      VC := Graphs.Vertices.First (G);
      while Graphs.Vertices.Has_Element (G, VC) loop
         Maps.Set (Map, Graphs.Vertices.Element (G, VC), White);
         Visit.Initialize_Vertex (G, Graphs.Vertices.Element (G, VC));
         VC := Graphs.Vertices.Next (G, VC);
      end loop;

      --  Search from the start vertex

      Visit.Start_Vertex (G, V);
      Impl (V);

      --  Search from remaining unvisited vertices

      VC := Graphs.Vertices.First (G);
      while Graphs.Vertices.Has_Element (G, VC) loop
         if Maps.Get (Map, Graphs.Vertices.Element (G, VC)) = White then
            Impl (Graphs.Vertices.Element (G, VC));
         end if;

         VC := Graphs.Vertices.Next (G, VC);
      end loop;
   end Search_From_Vertex_With_Map;

   ---------------------
   -- Search_With_Map --
   ---------------------

   procedure Search_With_Map
      (G : Graph; Visit : in out Visitor; Map : in out Maps.Map)
   is
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map, Graphs.Default_Start_Vertex (G));
   end Search_With_Map;

   ------------
   -- Search --
   ------------

   procedure Search (G : in out Graph; Visit : in out Visitor) is
      package Maps is new Graphs.Graphs.Color_Property_Maps.Traits
         (Map => Graph,
          Set => Set_Color,
          Get => Get_Color);
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map => G, V => Graphs.Default_Start_Vertex (G));
   end Search;

   ------------------------
   -- Search_From_Vertex --
   ------------------------

   procedure Search_From_Vertex
      (G : in out Graph; Visit : in out Visitor; V : Vertex)
   is
      package Maps is new Graphs.Graphs.Color_Property_Maps.Traits
         (Map => Graph,
          Set => Set_Color,
          Get => Get_Color);
      procedure Internal is new
         Search_From_Vertex_With_Map (Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map => G, V => V);
   end Search_From_Vertex;

end Conts.Graphs.DFS;
