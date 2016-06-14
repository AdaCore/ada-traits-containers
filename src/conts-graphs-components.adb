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
with Conts.Vectors.Storage.Unbounded;
with Conts.Vectors.Generics;
with Conts.Vectors.Definite_Unbounded;

package body Conts.Graphs.Components is

   package Roots_Vectors is new Conts.Vectors.Definite_Unbounded
     (Index_Type          => Natural,
      Element_Type        => Integer,
      Container_Base_Type => Conts.Limited_Base);
   --  A stack of integers.
   --  For the strongly-connected-components, these are the DFS numbers for
   --  the roots of each SCC.

   -----------------------------------
   -- Strongly_Connected_Components --
   -----------------------------------

   procedure Strongly_Connected_Components
     (G                : Graphs.Graph;
      Components       : out Component_Maps.Map;
      Components_Count : out Positive)
   is
      use Graphs;

      --  This algorithm needs multiple pieces of information for each
      --  vertex:
      --     * Whether the node has been visited or not.
      --        unvisited = white color,  visited = gray or black
      --     * whether a node is closed
      --        when it is given a component number, it is closed
      --     * a DFS index (time at which the vertex was discovered)
      --
      --  We combine all of these into a single piece of information, to
      --  make better use of memory cache.
      --  The information is stored in the Components map, with the
      --  following definitions:
      --     * n = 0
      --       The node has not been visited yet.
      --       DFS index unknown, as well as lowlink
      --       Component id unset
      --     * n < 0
      --       Node has been visited. The algorithm does not
      --          need to distinguish gray and black (but then Back_Edge is
      --          the same as Forward_Or_Cross_Edge).
      --       Component id unset
      --       n is the negated DFS index.
      --     * n > 0
      --       Node has been closed
      --       n is the component id
      --
      --  See:
      --    https://people.mpi-inf.mpg.de/~mehlhorn/ftp/EngineeringDFS.pdf

      package Vertex_Storage is new Conts.Vectors.Storage.Unbounded
        (Elements            => Graphs.Vertices,
         Container_Base_Type => Conts.Limited_Base,
         Resize_Policy       => Conts.Vectors.Resize_1_5);
      package Vertex_Vectors is new Conts.Vectors.Generics
        (Natural, Vertex_Storage.Traits);
      --  A stack of vertices.

      Roots : Roots_Vectors.Vector;
      Roots_Top : Integer;
      --  The roots of the SCC components (their DFS number is enough)

      Open : Vertex_Vectors.Vector;

      Comp  : Positive := 1;   --  current component
      DFS_Index : Positive := 1;         --  current DFS index

      --  A custom color map which stores integers instead
      procedure Set
        (M : in out Component_Maps.Map; V : Vertex; C : Color);
      function Get (M : Component_Maps.Map; V : Vertex) return Color;

      procedure Set
        (M : in out Component_Maps.Map; V : Vertex; C : Color) is
      begin
         case C is
            when White =>
               Component_Maps.Set (M, V, 0);  --  unvisited

            when Gray =>   --  Vertex is discovered
               Roots_Top := -DFS_Index;
               Component_Maps.Set (M, V, Roots_Top);  --  visited
               Roots.Append (Roots_Top);
               Open.Append (V);
               DFS_Index := DFS_Index + 1;

            when Black =>   --  Vertex is finished
               declare
                  V_DFS_Index : constant Integer :=
                    Component_Maps.Get (Components, V);
               begin
                  if V_DFS_Index = Roots_Top then
                     Roots.Delete_Last;
                     if not Roots.Is_Empty then
                        Roots_Top := Roots.Last_Element;
                     end if;

                     loop
                        declare
                           U : constant Vertex :=
                             Graphs.Vertices.To_Elem (Open.Last_Element);
                           U_Index  : constant Integer :=
                             Component_Maps.Get (Components, U);
                        begin
                           Open.Delete_Last;
                           Component_Maps.Set (Components, U, Comp);
                           exit when U_Index = V_DFS_Index;
                        end;
                     end loop;

                     Comp := Comp + 1;
                  end if;
               end;
         end case;
      end Set;

      function Get (M : Component_Maps.Map; V : Vertex) return Color is
      begin
         if Component_Maps.Get (M, V) = 0 then
            return White;
         else
            return Gray;
         end if;
      end Get;

      package Color_Maps is new Conts.Properties.Maps
        (Key_Type     => Vertex,
         Element_Type => Color,
         Map_Type     => Component_Maps.Map,
         Set          => Set,
         Get          => Get);
      package Local_DFS is new Conts.Graphs.DFS.With_Map (Graphs, Color_Maps);

      type SCC_Visitor is new DFS_Visitor with null record;
      overriding procedure Vertices_Initialized
        (Self : in out SCC_Visitor;
         G    : Graphs.Graph; Count : Count_Type);
      overriding procedure Back_Edge
        (Self : in out SCC_Visitor; G : Graphs.Graph; E : Graphs.Edge);
      --  Some of the operations (discover and finish) are handled in the
      --  color map.

      overriding procedure Vertices_Initialized
        (Self : in out SCC_Visitor; G : Graphs.Graph; Count : Count_Type)
      is
         pragma Unreferenced (Self, G);
      begin
         Roots.Reserve_Capacity (Count_Type'Min (300_000, Count));
         Open.Reserve_Capacity (Count_Type'Min (300_000, Count));
      end Vertices_Initialized;

      overriding procedure Back_Edge
        (Self : in out SCC_Visitor; G : Graphs.Graph; E : Graphs.Edge)
      is
         pragma Unreferenced (Self);
         V           : constant Vertex := Graphs.Get_Edge_Target (G, E);
         V_DFS_Index : constant Integer := Component_Maps.Get (Components, V);
      begin
         --  If V is open
         if V_DFS_Index < 0 then
            while Roots_Top < V_DFS_Index loop
               Roots.Delete_Last;
               Roots_Top := Roots.Last_Element;
            end loop;
         end if;
      end Back_Edge;

      procedure DFS is new Local_DFS.Search (SCC_Visitor);

      V : SCC_Visitor;
   begin
      DFS (G, V, Components);
      Components_Count := Comp - 1;
      Roots.Clear;
      Open.Clear;
   end Strongly_Connected_Components;

end Conts.Graphs.Components;
