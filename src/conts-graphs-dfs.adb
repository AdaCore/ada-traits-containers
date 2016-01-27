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
with Conts.Elements.Definite;
with Conts.Vectors.Nodes.Unbounded;
with Conts.Vectors.Generics;

package body Conts.Graphs.DFS is

   --------------
   -- With_Map --
   --------------

   package body With_Map is
      use Graphs.Graphs;

      type Acyclic_Visitor is new Graphs.Graphs.DFS_Visitor with record
         Has_Cycle : Boolean := False;
      end record;
      overriding procedure Back_Edge
        (Self : in out Acyclic_Visitor; G : Graph; E : Edge) with Inline;
      overriding procedure Should_Stop
        (Self : Acyclic_Visitor; G : Graph; V : Vertex;
         Stop : in out Boolean) with Inline;

      -----------------
      -- Should_Stop --
      -----------------

      overriding procedure Should_Stop
        (Self : Acyclic_Visitor; G : Graph; V : Vertex;
         Stop : in out Boolean)
      is
         pragma Unreferenced (G, V);
      begin
         Stop := Self.Has_Cycle;
      end Should_Stop;

      ---------------
      -- Back_Edge --
      ---------------

      overriding procedure Back_Edge
        (Self : in out Acyclic_Visitor; G : Graph; E : Edge)
      is
         pragma Unreferenced (G, E);
      begin
         Self.Has_Cycle := True;
      end Back_Edge;

      ------------
      -- Search --
      ------------

      procedure Search
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         Map   : in out Maps.Map;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
      is
         use Graphs.Graphs;

         type Vertex_Info is record
            VC : Graphs.Graphs.Vertices.Stored_Type;
            EC : Graphs.Out_Edges.Cursor;  --  next edge to examine
         end record;
         procedure Free (Self : in out Vertex_Info) with Inline;

         procedure Free (Self : in out Vertex_Info) is
         begin
            Graphs.Graphs.Vertices.Release (Self.VC);
         end Free;

         package Vertex_Info_Elements is new Conts.Elements.Definite
           (Vertex_Info, Free => Free);
         package Vertex_Nodes is new Conts.Vectors.Nodes.Unbounded
           (Elements      => Vertex_Info_Elements.Traits,
            Base_Type     => Conts.Limited_Base,
            Resize_Policy => Conts.Vectors.Resize_1_5);
         package Vertex_Vectors is new Conts.Vectors.Generics
           (Natural, Vertex_Nodes.Traits);

         Stack      : Vertex_Vectors.Vector;
         Terminated : Boolean := False;

         procedure Impl (Start : Vertex);

         procedure Impl (Start : Vertex) is
            EC   : Graphs.Out_Edges.Cursor;
            Info : Vertex_Info;
         begin
            Maps.Set (Map, Start, Gray);
            Visit.Discover_Vertex (G, Start);

            Visit.Should_Stop (G, Start, Stop => Terminated);
            if Terminated then
               return;
            end if;

            Stack.Append
              ((VC => Graphs.Graphs.Vertices.To_Stored (Start),
                EC => Graphs.Out_Edges.First (G, Start)));

            while not Stack.Is_Empty loop
               Info := Stack.Last_Element;
               Stack.Delete_Last;

               if not Graphs.Out_Edges.Has_Element (G, Info.EC) then
                  --  No more out edges
                  declare
                     --   ??? Inefficient, we might be using the secondary
                     --   stack here if Vertex is unconstrained
                     V : Vertex renames
                       Graphs.Graphs.Vertices.To_Element
                         (Graphs.Graphs.Vertices.To_Return (Info.VC));
                  begin
                     Maps.Set (Map, V, Black);
                     Visit.Finish_Vertex (G, V);
                  end;

               else
                  EC := Info.EC;

                  --  Next time we look at the same vertex, we'll look at
                  --  the next out edge
                  Info.EC := Graphs.Out_Edges.Next (G, Info.EC);
                  Stack.Append (Info);

                  --  Append the next vertex to examine (and we need to
                  --  examine it first)

                  declare
                     E  : constant Edge := Graphs.Out_Edges.Element (G, EC);

                     --  ??? Might be using secondary stack
                     Target : constant Vertex := Get_Target (G, E);
                  begin
                     Visit.Examine_Edge (G, E);
                     case Maps.Get (Map, Target) is
                     when White =>
                        Visit.Tree_Edge (G, E);
                        Maps.Set (Map, Target, Gray);
                        Visit.Discover_Vertex (G, Target);
                        Visit.Should_Stop (G, Target, Stop => Terminated);
                        if Terminated then
                           return;
                        end if;
                        Stack.Append
                          ((VC => Graphs.Graphs.Vertices.To_Stored (Target),
                            EC => Graphs.Out_Edges.First (G, Target)));

                     when Gray =>
                        Visit.Back_Edge (G, E);

                     when Black =>
                        Visit.Forward_Or_Cross_Edge (G, E);
                     end case;
                  end;
               end if;
            end loop;
         end Impl;

         use type Vertex;

         VC    : Graphs.Vertices.Cursor;
         Count : Natural := 0;
      begin
         --  Initialize

         VC := Graphs.Vertices.First (G);
         while Graphs.Vertices.Has_Element (G, VC) loop
            Maps.Set (Map, Graphs.Vertices.Element (G, VC), White);
            Visit.Initialize_Vertex (G, Graphs.Vertices.Element (G, VC));
            Count := Count + 1;
            VC := Graphs.Vertices.Next (G, VC);
         end loop;

         --  Preallocate some space, to improve performance
         --  Unless the graph is a tree with depth n, we do not need as many
         --  nodes in the stack as they are elements in the graph. So we use
         --  a number somewhere in between, as an attempt to limit the number
         --  of allocations, and yet not allocating too much memory.
         Stack.Reserve_Capacity (Count_Type'Min (300_000, Count));

         --  Search from the start vertex

         if V /= Graphs.Graphs.Null_Vertex then
            Visit.Start_Vertex (G, V);
            Impl (V);
         end if;

         --  Search for remaining unvisited vertices

         VC := Graphs.Vertices.First (G);
         while not Terminated and then Graphs.Vertices.Has_Element (G, VC) loop
            if Maps.Get (Map, Graphs.Vertices.Element (G, VC)) = White then
               Impl (Graphs.Vertices.Element (G, VC));
            end if;

            VC := Graphs.Vertices.Next (G, VC);
         end loop;

         Stack.Clear;
      end Search;

      ----------------------
      -- Search_Recursive --
      ----------------------

      procedure Search_Recursive
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         Map   : in out Maps.Map;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
      is
         use Graphs.Graphs;

         Terminated : Boolean := False;

         procedure Impl (Current : Vertex);

         procedure Impl (Current : Vertex) is
            EC   : Graphs.Out_Edges.Cursor;
         begin
            Maps.Set (Map, Current, Gray);
            Visit.Discover_Vertex (G, Current);
            Visit.Should_Stop (G, Current, Terminated);
            if not Terminated then
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
                  end;
                  EC := Graphs.Out_Edges.Next (G, EC);
               end loop;
            end if;

            Maps.Set (Map, Current, Black);
            Visit.Finish_Vertex (G, Current);
         end Impl;

         use type Vertex;

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

         if V /= Graphs.Graphs.Null_Vertex then
            Visit.Start_Vertex (G, V);
            Impl (V);
         end if;

         --  Search from remaining unvisited vertices

         VC := Graphs.Vertices.First (G);
         while not Terminated and then Graphs.Vertices.Has_Element (G, VC) loop
            if Maps.Get (Map, Graphs.Vertices.Element (G, VC)) = White then
               Impl (Graphs.Vertices.Element (G, VC));
            end if;

            VC := Graphs.Vertices.Next (G, VC);
         end loop;
      end Search_Recursive;

      ----------------
      -- Is_Acyclic --
      ----------------

      procedure Is_Acyclic
        (G       : Graphs.Graphs.Graph;
         Map     : in out Maps.Map;
         Acyclic : out Boolean)
      is
         use Graphs.Graphs;
         procedure DFS is new Search (Acyclic_Visitor);
         V   : Acyclic_Visitor;
      begin
         DFS (G, V, Map);
         Acyclic := not V.Has_Cycle;
      end Is_Acyclic;

   end With_Map;

   --------------
   -- Exterior --
   --------------

   package body Exterior is
      package Internal is new With_Map (Graphs, Maps);

      ----------------
      -- Is_Acyclic --
      ----------------

      function Is_Acyclic (G : Graphs.Graphs.Graph) return Boolean is
         Acyclic : Boolean;
         Map     : Maps.Map := Create_Map (G);   --  uninitialized map
      begin
         Internal.Is_Acyclic (G, Map, Acyclic);
         return Acyclic;
      end Is_Acyclic;

      ------------
      -- Search --
      ------------

      procedure Search
        (G     : Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
      is
         procedure Internal_Search is new Internal.Search (Visitor);
         Map : Maps.Map := Create_Map (G);   --  uninitialized map
      begin
         Internal_Search (G, Visit, Map, V);
      end Search;

   end Exterior;

   --------------
   -- Interior --
   --------------

   package body Interior is

      package Internal is new With_Map (Graphs, Maps.As_Exterior);

      ----------------
      -- Is_Acyclic --
      ----------------

      function Is_Acyclic (G : in out Graphs.Graphs.Graph) return Boolean is
         Acyclic : Boolean;
      begin
         Internal.Is_Acyclic (G, G, Acyclic);
         return Acyclic;
      end Is_Acyclic;

      ------------
      -- Search --
      ------------

      procedure Search
        (G     : in out Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
      is
         procedure Internal_Search is new Internal.Search (Visitor);
      begin
         Internal_Search (G, Visit, G, V);
      end Search;

      ----------------------
      -- Search_Recursive --
      ----------------------

      procedure Search_Recursive
        (G     : in out Graphs.Graphs.Graph;
         Visit : in out Visitor;
         V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
      is
         procedure Internal_Search is new Internal.Search_Recursive (Visitor);
      begin
         Internal_Search (G, Visit, G, V);
      end Search_Recursive;

   end Interior;

end Conts.Graphs.DFS;
