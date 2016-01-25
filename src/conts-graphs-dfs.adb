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

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      type Visitor (<>) is new Graphs.Graphs.DFS_Visitor with private;
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Exterior (<>);
      with function Terminator
         (G : Graphs.Graphs.Graph; V : Graphs.Graphs.Vertex) return Boolean
         is Graphs.Graphs.Never_Stop;
   procedure Internal_DFS
      (G     : Graphs.Graphs.Graph;
       Visit : in out Visitor;
       Map   : in out Maps.Map;
       V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex);
   --  Internal implementation

   generic
      with package Graphs is new Conts.Graphs.Incidence_Graph_Traits (<>);
      with package Maps is new Graphs.Graphs.Color_Property_Maps.Exterior (<>);
   procedure Internal_Is_Acyclic
      (G       : Graphs.Graphs.Graph;
       Map     : in out Maps.Map;
       Acyclic : out Boolean);
   --  Internal implementation

   ------------------
   -- Internal_DFS --
   ------------------

   procedure Internal_DFS
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

      Stack : Vertex_Vectors.Vector;
      Terminated : Boolean := False;

      procedure Impl (Start : Vertex);

      procedure Impl (Start : Vertex) is
         EC   : Graphs.Out_Edges.Cursor;
         Info : Vertex_Info;
      begin
         Maps.Set (Map, Start, Gray);
         Visit.Discover_Vertex (G, Start);
         if Terminator (G, Start) then
            Terminated := True;
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
                  V : constant Vertex :=
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
                  E      : constant Edge := Graphs.Out_Edges.Element (G, EC);
                  Target : constant Vertex := Get_Target (G, E);
               begin
                  Visit.Examine_Edge (G, E);
                  case Maps.Get (Map, Target) is
                     when White =>
                        Visit.Tree_Edge (G, E);
                        Maps.Set (Map, Target, Gray);
                        Visit.Discover_Vertex (G, Target);
                        if Terminator (G, Target) then
                           Terminated := True;
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

      VC : Graphs.Vertices.Cursor;
   begin
      --  Preallocate some space, to improve performance
      Stack.Reserve_Capacity (200);

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

      Stack.Clear;
   end Internal_DFS;

   ---------------------
   -- Search_With_Map --
   ---------------------

   procedure Search_With_Map
      (G     : Graphs.Graphs.Graph;
       Visit : in out Visitor;
       V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
   is
      Map : Maps.Map := Maps.Get_Map (G);   --  uninitialized map
      procedure Internal is new
         Internal_DFS (Graphs, Visitor, Maps, Terminator);
   begin
      Internal (G, Visit, Map, V);
   end Search_With_Map;

   ------------
   -- Search --
   ------------

   procedure Search
      (G     : in out Graphs.Graphs.Graph;
       Visit : in out Visitor;
       V     : Graphs.Graphs.Vertex := Graphs.Graphs.Null_Vertex)
   is
      procedure Internal is new
         Internal_DFS (Graphs, Visitor, Maps.As_Exterior, Terminator);
   begin
      Internal (G, Visit, Map => G, V => V);
   end Search;

   -------------------------
   -- Internal_Is_Acyclic --
   -------------------------

   procedure Internal_Is_Acyclic
      (G       : Graphs.Graphs.Graph;
       Map     : in out Maps.Map;
       Acyclic : out Boolean)
   is
      use Graphs.Graphs;

      function Terminator (G : Graph; V : Vertex) return Boolean
         is (not Acyclic) with Inline;
      --  Stop searching as soon as we have found a cycle

      type Visitor is new DFS_Visitor with null record;
      overriding procedure Back_Edge
         (Self : in out Visitor; G : Graph; E : Edge);

      overriding procedure Back_Edge
         (Self : in out Visitor; G : Graph; E : Edge)
      is
         pragma Unreferenced (Self, G, E);
      begin
         Acyclic := False;
      end Back_Edge;

      procedure DFS is new Internal_DFS (Graphs, Visitor, Maps, Terminator);

      V : Visitor;
   begin
      Acyclic := True;
      DFS (G, V, Map);
   end Internal_Is_Acyclic;

   -------------------------
   -- Is_Acyclic_With_Map --
   -------------------------

   package body Is_Acyclic_With_Map is
      procedure Int is new Internal_Is_Acyclic (Graphs, Maps);

      function Is_Acyclic (G : Graphs.Graphs.Graph) return Boolean is
         Map : Maps.Map := Maps.Get_Map (G);   --  uninitialized map
         Acyclic : Boolean;
      begin
         Int (G, Map, Acyclic);
         return Acyclic;
      end Is_Acyclic;
   end Is_Acyclic_With_Map;

   ----------------
   -- Is_Acyclic --
   ----------------

   function Is_Acyclic (G : in out Graphs.Graphs.Graph) return Boolean is
      procedure Int is new Internal_Is_Acyclic (Graphs, Maps.As_Exterior);
      Acyclic : Boolean;
   begin
      Int (G, G, Acyclic);
      return Acyclic;
   end Is_Acyclic;

end Conts.Graphs.DFS;
