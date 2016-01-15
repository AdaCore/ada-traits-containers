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

      if V /= Graphs.Graphs.Null_Vertex then
         Visit.Start_Vertex (G, V);
         Impl (V);
      end if;

      --  Search from remaining unvisited vertices

      VC := Graphs.Vertices.First (G);
      while Graphs.Vertices.Has_Element (G, VC) loop
         if Maps.Get (Map, Graphs.Vertices.Element (G, VC)) = White then
            Impl (Graphs.Vertices.Element (G, VC));
         end if;

         VC := Graphs.Vertices.Next (G, VC);
      end loop;
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
         (Self : in out Visitor; G : Graph; E : Edge)
      is
         pragma Unreferenced (Self, G, E);
      begin
         Acyclic := False;
      end Back_Edge;

      procedure DFS is
         new Search_With_Map (Graphs, Visitor, Maps, Terminator);

      V : Visitor;
   begin
      Acyclic := True;
      DFS (G, V);
   end Internal_Is_Acyclic;

   -------------------------
   -- Is_Acyclic_With_Map --
   -------------------------

   procedure Is_Acyclic_With_Map
      (G       : in out Graphs.Graphs.Graph;
       Acyclic : out Boolean)
   is
      Map : Maps.Map := Maps.Get_Map (G);   --  uninitialized map
      procedure Int is new Internal_Is_Acyclic (Graphs, Maps);
   begin
      Int (G, Map, Acyclic);
   end Is_Acyclic_With_Map;

   ----------------
   -- Is_Acyclic --
   ----------------

   procedure Is_Acyclic
      (G       : in out Graphs.Graphs.Graph;
       Acyclic : out Boolean)
   is
      procedure Int is new Internal_Is_Acyclic (Graphs, Maps.As_Exterior);
   begin
      Int (G, G, Acyclic);
   end Is_Acyclic;

end Conts.Graphs.DFS;
