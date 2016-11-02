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

--  A graph data structure implemented as an adjacency list.
--  It stores a sequence of out-edges for each vertex.
--  This package lets users decide whether to use lists, vectors, or other
--  types of sequences for vertices and their edges.

pragma Ada_2012;

package body Conts.Graphs.Adjacency_List is

   ----------
   -- Impl --
   ----------

   package body Impl is

      -----------
      -- Clear --
      -----------

      procedure Clear (Self : in out Graph) is
      begin
         Self.Vertices.Clear;
      end Clear;

      ------------------
      -- Add_Vertices --
      ------------------

      procedure Add_Vertices
        (Self  : in out Graph;
         Props : Vertex_Properties.Element_Type;
         Count : Count_Type := 1) is
      begin
         Self.Vertices.Append
           (Element =>
              (Props     => Vertex_Properties.To_Stored (Props),
               Out_Edges => <>),
            Count   => Count);
      end Add_Vertices;

      --------------
      -- Add_Edge --
      --------------

      procedure Add_Edge
        (Self     : in out Graph;
         From, To : Vertex;
         Props    : Edge_Properties.Element_Type) is
      begin
         Self.Vertices.Reference (From).Out_Edges.Append
           (Edge'
              (From  => From,
               To    => To,
               Props => Edge_Properties.To_Stored (Props)));
      end Add_Edge;

      ------------
      -- Length --
      ------------

      function Length (Self : Graph) return Count_Type is
      begin
         return Self.Vertices.Length;
      end Length;

      ----------------
      -- Get_Target --
      ----------------

      function Get_Target (G : Graph; E : Edge) return Vertex is
         pragma Unreferenced (G);
      begin
         return E.To;
      end Get_Target;

      -----------
      -- First --
      -----------

      function First (G : Graph) return Vertex_Cursor is
      begin
         return (Current => G.Vertices.First);
      end First;

      -------------
      -- Element --
      -------------

      function Element (G : Graph; C : Vertex_Cursor) return Vertex is
         pragma Unreferenced (G);
      begin
         return C.Current;
      end Element;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (G : Graph; C : Vertex_Cursor) return Boolean is
      begin
         return G.Vertices.Has_Element (C.Current);
      end Has_Element;

      ----------
      -- Next --
      ----------

      function Next (G : Graph; C : Vertex_Cursor) return Vertex_Cursor is
      begin
         return (Current => G.Vertices.Next (C.Current));
      end Next;

      ---------------
      -- Out_Edges --
      ---------------

      function Out_Edges (G : Graph; V : Vertex) return Edges_Cursor is
      begin
         return (Current => G.Vertices.Element (V).Out_Edges.First,
                 From    => V);
      end Out_Edges;

      -------------
      -- Element --
      -------------

      function Element (G : Graph; C : Edges_Cursor) return Edge is
      begin
         return G.Vertices.Element (C.From).Out_Edges.Element (C.Current);
      end Element;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (G : Graph; C : Edges_Cursor) return Boolean is
      begin
         return G.Vertices.Element (C.From).Out_Edges.Has_Element (C.Current);
      end Has_Element;

      ----------
      -- Next --
      ----------

      function Next (G : Graph; C : Edges_Cursor) return Edges_Cursor is
      begin
         return (Current =>
                   G.Vertices.Element (C.From).Out_Edges.Next (C.Current),
                 From    => C.From);
      end Next;

      -------------
      -- Release --
      -------------

      procedure Release (E : in out Edge) is
      begin
         Edge_Properties.Release (E.Props);
      end Release;

      -------------
      -- Release --
      -------------

      procedure Release (V : in out Vertex_Record) is
      begin
         Vertex_Properties.Release (V.Props);
         V.Out_Edges.Clear;
      end Release;

   end Impl;

end Conts.Graphs.Adjacency_List;
