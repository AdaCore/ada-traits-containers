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

with Conts.Vectors.Indefinite_Unbounded_Ref;
with Conts.Vectors.Definite_Unbounded;
with Conts.Graphs.DFS;

generic
   type Vertex_Type is (<>);
   --  The type used to represent vertices. These are indices into an internal
   --  vector of vertices, but could be any integer type, enumeration,...

   with package Vertex_Properties is new Conts.Elements.Traits (<>);
   with package Edge_Properties is new Conts.Elements.Traits (<>);
   --  The data associated with edges and properties

   type Base_Type is abstract tagged limited private;
   --  The base type for the graph.
   --  This is a way to make lists either controlled or limited.

package Conts.Graphs.Adjacency_List is

   subtype Vertex is Vertex_Type;
   --  Make this type visible to all packages using instances.

   package Impl is
      type Graph is new Base_Type with private;

      function Length (Self : Graph) return Count_Type;
      --  Return the number of elements in the graph

      Null_Vertex : constant Vertex;

      function Identity (V : Vertex) return Vertex is (V) with Inline;
      --  Implements the traits package later

      type Edge is private;

      function Get_Target
        (G : Graph; E : Edge) return Vertex with Inline;

      type Vertex_Cursor is private;
      function First (G : Graph) return Vertex_Cursor;
      function Element (G : Graph; C : Vertex_Cursor) return Vertex;
      function Has_Element (G : Graph; C : Vertex_Cursor) return Boolean;
      function Next (G : Graph; C : Vertex_Cursor) return Vertex_Cursor;

      type Edges_Cursor is private;
      function Out_Edges (G : Graph; V : Vertex) return Edges_Cursor;
      function Element (G : Graph; C : Edges_Cursor) return Edge;
      function Has_Element (G : Graph; C : Edges_Cursor) return Boolean;
      function Next (G : Graph; C : Edges_Cursor) return Edges_Cursor;

      procedure Add_Vertices
        (Self  : in out Graph;
         Props : Vertex_Properties.Element_Type;
         Count : Count_Type);
      --  Add Count vertices to the graph.
      --  Each node gets a copy of Props

      procedure Add_Edge
        (Self     : in out Graph;
         From, To : Vertex;
         Props    : Edge_Properties.Element_Type)
        with Pre => Vertex'Pos (From) <= Self.Length
                    and Vertex'Pos (To) <= Self.Length;
      --  Add a new edge between two vertices

      procedure Clear (Self : in out Graph);
      --  Remove all vertices and edges from the graph

   private
      Null_Vertex : constant Vertex := Vertex'Last;

      type Edge_Index is new Natural;

      type Edge is record
         Props : Edge_Properties.Stored_Type;
         From, To : Vertex;
      end record;
      procedure Release (E : in out Edge);

      type Dummy_Record is tagged null record;
      package Edge_Vectors is new Conts.Vectors.Definite_Unbounded
        (Index_Type   => Edge_Index,
         Element_Type => Edge,
         Base_Type    => Dummy_Record,
         Free         => Release);

      type Vertex_Record is record
         Props     : Vertex_Properties.Stored_Type;
         Out_Edges : Edge_Vectors.Vector;
      end record;
      procedure Release (V : in out Vertex_Record);

      package Vertex_Vectors is new Conts.Vectors.Indefinite_Unbounded_Ref
        (Index_Type   => Vertex,
         Element_Type => Vertex_Record,
         Base_Type    => Dummy_Record,
         Free         => Release);

      type Graph is new Base_Type with record
         Vertices : Vertex_Vectors.Vector;
      end record;

      type Vertex_Cursor is record
         Current : Vertex_Vectors.Cursor;
      end record;

      type Edges_Cursor is record
         From    : Vertex;
         Current : Edge_Vectors.Cursor;
      end record;
   end Impl;

   subtype Graph is Impl.Graph;
   subtype Edge is Impl.Edge;

   function Dummy_Copy (E : Vertex) return Vertex is (E) with Inline;

   package Vertices is new Conts.Elements.Traits
     (Element_Type => Vertex,
      Stored_Type  => Vertex,
      Return_Type  => Vertex,
      To_Stored    => Impl.Identity,
      To_Return    => Impl.Identity,
      To_Element   => Impl.Identity,
      Copy         => Dummy_Copy,   --  Unused since Copyable is True
      Copyable     => True,
      Movable      => True);

   package Traits is new Conts.Graphs.Traits
     (Graph       => Impl.Graph,
      Vertices    => Vertices,
      Edge        => Impl.Edge,
      Null_Vertex => Impl.Null_Vertex,
      Get_Target  => Impl.Get_Target);

   package Vertices_Cursors is new Traits.Vertex_Cursors
     (Cursor      => Impl.Vertex_Cursor,
      First       => Impl.First,
      Element     => Impl.Element,
      Has_Element => Impl.Has_Element,
      Next        => Impl.Next);

   package Out_Edges_Cursors is new Traits.Edge_Cursors
     (Cursor      => Impl.Edges_Cursor,
      First       => Impl.Out_Edges,
      Element     => Impl.Element,
      Has_Element => Impl.Has_Element,
      Next        => Impl.Next);

   package Incidence_Traits is new Conts.Graphs.Incidence_Graph_Traits
     (Graphs      => Traits,
      Vertices    => Vertices_Cursors,
      Out_Edges   => Out_Edges_Cursors);

   --  Color map is always limited, since this is mostly created automatically
   --  by algorithms. If you create a color map yourself, you need to clear
   --  it manually.
   package Impl_Color_Maps is
     new Traits.Color_Property_Maps.Property_Maps_From_Index
       (Vertex, Conts.Limited_Base, Impl.Identity, Impl.Length);
   package Color_Maps renames Impl_Color_Maps.As_Exterior;

   --  An integer map is mostly created by the application, since it holds the
   --  results of strongly connected components for instance. So we make it
   --  a controlled type (implicit Clear) if the graph itself is controlled,
   --  so that it is compatible with SPARK when people want to, but on the
   --  other hand it is in general cleared automatically when possible.
   package Integer_Maps is
     new Traits.Integer_Property_Maps.Property_Maps_From_Index
       (Vertex, Base_Type, Impl.Identity, Impl.Length);

   package DFS is new Conts.Graphs.DFS.Exterior
     (Incidence_Traits, Color_Maps,
      Create_Map => Impl_Color_Maps.Create_Map);
end Conts.Graphs.Adjacency_List;
