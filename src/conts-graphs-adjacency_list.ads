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

pragma Ada_2012;

with Conts.Cursors;
with Conts.Properties.Indexed;
with Conts.Elements.Indefinite;
with Conts.Vectors.Generics;
with Conts.Vectors.Storage.Unbounded;
with Conts.Vectors.Definite_Unbounded;
with Conts.Graphs.DFS;

generic
   type Vertex_Type is (<>);
   --  The type used to represent vertices. These are indices into an internal
   --  vector of vertices, but could be any integer type, ...
   --  Just like for vectors's index_type, this package expects that
   --    (Vertex_Type'First - 1) is valid in Vertex_Type'Base.
   --  This prevents Integer or enumeration types from being used.

   with package Vertex_Properties is new Conts.Elements.Traits (<>);
   with package Edge_Properties is new Conts.Elements.Traits (<>);
   --  The data associated with edges and properties

   type Container_Base_Type is abstract tagged limited private;
   --  The base type for the graph.
   --  This is a way to make lists either controlled or limited.

package Conts.Graphs.Adjacency_List is

   subtype Extended_Vertex is Vertex_Type'Base range
     Vertex_Type'Pred (Vertex_Type'First) .. Vertex_Type'Last;
   --  Index_Type with one more element to the left, used to represent
   --  invalid indexes

   subtype Vertex is Vertex_Type;
   --  Make this type visible to all packages using instances.

   package Impl is
      type Graph is new Container_Base_Type with private;

      function Length (Self : Graph) return Count_Type;
      --  Return the number of elements in the graph

      Null_Vertex : constant Extended_Vertex;

      type Edge is private;

      function Get_Target
        (G : Graph; E : Edge) return Vertex with Inline;

      type Vertex_Cursor is private;
      No_Vertex : constant Vertex_Cursor;
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
         Props : Vertex_Properties.Element;
         Count : Count_Type := 1);
      --  Add Count vertices to the graph.
      --  Each node gets a copy of Props

      procedure Add_Edge
        (Self     : in out Graph;
         From, To : Vertex;
         Props    : Edge_Properties.Element)
        with Pre => Vertex'Pos (From) <= Self.Length
                    and Vertex'Pos (To) <= Self.Length;
      --  Add a new edge between two vertices

      procedure Clear (Self : in out Graph);
      --  Remove all vertices and edges from the graph

   private
      Null_Vertex : constant Extended_Vertex := Extended_Vertex'Last;

      type Edge_Index is new Natural;

      type Edge is record
         Props : Edge_Properties.Stored;
         From, To : Vertex;
      end record;
      procedure Release (E : in out Edge);

      type Dummy_Record is tagged null record;
      package Edge_Vectors is new Conts.Vectors.Definite_Unbounded
        (Index_Type          => Edge_Index,
         Element_Type        => Edge,
         Container_Base_Type => Dummy_Record,
         Free                => Release);

      type Vertex_Record is record
         Props     : Vertex_Properties.Stored;
         Out_Edges : Edge_Vectors.Vector;
      end record;
      procedure Release (V : in out Vertex_Record);

      --  Indefinite so that we can edit in place
      package Vertex_Elements is new Conts.Elements.Indefinite
        (Vertex_Record, Free => Release, Pool => Conts.Global_Pool);
      package Vertex_Storage is new Conts.Vectors.Storage.Unbounded
        (Vertex_Elements.Traits,
         Container_Base_Type => Dummy_Record,
         Resize_Policy       => Conts.Vectors.Resize_1_5);
      package Vertex_Vectors is new Conts.Vectors.Generics
        (Index_Type => Vertex, Storage => Vertex_Storage.Traits);

      type Graph is new Container_Base_Type with record
         Vertices : Vertex_Vectors.Vector;
      end record;

      type Vertex_Cursor is record
         Current : Vertex_Vectors.Cursor;
      end record;
      No_Vertex : constant Vertex_Cursor :=
         (Current => Vertex_Vectors.No_Element);

      type Edges_Cursor is record
         From    : Vertex;
         Current : Edge_Vectors.Cursor;
      end record;
   end Impl;
   use all type Impl.Vertex_Cursor;

   subtype Graph is Impl.Graph;
   subtype Edge is Impl.Edge;

   function Identity (V : Vertex) return Vertex is (V) with Inline;

   package Vertices is new Conts.Elements.Traits
     (Element_Type           => Vertex,
      Stored_Type            => Vertex,
      Returned_Type          => Vertex,
      Constant_Returned_Type => Vertex,
      To_Stored              => Identity,
      To_Returned            => Identity,
      To_Constant_Returned   => Identity,
      To_Element             => Identity,
      Copy                   => Identity,   --  Unused since Copyable is True
      Copyable               => True,
      Movable                => True);

   package Vertices_Cursors is new Conts.Cursors.Forward_Cursors
     (Container_Type => Graph,
      Cursor_Type    => Impl.Vertex_Cursor,
      No_Element     => Impl.No_Vertex,
      First          => Impl.First,
      Has_Element    => Impl.Has_Element,
      Next           => Impl.Next);

   package Vertices_Maps is new Conts.Properties.Read_Only_Maps
     (Map_Type       => Graph,
      Key_Type       => Impl.Vertex_Cursor,
      Element_Type   => Vertex,
      Get            => Impl.Element);

   package Out_Edges_Cursors is new Conts.Graphs.Edge_Cursors
     (Container_Type => Graph,
      Vertices       => Vertices,
      Edge_Type      => Edge,
      Cursor_Type    => Impl.Edges_Cursor,
      First          => Impl.Out_Edges,
      Element        => Impl.Element,
      Has_Element    => Impl.Has_Element,
      Next           => Impl.Next);

   package Traits is new Conts.Graphs.Traits
     (Graph_Type        => Impl.Graph,
      Vertices          => Vertices,
      Edge_Type         => Impl.Edge,
      Null_Vertex       => Impl.Null_Vertex,
      Get_Target        => Impl.Get_Target,
      Vertex_Cursors    => Vertices_Cursors,
      Vertex_Maps       => Vertices_Maps,
      Out_Edges_Cursors => Out_Edges_Cursors);

   --  Color map is always limited, since this is mostly created automatically
   --  by algorithms. If you create a color map yourself, you need to clear
   --  it manually.
   package Color_Maps is new Conts.Properties.Indexed
     (Container_Type      => Graph,
      Key_Type            => Vertex,
      Element_Type        => Color,
      Default_Value       => White,
      Index_Type          => Vertex,
      Container_Base_Type => Conts.Limited_Base,
      Get_Index           => Identity,
      Length              => Impl.Length);

   --  An integer map is mostly created by the application, since it holds the
   --  results of strongly connected components for instance. So we make it
   --  a controlled type (implicit Clear) if the graph itself is controlled,
   --  so that it is compatible with SPARK when people want to, but on the
   --  other hand it is in general cleared automatically when possible.
   package Integer_Maps is new Conts.Properties.Indexed
     (Container_Type      => Graph,
      Key_Type            => Vertex,
      Element_Type          => Integer,
      Default_Value       => -1,
      Index_Type          => Vertex,
      Container_Base_Type => Container_Base_Type,
      Get_Index           => Identity,
      Length              => Impl.Length);

   package DFS is new Conts.Graphs.DFS.Exterior
     (Traits, Color_Maps.As_Map, Create_Map => Color_Maps.Create_Map);
end Conts.Graphs.Adjacency_List;
