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
with Ada.Text_IO; use Ada.Text_IO;

package body Graph1_Support is

   procedure Set_Color (G : in out Graph; V : Vertex; C : Color) is
   begin
      G.Colors (V) := C;
   end Set_Color;

   function Get_Color (G : Graph; V : Vertex) return Color is
   begin
      return G.Colors (V);
   end Get_Color;

   overriding procedure Initialize_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      Put_Line ("Initialize" & V'Img);
   end Initialize_Vertex;

   overriding procedure Start_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      Put_Line ("Start" & V'Img);
   end Start_Vertex;

   overriding procedure Finish_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      Put_Line ("Finish" & V'Img);
   end Finish_Vertex;

   overriding procedure Discover_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      Put_Line ("Discover" & V'Img);
   end Discover_Vertex;

   function First (G : Graph) return Vertex_Cursor is
   begin
      return Vertex_Cursor (G.Colors'First);
   end First;

   function Element (G : Graph; C : Vertex_Cursor) return Vertex is
      pragma Unreferenced (G);
   begin
      return Vertex (C);
   end Element;

   function Has_Element
      (G : Graph; C : Vertex_Cursor) return Boolean is
   begin
      return C <= Vertex_Cursor (G.Colors'Last);
   end Has_Element;

   function Next
      (G : Graph; C : Vertex_Cursor) return Vertex_Cursor
   is
      pragma Unreferenced (G);
   begin
      return C + 1;
   end Next;

   function First (G : Graph; V : Vertex) return Edge_Cursor is
      pragma Unreferenced (G);
   begin
      return Edge_Cursor (V);
   end First;

   function Element (G : Graph; C : Edge_Cursor) return Edge is
      pragma Unreferenced (G);
   begin
      return (Source => Vertex (C), Target => Vertex (C + 1));
   end Element;

   function Has_Element
      (G : Graph; C : Edge_Cursor) return Boolean is
   begin
      return Integer (C) >= Integer (G.Colors'First)
         and then Integer (C) < Integer (G.Colors'Last);
   end Has_Element;

   function Next
      (G : Graph; C : Edge_Cursor) return Edge_Cursor
   is
      pragma Unreferenced (C);
   begin
      --  Only one edge from each vertex
      return Edge_Cursor (G.Colors'Last + 1);
   end Next;

   function Get_Target (G : Graph; E : Edge) return Vertex is
      pragma Unreferenced (G);
   begin
      return E.Target;
   end Get_Target;

end Graph1_Support;
