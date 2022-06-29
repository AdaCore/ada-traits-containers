--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Ada.Text_IO; use Ada.Text_IO;

package body Graph1_Support is

   Output : constant Boolean := False;

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
      if Output then
         Put_Line ("Initialize" & V'Img);
      end if;
   end Initialize_Vertex;

   overriding procedure Start_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      if Output then
         Put_Line ("Start" & V'Img);
      end if;
   end Start_Vertex;

   overriding procedure Finish_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      if Output then
         Put_Line ("Finish" & V'Img);
      end if;
   end Finish_Vertex;

   overriding procedure Discover_Vertex
      (Self : in out My_Visitor2; G : Graph; V : Vertex)
   is
      pragma Unreferenced (Self, G);
   begin
      if Output then
         Put_Line ("Discover" & V'Img);
      end if;
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
