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

   Edges : constant array (Natural range <>) of Edge :=
      ((2, 1), (2, 4), (5, 6), (3, 2));

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

   procedure For_Each_Vertex
      (G : Graph;
       Callback : not null access procedure (V : Vertex)) is
   begin
      for J in G.Colors'Range loop
         Callback (J);
      end loop;
   end For_Each_Vertex;

   procedure For_Each_Out_Edge
      (G : Graph;
       V : Vertex;
       Callback : not null access procedure (E : Edge))
   is
      pragma Unreferenced (G);
   begin
      for E in Edges'Range loop
         if Edges (E).Source = V then
            Callback (Edges (E));
         end if;
      end loop;
   end For_Each_Out_Edge;

   function Get_Target (G : Graph; E : Edge) return Vertex is
      pragma Unreferenced (G);
   begin
      return E.Target;
   end Get_Target;

end Graph1_Support;
