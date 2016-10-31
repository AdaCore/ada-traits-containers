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
with Ada.Finalization;
with Conts.Vectors.Definite_Bounded;
with Conts.Vectors.Definite_Unbounded;
with Conts.Vectors.Indefinite_Unbounded;
with Conts.Vectors.Indefinite_Unbounded_SPARK;
with Ada.Text_IO;       use Ada.Text_IO;
with Support;           use Support;

procedure Main is
begin

   Put_Line ("--- Definite Bounded ---");
   declare
      package Int_Vecs is new Conts.Vectors.Definite_Bounded
         (Index_Type, Integer);
      procedure T is new Support.Test
         (Image           => Integer'Image,
          Elements        => Int_Vecs.Elements.Traits,
          Storage         => Int_Vecs.Storage.Traits,
          Vectors         => Int_Vecs.Vectors);
      V1, V2 : Int_Vecs.Vector (20);
   begin
      T (V1, V2);
   end;

   Put_Line ("--- Definite Unbounded ---");
   declare
      package Int_Vecs is new Conts.Vectors.Definite_Unbounded
         (Index_Type, Integer, Ada.Finalization.Controlled);
      procedure T is new Support.Test
         (Image           => Integer'Image,
          Elements        => Int_Vecs.Elements.Traits,
          Storage         => Int_Vecs.Storage.Traits,
          Vectors         => Int_Vecs.Vectors);
      V1, V2 : Int_Vecs.Vector;
   begin
      T (V1, V2);
   end;

   Put_Line ("--- Indefinite Unbounded SPARK ---");
   declare
      package Int_Vecs is new Conts.Vectors.Indefinite_Unbounded_SPARK
         (Index_Type, Integer);
      procedure T is new Support.Test
         (Image           => Integer'Image,
          Elements        => Int_Vecs.Elements.Traits,
          Storage         => Int_Vecs.Storage.Traits,
          Vectors         => Int_Vecs.Vectors);
      V1, V2 : Int_Vecs.Vector;
   begin
      T (V1, V2);
   end;

   Put_Line ("--- Indefinite Unbounded ---");
   declare
      package Int_Vecs is new Conts.Vectors.Indefinite_Unbounded
         (Index_Type, Integer);
      function Image (R : Int_Vecs.Constant_Returned) return String
         is (Integer'Image (R));
      procedure T is new Support.Test
         (Image           => Image,
          Elements        => Int_Vecs.Elements.Traits,
          Storage         => Int_Vecs.Storage.Traits,
          Vectors         => Int_Vecs.Vectors);
      V1, V2 : Int_Vecs.Vector;
   begin
      T (V1, V2);
   end;
end Main;
