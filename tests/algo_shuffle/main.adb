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
with Conts.Algorithms;  use Conts.Algorithms;
with Conts.Vectors.Definite_Unbounded;
with Ada.Text_IO;       use Ada.Text_IO;

procedure Main is
   subtype Index_Type is Positive;

   package Int_Vecs is new Conts.Vectors.Definite_Unbounded
      (Index_Type, Integer, Ada.Finalization.Controlled);
   use Int_Vecs;
   package Rand is new Conts.Default_Random (Extended_Index);
   procedure Shuffle is new Conts.Algorithms.Shuffle
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Random  => Rand.Traits);
   function Equals is new Conts.Algorithms.Equals
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element_From_Index);

   V, V2 : Vector;
   G : Rand.Generator;

begin
   for J in 1 .. 40 loop
      V.Append (J);
   end loop;

   Rand.Reset (G);

   V2 := V;
   Shuffle (V, G);
   if Equals (V, V2) then
      Put_Line ("Shuffle should change the order of elements");
   end if;

   V2 := V;
   Shuffle (V, G);
   if Equals (V, V2) then
      Put_Line ("Shuffle should change the order of elements");
   end if;

end Main;
