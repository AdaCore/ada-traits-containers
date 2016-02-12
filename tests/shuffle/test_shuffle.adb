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

procedure Test_Shuffle is
   subtype Index_Type is Positive;

   package Rand is new Conts.Default_Random (Index_Type);
   package Int_Vecs is new Conts.Vectors.Definite_Unbounded
      (Index_Type, Integer, Ada.Finalization.Controlled);
   use Int_Vecs;

   procedure Swap (Self : in out Vector'Class; L, R : Index_Type) is
   begin
      Self.Swap (L, R);
   end Swap;

   procedure Shuffle is new Conts.Algorithms.Shuffle
      (Cursors => Int_Vecs.Cursors.Random,
       Random  => Rand.Traits);

   V : Vector;
   G : Rand.Generator;

begin
   for J in 1 .. 40 loop
      V.Append (J);
   end loop;

   Rand.Reset (G);
   Shuffle (V, G);

   for J of V loop
      Put (J'Img);
   end loop;
   New_Line;

end Test_Shuffle;
