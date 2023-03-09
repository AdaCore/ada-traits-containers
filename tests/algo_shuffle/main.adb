--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

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
