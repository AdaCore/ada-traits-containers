--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
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
   function Equals is new Conts.Algorithms.Equals
      (Cursors => Int_Vecs.Cursors.Random_Access,
       Getters => Int_Vecs.Maps.Element_From_Index);

   V1, V2 : Vector;

begin
   if not Equals (V1, V2) then
      Put_Line ("Empty Vectors should be equal");
   end if;

   for J in 1 .. 40 loop
      V1.Append (J);
   end loop;

   if Equals (V1, V2) then
      Put_Line ("Comparing non-empty and empty Vectors should not be equal");
   end if;
   if Equals (V2, V1) then
      Put_Line ("Comparing empty and non-empty Vectors should not be equal");
   end if;
   if not Equals (V2, V2) then
      Put_Line ("Comparing with self should be equal");
   end if;

   for J in 1 .. 39 loop
      V2.Append (J);
   end loop;

   if Equals (V1, V2) then
      Put_Line ("Vectors of different lengths should not be equal");
   end if;

   V2.Append (40);

   if not Equals (V1, V2) then
      Put_Line ("Vectors should be equal");
   end if;

end Main;
