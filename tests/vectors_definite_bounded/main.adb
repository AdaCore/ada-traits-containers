--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Conts.Vectors.Definite_Bounded;
with Support;           use Support;

procedure Main is
   package Int_Vecs is new Conts.Vectors.Definite_Bounded
      (Index_Type, Integer);
   procedure T is new Support.Test
      (Image           => Integer'Image,
       Elements        => Int_Vecs.Elements.Traits,
       Storage         => Int_Vecs.Storage.Traits,
       Vectors         => Int_Vecs.Vectors);
   V1 : Int_Vecs.Vector (20);
begin
   T (V1);
end Main;
