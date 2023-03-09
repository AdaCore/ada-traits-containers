--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Conts.Vectors.Indefinite_Unbounded;
with Support;           use Support;

procedure Main is
   package Int_Vecs is new Conts.Vectors.Indefinite_Unbounded
      (Index_Type, Integer);
   function Image (R : Int_Vecs.Constant_Returned) return String
      is (Integer'Image (R));
   procedure T is new Support.Test
      (Image           => Image,
       Elements        => Int_Vecs.Elements.Traits,
       Storage         => Int_Vecs.Storage.Traits,
       Vectors         => Int_Vecs.Vectors);
   V1 : Int_Vecs.Vector;
begin
   T (V1);
end Main;
