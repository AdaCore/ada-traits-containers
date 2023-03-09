--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Ada.Finalization;
with Conts.Vectors.Definite_Unbounded;
with Support;           use Support;

procedure Main is
   package Int_Vecs is new Conts.Vectors.Definite_Unbounded
      (Index_Type, Integer, Ada.Finalization.Controlled);
   procedure T is new Support.Test
      (Image           => Integer'Image,
       Elements        => Int_Vecs.Elements.Traits,
       Storage         => Int_Vecs.Storage.Traits,
       Vectors         => Int_Vecs.Vectors);
   V1 : Int_Vecs.Vector;
begin
   T (V1);
end Main;
