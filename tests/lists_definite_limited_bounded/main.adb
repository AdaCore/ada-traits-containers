--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Lists.Generics;
with Conts.Lists.Storage.Bounded;
with Support;

procedure Main is
   package E is new Conts.Elements.Definite (Integer);
   package S is new Conts.Lists.Storage.Bounded
      (E.Traits, Conts.Limited_Base);
   package Int_Lists is new Conts.Lists.Generics (S.Traits);
   package Tests is new Support
      (Image        => Integer'Image,
       Elements     => E.Traits,
       Storage      => S.Traits,
       Lists        => Int_Lists);
   L1, L2 : Int_Lists.List (20);
begin
   Tests.Test (L1, L2);
end Main;
