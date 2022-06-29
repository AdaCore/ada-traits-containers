--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Conts.Lists.Indefinite_Unbounded_SPARK;
with Support;

procedure Main is
   package Int_Lists is new Conts.Lists.Indefinite_Unbounded_SPARK
      (Integer);
   package Tests is new Support
      (Image        => Integer'Image,
       Elements     => Int_Lists.Elements.Traits,
       Storage      => Int_Lists.Storage.Traits,
       Lists        => Int_Lists.Lists);
   L1, L2 : Int_Lists.List;
begin
   Tests.Test (L1, L2);
end Main;
