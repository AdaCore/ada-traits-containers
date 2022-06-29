--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Conts.Lists.Indefinite_Unbounded;
with Support;

procedure Main is
   package Int_Lists is new Conts.Lists.Indefinite_Unbounded (Integer);
   function Image (R : Int_Lists.Constant_Returned) return String
      is (Integer'Image (R));
   package Tests is new Support
      (Image        => Image,
       Elements     => Int_Lists.Elements.Traits,
       Storage      => Int_Lists.Storage.Traits,
       Lists        => Int_Lists.Lists);
   L1, L2 : Int_Lists.List;
begin
   Tests.Test (L1, L2);
end Main;
