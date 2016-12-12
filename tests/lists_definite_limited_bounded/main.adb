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
