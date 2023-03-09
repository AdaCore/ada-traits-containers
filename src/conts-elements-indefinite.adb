--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Conts.Elements.Indefinite is

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Element_Type, Element_Access);

   -------------
   -- Release --
   -------------

   procedure Release (E : in out Element_Access) is
   begin
      if E /= null then
         Free (E.all);
         Unchecked_Free (E);
      end if;
   end Release;

end Conts.Elements.Indefinite;
