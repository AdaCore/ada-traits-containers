--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with Ada.Unchecked_Deallocation;

package body Conts.Elements.Indefinite_SPARK with SPARK_Mode => Off is

   package body Impl with SPARK_Mode => Off is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Element_Access);

      ----------
      -- Free --
      ----------

      procedure Free (X : in out Element_Access) is
      begin
         Unchecked_Free (X);
      end Free;

   end Impl;

end Conts.Elements.Indefinite_SPARK;
