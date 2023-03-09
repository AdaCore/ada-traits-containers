--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Conts.Elements;
with Conts.Lists.Generics;
with Conts.Lists.Storage;

generic

   with package Elements is new Conts.Elements.Traits
      (Element_Type => Integer, others => <>);
   with package Storage is new Conts.Lists.Storage.Traits
      (Elements => Elements, others => <>);
   with package Lists is new Conts.Lists.Generics
      (Storage => Storage);

   with function Image (Self : Elements.Constant_Returned_Type) return String;

package Support is

   procedure Test (L1, L2 : in out Lists.List);
   --  Perform various tests.
   --  All lists should be empty on input. This is used to handle bounded
   --  lists.

end Support;
