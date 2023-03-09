--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;
with Conts.Elements;
with Conts.Vectors.Generics;
with Conts.Vectors.Storage;

package Support is
   subtype Index_Type is Positive;

   generic
      with package Elements is new Conts.Elements.Traits
         (Element_Type => Integer, others => <>);
      with package Storage is new Conts.Vectors.Storage.Traits
         (Elements => Elements, others => <>);
      with package Vectors is new Conts.Vectors.Generics
         (Storage => Storage, Index_Type => Index_Type);
      with function Image (Self : Elements.Constant_Returned) return String;
   procedure Test (V1 : in out Vectors.Vector);
   --  Perform various tests.
   --  All vectors should be empty on input. This is used to handle bounded
   --  vectors.

end Support;
