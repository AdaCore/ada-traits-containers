--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;

package body Conts.Lists.Indefinite_Unbounded_SPARK with SPARK_Mode => Off is

   ----------
   -- Copy --
   ----------

   function Copy (Self : List'Class) return List'Class is
   begin
      return Result : List do
         Result.Assign (Self);
      end return;
   end Copy;

end Conts.Lists.Indefinite_Unbounded_SPARK;
