--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2012;

package body Conts.Maps.Indef_Indef_Unbounded_SPARK with SPARK_Mode => Off is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ----------
   -- Copy --
   ----------

   function Copy (Self : Map'Class) return Map'Class is
   begin
      return Result : Map do
         Result.Assign (Self);
      end return;
   end Copy;

end Conts.Maps.Indef_Indef_Unbounded_SPARK;
