--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;

package body Conts.Vectors.Indefinite_Unbounded_SPARK with SPARK_Mode => Off is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   ----------
   -- Copy --
   ----------

   function Copy (Self : Vector'Class) return Vector'Class is
   begin
      return Result : Vector do
         Result.Assign (Self);
      end return;
   end Copy;

end Conts.Vectors.Indefinite_Unbounded_SPARK;
