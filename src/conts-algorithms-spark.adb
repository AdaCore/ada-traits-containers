--
--  Copyright (C) 2016-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;

package body Conts.Algorithms.SPARK is

   ----------
   -- Find --
   ----------

   function Find
     (Self : Cursors.Container;
      E    : Getters.Element)
      return Cursors.Cursor
     with SPARK_Mode => Off
   is
      function Find_Impl is
        new Conts.Algorithms.Find (Cursors, Getters, "=");
   begin
      return Find_Impl (Self, E);
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains
     (Self : Cursors.Container;
      E    : Getters.Element)
      return Boolean
     with SPARK_Mode => Off
   is
      function Contains_Impl is
        new Conts.Algorithms.Contains (Cursors, Getters, "=");
   begin
      return Contains_Impl (Self, E);
   end Contains;

   ------------
   -- Equals --
   ------------

   function Equals (Left, Right  : Cursors.Container) return Boolean
     with SPARK_Mode => Off
   is
      function Equals_Impl is
        new Conts.Algorithms.Equals (Cursors, Getters, "=");
   begin
      return Equals_Impl (Left, Right);
   end Equals;

end Conts.Algorithms.SPARK;
