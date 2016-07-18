------------------------------------------------------------------------------
--                     Copyright (C) 2016-2016, AdaCore                     --
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
