------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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
with System.Storage_Pools;  use System.Storage_Pools;
with System.Pool_Global;

package Conts with SPARK_Mode is

   subtype Count_Type is Natural;

   type Limited_Base is abstract tagged limited null record;
   --  A type that can be used as the root of a container hierarchy when a
   --  container should be limited (and thus prevent its copying).
   --  Other containers will in general derive from
   --  Ada.Finalization.Controlled.

   generic
      type Storage_Pool is new Root_Storage_Pool with private;
      Pool : access Storage_Pool;
   package Pools with SPARK_Mode => Off is
   end Pools;
   --  This package provides a way to pass storage pools as a generic parameter
   --  to other packages.
   --  Such storage pools are limited types, and thus need to be passed as
   --  access types. Furthermore, to avoid the need for dynamic dispatching, we
   --  also pass the type of the storage pool itself, rather than use a class
   --  wide type.

   package Global_Pool is new Pools
      (Storage_Pool  => System.Pool_Global.Unbounded_No_Reclaim_Pool,
       Pool => System.Pool_Global.Global_Pool_Object'Unrestricted_Access);
   --  The default storage pool used by the GNAT runtime (a direct use of
   --  malloc and free).

end Conts;
