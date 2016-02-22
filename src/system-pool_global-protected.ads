------------------------------------------------------------------------------
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

with System;
with System.Storage_Pools;
with System.Storage_Elements;

package System.Pool_Global.Protected is
   pragma Elaborate_Body;

   type Protected_Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with private;

   overriding function Storage_Size
     (Pool : Protected_Unbounded_No_Reclaim_Pool)
      return System.Storage_Elements.Storage_Count;

   overriding procedure Allocate
     (Pool         : in out Protected_Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool         : in out Protected_Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   --Protected_Global_Pool_Object : Protected_Unbounded_No_Reclaim_Pool;

private

   protected type Locked_Memory is 

      function Storage_Size
        return System.Storage_Elements.Storage_Count;

      procedure Allocate
        (Address      : out System.Address;
         Storage_Size : System.Storage_Elements.Storage_Count;
         Alignment    : System.Storage_Elements.Storage_Count);

      procedure Deallocate
        (Address      : System.Address;
         Storage_Size : System.Storage_Elements.Storage_Count;
         Alignment    : System.Storage_Elements.Storage_Count);

   end Locked_Memory;

   type Protected_Unbounded_No_Reclaim_Pool is new
     System.Storage_Pools.Root_Storage_Pool with record
      Memory : Locked_Memory;
   end record;

end System.Pool_Global.Protected;