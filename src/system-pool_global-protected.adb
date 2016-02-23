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

with System.Storage_Pools; use System.Storage_Pools;
with System.Memory;

package body System.Pool_Global.Protected is

   package SSE renames System.Storage_Elements;

   -------------------
   -- Locked_Memory --
   -------------------

   protected body Locked_Memory is 

      function Storage_Size
        return SSE.Storage_Count
      is
      
      begin
         return Storage_Size (Global_Pool_Object);
      end Storage_Size;

      procedure Allocate
        (Address      : out System.Address;
         Storage_Size : SSE.Storage_Count;
         Alignment    : SSE.Storage_Count)
      is

      begin
         Allocate (Global_Pool_Object, Address, Storage_Size, Alignment);
      end Allocate;

      procedure Deallocate
        (Address      : System.Address;
         Storage_Size : SSE.Storage_Count;
         Alignment    : SSE.Storage_Count)
      is

      begin
         Deallocate (Global_Pool_Object, Address, Storage_Size, Alignment);
      end Deallocate;

   end Locked_Memory;

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Protected_Unbounded_No_Reclaim_Pool;
      Address      : out System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is

   begin
      Pool.Memory.Allocate (Address, Storage_Size, Alignment);
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
     (Pool         : in out Protected_Unbounded_No_Reclaim_Pool;
      Address      : System.Address;
      Storage_Size : SSE.Storage_Count;
      Alignment    : SSE.Storage_Count)
   is

   begin
      Pool.Memory.Deallocate (Address, Storage_Size, Alignment);
   end Deallocate;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool  : Protected_Unbounded_No_Reclaim_Pool)
      return  SSE.Storage_Count
   is

   begin
      return Pool.Memory.Storage_Size;
   end Storage_Size;

end System.Pool_Global.Protected;