--
--  Copyright (C) 2001-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This is the default implementation of this package

--  This implementation assumes that the underlying malloc/free/realloc
--  implementation is thread safe, and thus, no additional lock is required.
--  Note that we still need to defer abort because on most systems, an
--  asynchronous signal (as used for implementing asynchronous abort of
--  task) cannot safely be handled while malloc is executing.

--  If you are not using Ada constructs containing the "abort" keyword, then
--  you can remove the calls to Abort_Defer.all and Abort_Undefer.all from
--  this unit.

pragma Compiler_Unit_Warning;

with Ada.Exceptions;
with System.Soft_Links;
with System.Parameters;
with System.CRTL;
with Memory;

package body System.Memory is

   use Ada.Exceptions;
   use System.Soft_Links;

   function c_malloc (Size : System.CRTL.size_t) return System.Address
    renames System.CRTL.malloc;

   procedure c_free (Ptr : System.Address)
     renames System.CRTL.free;

   function c_realloc
     (Ptr : System.Address; Size : System.CRTL.size_t) return System.Address
     renames System.CRTL.realloc;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      if Parameters.No_Abort then
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      if not Standard.Memory.Paused then
         Standard.Memory.Current.Total_Allocated :=
           Standard.Memory.Current.Total_Allocated
           + Long_Long_Integer (Actual_Size);
         Standard.Memory.Current.Allocs := Standard.Memory.Current.Allocs + 1;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      if not Standard.Memory.Paused then
         Standard.Memory.Current.Frees := Standard.Memory.Current.Frees + 1;
      end if;

      if Parameters.No_Abort then
         c_free (Ptr);
      else
         Abort_Defer.all;
         c_free (Ptr);
         Abort_Undefer.all;
      end if;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      if Parameters.No_Abort then
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      if not Standard.Memory.Paused then
         Standard.Memory.Current.Reallocs :=
           Standard.Memory.Current.Reallocs + 1;
         Standard.Memory.Current.Total_Allocated :=
           Standard.Memory.Current.Total_Allocated
           + Long_Long_Integer (Actual_Size);
      end if;

      return Result;
   end Realloc;

end System.Memory;
