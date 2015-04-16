with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with Interfaces;       use Interfaces;
with System.Memory;    use System, System.Memory;

package body Refcount is

   procedure Inc_Ref (R : access Counters; Atomic : Boolean)
      with Inline => True;
   function Inc_Ref (R : access Counters; Atomic : Boolean) return Integer_32
      with Inline => True;
   function Dec_Ref (R : access Counters; Atomic : Boolean) return Integer_32
      with Inline => True;
   procedure Inc_Weak_Ref (R : access Counters; Atomic : Boolean)
      with Inline => True;
   function Dec_Weak_Ref
      (R : access Counters; Atomic : Boolean) return Integer_32
      with Inline => True;
   --  Increase/Decrease the refcount, and return the new value

   -------------
   -- Inc_Ref --
   -------------

   function Inc_Ref
      (R : access Counters; Atomic : Boolean) return Integer_32 is
   begin
      if Atomic then
         return Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         R.Refcount := R.Refcount + 1;
         return R.Refcount;
      end if;
   end Inc_Ref;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (R : access Counters; Atomic : Boolean) is
      Tmp : Integer_32;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Inc_Ref (R, Atomic);
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   function Dec_Ref
      (R : access Counters; Atomic : Boolean) return Integer_32 is
   begin
      if Atomic then
         return Sync_Add_And_Fetch (R.Refcount'Access, -1);
      else
         R.Refcount := R.Refcount - 1;
         return R.Refcount;
      end if;
   end Dec_Ref;

   ------------------
   -- Inc_Weak_Ref --
   ------------------

   procedure Inc_Weak_Ref (R : access Counters; Atomic : Boolean) is
   begin
      if Atomic then
         Sync_Add_And_Fetch (R.Weak_Refcount'Access, 1);
      else
         R.Weak_Refcount := R.Weak_Refcount + 1;
      end if;
   end Inc_Weak_Ref;

   ------------------
   -- Dec_Weak_Ref --
   ------------------

   function Dec_Weak_Ref
      (R : access Counters; Atomic : Boolean) return Integer_32 is
   begin
      if Atomic then
         return Sync_Add_And_Fetch (R.Weak_Refcount'Access, -1);
      else
         R.Weak_Refcount := R.Weak_Refcount - 1;
         return R.Weak_Refcount;
      end if;
   end Dec_Weak_Ref;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is
      use type Pools.Element_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Pools.Element_Access);

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref'Class; Data : Element_Type) is
         R : access Counters;
      begin
         Finalize (Self);
         Self.Data := new Element_Type'(Data);  --  uses storage pool
         R := Pools.Element_Pool.Header_Of (Self.Data.all'Address);
         R.Refcount := 1;
         R.Weak_Refcount := 0;
      end Set;

      ---------
      -- Get --
      ---------

      function Get (Self : Ref'Class) return access Element_Type is
      begin
         return Self.Data;
      end Get;

      ----------
      -- Weak --
      ----------

      function Weak (Self : Ref'Class) return Weak_Ref is
         R : access Counters;
      begin
         if Self.Data = null then
            return (Controlled with Data => null);
         end if;

         R := Pools.Element_Pool.Header_Of (Self.Data.all'Address);
         Inc_Weak_Ref (R, Atomic_Counters);
         return (Controlled with Data => Self.Data);
      end Weak;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (Self : Weak_Ref'Class) return Boolean is
      begin
         return Self.Data = null
            or else Pools.Element_Pool.Header_Of
               (Self.Data.all'Address).Refcount = 0;
      end Was_Freed;

      ---------
      -- Get --
      ---------

      function Get (Self : Weak_Ref'Class) return Ref is
         R : access Counters;
         Tmp : Integer_32;
      begin
         --  Thread safety: while in this function, the data still has
         --  a weak_refcount of at least 1, so will not be freed
         --  completely (although it is possible that another thread is
         --  decreasing the refcount to 0, and thus the element itself
         --  would be destroyed).

         if Self.Data = null then
            return Null_Ref;
         end if;

         R :=  Pools.Element_Pool.Header_Of (Self.Data.all'Address);
         if Inc_Ref (R, Atomic_Counters) = 1 then
            --  If the refcount is now 1, the element has in fact been freed
            --  already. So we need to reset the refcount to 0.

            Tmp := Dec_Ref (R, Atomic_Counters);
            return Null_Ref;
         else
            return (Controlled with Data => Self.Data);
         end if;
      end Get;

      ---------
      -- "=" --
      ---------

      overriding function "=" (P1, P2 : Ref) return Boolean is
      begin
         return P1.Data = P2.Data;
      end "=";

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Ref) is
      begin
         if Self.Data /= null then
            Inc_Ref (Pools.Element_Pool.Header_Of (Self.Data.all'Address),
                     Atomic_Counters);
         end if;
      end Adjust;

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (Self : in out Weak_Ref) is
      begin
         if Self.Data /= null then
            Inc_Weak_Ref
               (Pools.Element_Pool.Header_Of (Self.Data.all'Address),
                Atomic_Counters);
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         R    : access Counters;
         Data : Pools.Element_Access := Self.Data;
      begin
         if Data /= null then
            Self.Data := null;   --  make idempotent

            R := Pools.Element_Pool.Header_Of (Data.all'Address);
            if Dec_Ref (R, Atomic_Counters) = 0 then
               Release (Data.all);

               --  ??? Not thread safe, if the last weak_ref also in Finalize
               if R.Weak_Refcount = 0 then
                  Unchecked_Free (Data);
               else
                  --  Resize the pointed area to just the size of the header
                  --  to save memory
                  null;
               end if;
            end if;
         end if;
      end Finalize;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Weak_Ref) is
         R    : access Counters;
         Data : Pools.Element_Access := Self.Data;
      begin
         if Data /= null then
            Self.Data := null;   --  make idempotent

            R := Pools.Element_Pool.Header_Of (Data.all'Address);
            if Dec_Weak_Ref (R, Atomic_Counters) = 0 then
               --  ??? Not thread safe, when a Ref is also in Finalize
               if R.Refcount = 0 then
                  Unchecked_Free (Data);
               end if;
            end if;
         end if;
      end Finalize;
   end Smart_Pointers;
end Refcount;
