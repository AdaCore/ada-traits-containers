with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with Interfaces;       use Interfaces;
with System.Memory;    use System, System.Memory;

package body Refcount is

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
      begin
         Finalize (Self);
         Self.Data := new Element_Type'(Data);  --  uses storage pool
         Pools.Element_Pool.Header_Of (Self.Data.all'Address).all := 1;
      end Set;

      ---------
      -- Get --
      ---------

      function Get (Self : Ref'Class) return access Element_Type is
      begin
         return Self.Data;
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
         R : access Interfaces.Integer_32;
      begin
         if Self.Data /= null then
            R := Pools.Element_Pool.Header_Of (Self.Data.all'Address);
            --  R is the 4 bytes before Self.Data. But in the case of an
            --  unconstrained array, the bounds are stored just before
            --  Self.Data already (provided the access type has a 'Size
            --  equal to address'Size). So in effect here we are changing
            --  the 'Last bound, not the reference counter.
            if Atomic_Counters then
               Sync_Add_And_Fetch (R, 1);
            else
               R.all := R.all + 1;
            end if;
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         R : access Interfaces.Integer_32;
         Data : Pools.Element_Access := Self.Data;
         Tmp  : Interfaces.Integer_32;
      begin
         if Data /= null then
            Self.Data := null;   --  make idempotent

            R := Pools.Element_Pool.Header_Of (Data.all'Address);
            if Atomic_Counters then
               Tmp := Sync_Add_And_Fetch (R, -1);
            else
               R.all := R.all - 1;
               Tmp := R.all;
            end if;

            if Tmp = 0 then
               Release (Data.all);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;
   end Smart_Pointers;
end Refcount;
