--  Notes on the implementation of weak pointers:
--  There are several ways in which a weak pointer can be implemented:
--    - Using two counters (one for full references, one for weak). When both
--      reach 0, the memory blocks is freed; when only the first reaches 0,
--      the element is released, and the block can be resized.
--      This is hard to make task safe without using critical section though.
--    - store a doubly-linked list of weak pointers along with the counter.
--      When the counter reaches 0, change each of the weak pointers to null.
--      This requires more memory.
--    - (our choice) make the weak pointer a smart pointer pointing to the
--      same data:
--           smart_ptr ---> chunk1: counter + element + pointer to chunk2
--           weak_ptr  ---> chunk2: weak_counter + pointer to chunk1

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with Interfaces;       use Interfaces;
with System.Memory;    use System, System.Memory;

package body Refcount is

   procedure Inc_Ref (R : access Counters; Atomic : Boolean)
      with Inline => True;
   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean)
      with Inline => True;
   --  Increase/Decrease the refcount, and return the new value

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (Weak_Data, Weak_Data_Access);

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean);
   --  Decrease refcount, and free memory if needed

   function Sync_Bool_Compare_And_Swap
      (Ptr    : in out Weak_Data_Access;
       Oldval : Weak_Data_Access;
       Newval : Weak_Data_Access) return Integer_8;
   pragma Import
      (C, Sync_Bool_Compare_And_Swap, "ada_sync_bool_compare_and_swap");

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (R : access Counters; Atomic : Boolean) is
   begin
      if Atomic then
         Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         R.Refcount := R.Refcount + 1;
      end if;
   end Inc_Ref;

   procedure Inc_Ref (R : access Weak_Data; Atomic : Boolean) is
   begin
      if Atomic then
         Sync_Add_And_Fetch (R.Refcount'Access, 1);
      else
         R.Refcount := R.Refcount + 1;
      end if;
   end Inc_Ref;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Data : in out Weak_Data_Access; Atomic : Boolean) is
      Tmp  : Integer_32;
   begin
      if Atomic then
         Tmp := Sync_Add_And_Fetch (Data.Refcount'Access, -1);
      else
         Data.Refcount := Data.Refcount - 1;
      end if;

      if Tmp = 0 then
         Unchecked_Free (Data);
      end if;
   end Finalize;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is
      use type Pools.Element_Access;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Pools.Element_Access);

      pragma Warnings (Off, "*possible aliasing problem*");
      function Convert is new Ada.Unchecked_Conversion
         (Pools.Element_Access, System.Address);
      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Pools.Element_Access);
      pragma Warnings (On, "*possible aliasing problem*");

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
         R.Weak_Data := null;
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
         V : Weak_Data_Access;
      begin
         if Self.Data = null then
            return Null_Weak_Ref;
         end if;

         R := Pools.Element_Pool.Header_Of (Self.Data.all'Address);

         if R.Weak_Data = null then
            V := new Weak_Data'
               (Refcount => 2,   --  hold by Self and the result
                Element  => Convert (Self.Data));
            if Sync_Bool_Compare_And_Swap
               (R.Weak_Data, Oldval => null, Newval => V) = 0
            then
               --  Was set by another thread concurrently
               Unchecked_Free (V);

               --  Need to increase refcount for the new weak ref
               Inc_Ref (R.Weak_Data, Atomic_Counters);
            end if;
         end if;

         return (Controlled with Data => R.Weak_Data);
      end Weak;

      ---------------
      -- Was_Freed --
      ---------------

      function Was_Freed (Self : Weak_Ref'Class) return Boolean is
      begin
         return Self.Data = null
            or else Self.Data.Element = System.Null_Address;
      end Was_Freed;

      ---------
      -- Get --
      ---------

      function Get (Self : Weak_Ref'Class) return Ref is
      begin
         if Self.Was_Freed then
            return Null_Ref;
         end if;

         Inc_Ref
            (Pools.Element_Pool.Header_Of (Self.Data.Element),
             Atomic_Counters);
         return (Controlled with Data => Convert (Self.Data.Element));
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
            Inc_Ref (Self.Data, Atomic_Counters);
         end if;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Ref) is
         R    : access Counters;
         Data : Pools.Element_Access := Self.Data;
         Tmp  : Integer_32;
      begin
         if Data /= null then
            Self.Data := null;

            R := Pools.Element_Pool.Header_Of (Data.all'Address);
            if Atomic_Counters then
               Tmp := Sync_Add_And_Fetch (R.Refcount'Access, -1);
            else
               R.Refcount := R.Refcount - 1;
               Tmp := R.Refcount;
            end if;

            if Tmp = 0 then
               if R.Weak_Data /= null then
                  R.Weak_Data.Element := System.Null_Address;
                  Finalize (R.Weak_Data, Atomic_Counters);
               end if;

               Release (Data.all);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (Self : in out Weak_Ref) is
         Data : Weak_Data_Access := Self.Data;
      begin
         if Data /= null then
            Self.Data := null;
            Finalize (Data, Atomic_Counters);
         end if;
      end Finalize;
   end Smart_Pointers;
end Refcount;
