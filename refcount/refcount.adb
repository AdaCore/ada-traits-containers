with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Atomic;  use GNATCOLL.Atomic;
with Interfaces;       use Interfaces;
with System.Memory;    use System, System.Memory;
with System.Storage_Elements; use System.Storage_Elements;

package body Refcount is

   type Allocation_Header is record
      Refcount : aliased Interfaces.Integer_32;
   end record;
   type Allocation_Header_Access is access Allocation_Header;
   Header_Size : constant Storage_Count :=
      Allocation_Header'Object_Size / System.Storage_Unit;
   --  Offset in bytes from start of allocation header to start of user data.

   Extra_Allocation : constant Storage_Count :=
      Standard'Maximum_Alignment - 1 + Header_Size;

   function Convert is new Ada.Unchecked_Conversion
      (System.Address, Allocation_Header_Access);
   function Convert is new Ada.Unchecked_Conversion
      (Allocation_Header_Access, System.Address);

   function Header_Of (Addr : System.Address) return Allocation_Header_Access
      is (Convert (Addr - Header_Size));
   --  Beginning of the header given an address used by the user

   function Get_Refcount
      (Addr : System.Address) return access Interfaces.Integer_32;
   --  Return the address of the refcount for memory allocated via the
   --  storage pool above.

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
      (Self      : in out My_Pool;
       Addr      : out System.Address;
       Size      : System.Storage_Elements.Storage_Count;
       Alignment : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Self, Alignment);
      type Local_Array is new Storage_Array (1 .. Size + Extra_Allocation);
      type Ptr is access Local_Array;
      P : Ptr;
      function Align (Addr : Integer_Address) return Integer_Address
         is (Addr);  --  ??? To be fixed
   begin
      --  Use standard allocations (through malloc and system.memory)
      P := new Local_Array;
      Addr := To_Address
         (Align (To_Integer (P.all'Address) + Integer_Address (Header_Size)));
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   overriding procedure Deallocate
      (Self      : in out My_Pool;
       Addr      : System.Address;
       Size      : System.Storage_Elements.Storage_Count;
       Alignment : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Self, Size, Alignment);
      Header : constant Allocation_Header_Access := Header_Of (Addr);
   begin
      --  ??? Should take into account the alignment offset used in Allocate.
      --  ??? Can we use the Alignment parameter for this, so that we do not
      --  have to store this info in the header ?
      System.Memory.Free (Convert (Header));
   end Deallocate;

   ------------------
   -- Get_Refcount --
   ------------------

   function Get_Refcount
      (Addr : System.Address) return access Interfaces.Integer_32
   is
   begin
      return Header_Of (Addr).Refcount'Access;
   end Get_Refcount;

   --------------------
   -- Smart_Pointers --
   --------------------

   package body Smart_Pointers is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Element_Type, Element_Access);

      ---------
      -- Set --
      ---------

      procedure Set (Self : in out Ref'Class; Data : Element_Type) is
      begin
         Finalize (Self);
         Self.Data := new Element_Type'(Data);
         Get_Refcount (Self.Data.all'Address).all := 1;
      end Set;

      ---------
      -- Get --
      ---------

      function Get (Self : Ref'Class) return Element_Access is
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
            R := Get_Refcount (Self.Data.all'Address);
            if Thread_Safe then
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
         Data : Element_Access := Self.Data;
         Tmp  : Interfaces.Integer_32;
      begin
         Self.Data := null;   --  make idempotent
         if Data /= null then
            R := Get_Refcount (Data.all'Address);
            if Thread_Safe then
               Tmp := Sync_Add_And_Fetch (R, -1);
            else
               R.all := R.all - 1;
               Tmp := R.all;
            end if;

            if Tmp = 0 then
               Free (Data.all);
               Unchecked_Free (Data);
            end if;
         end if;
      end Finalize;
   end Smart_Pointers;
end Refcount;
