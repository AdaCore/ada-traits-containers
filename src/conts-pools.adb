with Ada.Unchecked_Conversion;
with System.Memory;           use System, System.Memory;
with System.Storage_Elements; use System.Storage_Elements;

package body Conts.Pools is

   ------------------
   -- Header_Pools --
   ------------------

   package body Header_Pools is

      type Header is record
         Extra : Extra_Header;
      end record;
      type Extra_Header_Access is access all Extra_Header;

      Header_Size_Bytes : constant Storage_Count :=
         Header'Size / Storage_Unit;
      Pointer_Size_Bytes : constant Storage_Count :=
         System.Address'Size / Storage_Unit;
      Extra_Allocation_Bytes : constant Storage_Count :=
         (Header_Size_Bytes / Pointer_Size_Bytes + 1)
         * Pointer_Size_Bytes;
      --   Allocate a multiple of Pointer_Size bytes, so that the
      --   alignment of the Element_Type is suitable.
      --   ??? Should we use Standard'Maximum_Alignment instead, although
      --   this wastes more space.

      function Convert is new Ada.Unchecked_Conversion
         (System.Address, Extra_Header_Access);
      function Convert is new Ada.Unchecked_Conversion
         (Extra_Header_Access, System.Address);

      ---------------
      -- Header_Of --
      ---------------

      function Header_Of
         (Self : Pool; Addr : System.Address) return access Extra_Header is
      begin
         return Convert (Addr - Extra_Allocation_Bytes - Self.Descriptor_Size);
      end Header_Of;

      --------------
      -- Allocate --
      --------------

      overriding procedure Allocate
         (Self      : in out Pool;
          Addr      : out System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
      is
         pragma Unreferenced (Alignment);
         Aligned_Size : constant Storage_Count :=   --  bytes
            Size + Extra_Allocation_Bytes + Self.Descriptor_Size;
         Allocated    : System.Address;
      begin
         Allocated := Alloc (size_t (Aligned_Size));
         Addr := To_Address
            (To_Integer (Allocated) +
            Integer_Address (Extra_Allocation_Bytes + Self.Descriptor_Size));
      end Allocate;

      ----------------
      -- Deallocate --
      ----------------

      overriding procedure Deallocate
         (Self      : in out Pool;
          Addr      : System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
      is
         pragma Unreferenced (Size, Alignment);
         Header : Extra_Header_Access;
      begin
         Header := Extra_Header_Access (Header_Of (Self, Addr));
         System.Memory.Free (Convert (Header));
      end Deallocate;

   end Header_Pools;

end Conts.Pools;
