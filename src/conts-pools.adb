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

      Header_Size : constant Storage_Count :=
         Header'Object_Size / System.Storage_Unit;
      Pointer_Size : constant Storage_Count :=
         System.Address'Size / System.Storage_Unit;
      Extra_Allocation : constant Storage_Count :=
         (Header_Size / Pointer_Size + 1) * Pointer_Size;
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
         return Convert (Addr - Extra_Allocation - Self.Descriptor_Size);
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
         Aligned_Size : constant Storage_Count :=
            Size + Extra_Allocation + Self.Descriptor_Size;
         Allocated    : System.Address;
      begin
         --  Unfortunately, we do not have access to the element type, and
         --  in particular Element_Type'Descriptor_Size, which would give use
         --  the size of the dope vector stored before the element.
         --  ??? Or we need to pass the element_type to the pool, and let it
         --  declare the element_access, so that it also sets the 'Size on it.
         --  ??? We would need to share as much of the implementation as
         --  possible.
         Allocated := Alloc (size_t (Aligned_Size));
         Addr := To_Address
            (To_Integer (Allocated) + Integer_Address (Extra_Allocation));
         --  Put_Line ("MANU allocate.size=" & Size'Img & " returned Addr="
         --     & System.Address_Image (Addr) & " allocated="
         --     & System.Address_Image (Allocated));
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
         Header : constant Extra_Header_Access :=
            Extra_Header_Access (Header_Of (Self, Addr));
      begin
         --  ??? Should take into account the alignment offset used in Allocate
         --  ??? Can we use the Alignment parameter for this, so that we do not
         --  have to store this info in the header ?
         System.Memory.Free (Convert (Header));
      end Deallocate;

   end Header_Pools;

end Conts.Pools;
