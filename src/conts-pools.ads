--  This package provides storage pools used in various parts of this
--  library.

with System.Storage_Pools;    use System.Storage_Pools;
with System.Storage_Elements; use System.Storage_Elements;

package Conts.Pools is

   generic
      type Extra_Header is private;
   package Header_Pools is
      type Pool (Descriptor_Size : Storage_Count)
          is new Root_Storage_Pool with null record;
      --  A pool that allocates enough space for an element, plus some extra
      --  space before. This extra space can be used to store extra
      --  information associated with the memory chunk.
      --  The descriptor size is given by the element_type 'Descriptor_Size
      --  attribute (see also Header_Pools package below)

      function Header_Of
         (Self : Pool; Addr : System.Address) return access Extra_Header
         with Inline => True;
      --  Points to the begining of the header, given an element allocated by
      --  the pool.

      overriding procedure Allocate
         (Self      : in out Pool;
          Addr      : out System.Address;
          Size      : Storage_Count;
          Alignment : Storage_Count);
      overriding procedure Deallocate
         (Self      : in out Pool;
          Addr      : System.Address;
          Size      : Storage_Count;
          Alignment : Storage_Count);
      overriding function Storage_Size
         (Self      : Pool) return Storage_Count is (Storage_Count'Last)
         with Inline => True;
   end Header_Pools;

   generic
      with package Pools is new Header_Pools (<>);

      type Element_Type (<>) is private;
      --  A pool is specific to an element type, so that we can properly
      --  handle unconstrained arrays (for which we need a specific Size
      --  representation clause and we need to take into account the bounds
      --  of the array, which are stored next to the array).
   package Typed_Header_Pools is
      Element_Pool : Pools.Pool (Element_Type'Descriptor_Size);

      type Element_Access is access all Element_Type;
      for Element_Access'Size use Standard'Address_Size;
      --  Force array bounds to be stored before the array's data, rather than
      --  as a separate dope vector.

      for Element_Access'Storage_Pool use Element_Pool;
      --  All memory allocation and deallocation for Element_Access will go
      --  through the pool.
   end Typed_Header_Pools;

end Conts.Pools;
