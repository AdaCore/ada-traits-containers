--  This package provides storage pools used in various parts of this
--  library.

with System.Storage_Pools;
with System.Storage_Elements;

package Conts.Pools is

   generic
      type Element_Type (<>) is private;
      --  A pool is specific to an element type, so that we can properly
      --  handle unconstrained arrays (for which we need a specific Size
      --  representation clause and we need to take into account the bounds
      --  of the array, which are stored next to the array).

      type Extra_Header is private;
   package Header_Pools is
      type Pool is new System.Storage_Pools.Root_Storage_Pool with null record;
      --  A pool that allocates enough space for an element, plus some extra
      --  space before. This extra space can be used to store extra
      --  information associated with the memory chunk.

      function Header_Of (Addr : System.Address) return access Extra_Header
         with Inline => True;
      --  Points to the begining of the header, given an element allocated by
      --  the pool.

      overriding procedure Allocate
         (Self      : in out Pool;
          Addr      : out System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
          with Inline => True;
      overriding procedure Deallocate
         (Self      : in out Pool;
          Addr      : System.Address;
          Size      : System.Storage_Elements.Storage_Count;
          Alignment : System.Storage_Elements.Storage_Count)
          with Inline => True;
      overriding function Storage_Size
         (Self      : Pool) return System.Storage_Elements.Storage_Count
         is (System.Storage_Elements.Storage_Count'Last)
         with Inline => True;

      Element_Pool : Pool;

      type Element_Access is access all Element_Type;
      for Element_Access'Size use Standard'Address_Size;
      --  Force array bounds to be stored before the array's data, rather than
      --  as a separate dope vector.

      for Element_Access'Storage_Pool use Element_Pool;
      --  All memory allocation and deallocation for Element_Access will go
      --  through the pool.

   end Header_Pools;
end Conts.Pools;
