with Ada.Finalization;   use Ada.Finalization;
with System.Storage_Pools;
with System.Storage_Elements;

package Refcount is
   --  A smart pointer implementation that does not force the type to be
   --  derived from a common ancestor, at the cost of extra allocations and
   --  indirections in some cases.

   type My_Pool is new System.Storage_Pools.Root_Storage_Pool with null record;
   overriding procedure Allocate
      (Self      : in out My_Pool;
       Addr      : out System.Address;
       Size      : System.Storage_Elements.Storage_Count;
       Alignment : System.Storage_Elements.Storage_Count);
   overriding procedure Deallocate
      (Self      : in out My_Pool;
       Addr      : System.Address;
       Size      : System.Storage_Elements.Storage_Count;
       Alignment : System.Storage_Elements.Storage_Count);
   overriding function Storage_Size
      (Self      : My_Pool) return System.Storage_Elements.Storage_Count
      is (System.Storage_Elements.Storage_Count'Last);

   Refcount_Storage_Pool : My_Pool;
   --  A storage pool that allocates extra memory to store a refcount.
   --  This is used to save calls to malloc

   generic
      type Element_Type (<>) is private;
      with procedure Free (Self : in out Element_Type) is null;
      Thread_Safe : Boolean := True;
   package Smart_Pointers is
      type Ref is tagged private;
      Null_Ref : constant Ref;

      type Element_Access is access all Element_Type;
      for Element_Access'Storage_Pool use Refcount_Storage_Pool;

      procedure Set (Self : in out Ref'Class; Data : Element_Type)
         with Inline => True;

      function Get (Self : Ref'Class) return Element_Access
         with Inline => True;
      function Element (Self : Ref'Class) return Element_Type
         is (Get (Self).all)
         with Inline => True;

      type Reference_Type (E : access Element_Type) is null record
         with Implicit_Dereference => E;
      function Reference (Self : Ref'Class) return Reference_Type
         is (Reference_Type'(E => Self.Get))
         with Inline => True;
      --  As a speed experiment, for now

      overriding function "=" (P1, P2 : Ref) return Boolean
         with Inline => True;
      --  This operator checks whether P1 and P2 share the same pointer.
      --  When the pointers differ, this operator returns False even if the
      --  two pointed elements are equal.

   private
      type Ref is new Controlled with record
         Data : Element_Access;
      end record;
      overriding procedure Adjust (Self : in out Ref)
         with Inline => True;
      overriding procedure Finalize (Self : in out Ref);

      Null_Ref : constant Ref :=
         (Ada.Finalization.Controlled with Data => null);

   end Smart_Pointers;

end Refcount;
