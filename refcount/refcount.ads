with Ada.Finalization;   use Ada.Finalization;
with Interfaces;         use Interfaces;
with Conts.Pools;

package Refcount is
   --  A smart pointer implementation that does not force the type to be
   --  derived from a common ancestor, at the cost of extra allocations and
   --  indirections in some cases.

   type Counters is record
      Refcount      : aliased Interfaces.Integer_32 := 1;
      Weak_Refcount : aliased Interfaces.Integer_32 := 0;
   end record;
   package Headers is new Conts.Pools.Header_Pools (Counters);

   generic
      type Element_Type (<>) is private;

      with procedure Release (Self : in out Element_Type) is null;
      --  This procedure should be used if you need to perform actions when
      --  the last reference to an element is removed. Typically, this is
      --  used to free element_type and its contents, when it is not a
      --  controlled type.

      Atomic_Counters : Boolean := True;
      --  Whether to use atomic (and thus thread-safe) counters.

   package Smart_Pointers is
      type Ref is tagged private;
      Null_Ref : constant Ref;
      --  This type acts like a pointer, but holds a reference to the object,
      --  which will thus never been free while there exists at least one
      --  reference to it.

      procedure Set (Self : in out Ref'Class; Data : Element_Type)
         with Inline => True;

      function Get (Self : Ref'Class) return access Element_Type
         with Inline => True;
      --  The resulting access must not be deallocated. Passing it to
      --  Set might also be dangerous if the Element_Type contains data
      --  that might be freed when other smart pointers are freed.

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

      --  ??? Could provide a "<" for use in sorted containers

      type Weak_Ref is tagged private;
      --  A weak reference to an object. The value returned by Get will be
      --  reset to null when the object is freed (because its last reference
      --  expired). Holding a weak reference does not prevent the deallocation
      --  of the object

      function Weak (Self : Ref'Class) return Weak_Ref;
      function Get (Self : Weak_Ref'Class) return Ref;
      --  Get returns a reference to the object. Otherwise, it would be
      --  possible for a procedure to retrieve a pointer from the weak
      --  reference, and then reference it throughout the procedure, even
      --  though the pointer might be free in between.

      function Was_Freed (Self : Weak_Ref'Class) return Boolean
         with Inline => True;
      --  True if the object referenced by Self was freed.

   private
      package Pools is
         new Conts.Pools.Typed_Header_Pools (Headers, Element_Type);
      --  ??? Could we share a pool for all types that are not an
      --  unconstrained array.

      type Ref is new Controlled with record
         Data : Pools.Element_Access;
      end record;
      overriding procedure Adjust (Self : in out Ref)
         with Inline => True;
      overriding procedure Finalize (Self : in out Ref);

      type Weak_Ref is new Controlled with record
         Data : Pools.Element_Access;
      end record;
      overriding procedure Adjust (Self : in out Weak_Ref)
         with Inline => True;
      overriding procedure Finalize (Self : in out Weak_Ref);

      Null_Ref : constant Ref :=
         (Ada.Finalization.Controlled with Data => null);

   end Smart_Pointers;

end Refcount;
