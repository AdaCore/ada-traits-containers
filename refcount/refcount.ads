with Ada.Finalization;   use Ada.Finalization;
with Interfaces;         use Interfaces;

package Refcount is
   --  A smart pointer implementation that does not force the type to be
   --  derived from a common ancestor, at the cost of extra allocations and
   --  indirections in some cases.

   generic
      type Element_Type (<>) is private;
      with procedure Free (Self : in out Element_Type) is null;
      Thread_Safe : Boolean := True;
   package Smart_Pointers is
      type Ref is tagged private;
      Null_Ref : constant Ref;

      type Element_Access is access all Element_Type;

      procedure Set (Self : in out Ref'Class; Data : Element_Type)
         with Inline => True;
      procedure Adopt (Self : in out Ref; Data : access Element_Type);
      --  ??? Adopt is dangereous if Data comes from a call to Get.
      --  It is only meant to avoid possibly expensive copies of Data
      --  in the call to Set.

      function Get (Self : Ref'Class) return Element_Access
         with Inline => True;

      overriding function "=" (P1, P2 : Ref) return Boolean
         with Inline => True;

   private
      type Object_Refcount is record
         Refcount : aliased Interfaces.Integer_32 := 0;
         Object   : Element_Access;
         --  Object is "not null", but if we put that in the declaration we
         --  can't free the pointer later on
      end record;
      type Object_Refcount_Access is access Object_Refcount;

      type Ref is new Controlled with record
         Data : Object_Refcount_Access;
      end record;
      overriding procedure Adjust (Self : in out Ref)
         with Inline => True;
      overriding procedure Finalize (Self : in out Ref);

      Null_Ref : constant Ref :=
         (Ada.Finalization.Controlled with Data => null);

   end Smart_Pointers;

end Refcount;
