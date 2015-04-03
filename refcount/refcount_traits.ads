with Ada.Finalization;   use Ada.Finalization;
with Interfaces;         use Interfaces;
with Conts;              use Conts;

package Refcount_Traits is
   --  Smart pointers using an Elements_Traits to save one memory
   --  allocations.

   generic
      with package Elements is new Elements_Traits (<>);
      Thread_Safe : Boolean := True;
   package Smart_Pointers is
      type Ref is tagged private;
      Null_Ref : constant Ref;

      subtype Element_Type is Elements.Element_Type;

      procedure Set (Self : in out Ref'Class; Data : Element_Type)
         with Inline => True;
      procedure Adopt (Self : in out Ref; Data : Elements.Stored_Element_Type);

      function Get (Self : Ref'Class) return Elements.Stored_Element_Type
         with Inline => True;
      function Reference (Self : Ref'Class) return Elements.Reference_Type
         with Inline => True;

      overriding function "=" (P1, P2 : Ref) return Boolean
         with Inline => True;

   private
      type Object_Refcount is record
         Refcount : aliased Interfaces.Integer_32 := 0;
         Object   : Elements.Stored_Element_Type;
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

end Refcount_Traits;
