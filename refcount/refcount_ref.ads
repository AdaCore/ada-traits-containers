with Ada.Finalization;   use Ada.Finalization;
with Interfaces;         use Interfaces;

package Refcount_Ref is
   --  Smart pointers implemented as reference types.
   --  The goal is to avoid the call to Get. This however makes the smart
   --  pointer an unconstrained type.

   generic
      type Element_Type (<>) is private;
      with procedure Free (E : in out Element_Type) is null;

      Thread_Safe : Boolean := True;
      --  Used to decide whether to use atomic operations for refcount or
      --  not.
      --  ??? Would be nice to set this automatically based on whether there
      --  are tasks in the application or not (even if those tasks do not use
      --  that type).
      
   package Smart_Pointers is

      type Ref (E : access Element_Type) is private
         with Implicit_Dereference => E;
      --  ??? This is unfortunately an unconstrained type, so much harder
      --  to use in data types for instance.
      --  It also forces us to make Set a function using the secondary stack,
      --  which is much slower.
      --  Benefit: we do not need a Get function, this is implicit, and safe
      --  since users can't free the access type nor change it.
      --  It would be nice if we could set ":= null" for the discriminant.
      --    http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00402.txt?rev=1.5
      --  This is apparently just to prevent accessibility checks

      Null_Ref : constant Ref;
      --  ??? What happens when we dereference Null_Ref ?

      function Set (Data : Element_Type) return Ref
         with Inline => True;
      --  ??? Would be nicer and more efficient as a procedure, if Ref was
      --  constrained.

      overriding function "=" (P1, P2 : Ref) return Boolean
         with Inline => True;
      --  Compares the pointers, not the elements themselves.
      --  ??? Should we use another name, so that the default "=" would
      --  compare the objects themselves ?

      --  ??? Do we need a function to check whether the objet is null ?
      --  ??? Can we add support for weak reference, by having one more
      --     pointer stored in Object_Refcount, as we do in GNATCOLL ?

   private
      type Object_Refcount is access Interfaces.Integer_32;

      type Ref (E : access Element_Type) is new Controlled with record
         Refcount : Object_Refcount;
      end record;
      overriding procedure Adjust (Self : in out Ref)
         with Inline => True;
      overriding procedure Finalize (Self : in out Ref);

      Null_Ref : constant Ref :=
         (Ada.Finalization.Controlled with E => null, Refcount => null);
   end Smart_Pointers;
end Refcount_Ref;
