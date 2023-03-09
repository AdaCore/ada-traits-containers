--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package describes the types of elements stored in a container.  We
--  want to handle both constrained and unconstrained elements, which is done
--  by providing subprograms to convert from one type to the other (presumably,
--  but not limited to, using access types)

pragma Ada_2012;

package Conts.Elements with SPARK_Mode is

   generic
      type Element_Type (<>) is private;
      --  The element type visible to the user (in parameter to Append for
      --  instance).

      type Stored_Type is private;
      --  The type of elements stored internally. This must be unconstrained.

      type Returned_Type (<>) is private;
      --  The type of elements returned by getters. Various possibilities exit:
      --  you could return an Element_Type (which might be big and thus slow),
      --  a Stored_Type (which might be an access type, and thus unsafe), or a
      --  Reference type as introduced by Ada 2012. Other variations are of
      --  course possible.

      type Constant_Returned_Type (<>) is private;
      --  The type of elements returned by getters. As opposed to
      --  Returned_Type, this one guarantees that the type cannot be modified
      --  via this value (so it can't be a direct pointer, not a reference_type
      --  for which the discriminant is not "constant"). This is used in
      --  particular for the Constant_Indexing aspect.

      with function To_Stored (E : Element_Type) return Stored_Type;
      with function To_Returned (E : Stored_Type) return Returned_Type;
      with function To_Constant_Returned
        (E : Stored_Type) return Constant_Returned_Type;
      with function To_Element
        (E : Constant_Returned_Type) return Element_Type;
      --  Converting between the types

      with procedure Release (E : in out Stored_Type) is null;
      --  Called whenever an element is removed from the container.
      --  Memory can be freed at this point, and other resources can be closed.

      with function Copy (E : Stored_Type) return Stored_Type;

      Copyable : Boolean := False;
      --  True when a stored_type variable can be copied (duplicated) in
      --  memory using the standard Ada operations (assigning an array
      --  for instance), including Adjust and Finalize call when
      --  applicable.
      --  False when an explicit Copy operation needs to be performed. This
      --  is safer in general, but less efficient.
      --  It should be set to False when Stored_Type is an access type,
      --  since copying would create an alias and it would be impossible to
      --  know who the owner of the element is and when to free it.

      Movable : Boolean := True;
      --  If True, a stored_Element can be moved in memory (as part of a
      --  realloc call for instance), bypassing Adjust and Finalize calls
      --  on controlled types.
      --
      --  This is very similar to Copyable, but no aliasing issue occurs, so
      --  this should be safe for access types.
      --  When an element is not Movable, a copy is made (via Copy), and the
      --  original element is deleted.

   package Traits is
      subtype Element is Element_Type;
      subtype Stored is Stored_Type;
      subtype Returned is Returned_Type;
      subtype Constant_Returned is Constant_Returned_Type;

      function To_Elem (E : Constant_Returned_Type) return Element_Type
         renames To_Element;

      function Identity (E : Returned_Type) return Returned_Type is (E);
      --  Convenience function

   end Traits;

end Conts.Elements;
