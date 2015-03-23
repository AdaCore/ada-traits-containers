--  This package describes the types of elements stored in a container.
--  We want to handle both constrained and unconstrained elements, which is
--  done by providing subprograms to convert from one type to the other
--  (presumably, but not limited to, using access types)

pragma Ada_2012;

generic
   type Element_Type (<>) is private;
   --  The element type visible to the user (in parameter to Append, and
   --  returned by Element, for instance)

   type Stored_Element_Type is private;
   --  The type of elements stored internally. This must be unconstrained.

   with function Convert_From (E : Element_Type) return Stored_Element_Type;
   with function Convert_To (E : Stored_Element_Type) return Element_Type;
   --  Converting between the two types

   with procedure Release (E : in out Stored_Element_Type) is null;
   --  Called whenever a value of Element_Type is removed from the table.
   --  This can be used to add unconstrained types to the list: Element_Type
   --  would then be a pointer to that type, and Release is a call to
   --  Unchecked_Deallocation.
   
package Conts.Generic_Elements is
   --  pragma Unreferenced (Convert_From, Convert_To, Release);
   --  Can't use the pragma above since other packages need those. But then
   --  the compiler is complaining that these formal parameters are unused
end Conts.Generic_Elements;

