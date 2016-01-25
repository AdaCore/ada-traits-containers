------------------------------------------------------------------------------
--                     Copyright (C) 2015-2016, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

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

      type Return_Type (<>) is private;
      --  The type of elements returned by getters. Various possibilities exit:
      --  you could return an Element_Type (which might be big and thus slow),
      --  a Stored_Type (which might be an access type, and thus unsafe), or a
      --  Reference type as introduced by Ada 2012. Other variations are of
      --  course possible.

      with function To_Stored (E : Element_Type) return Stored_Type;
      with function To_Return (E : Stored_Type) return Return_Type;
      with function To_Element (E : Return_Type) return Element_Type;
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
   end Traits;

end Conts.Elements;
