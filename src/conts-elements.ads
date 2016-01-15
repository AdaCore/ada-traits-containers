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
      --  The type of elements returned by getters. Various possibilities
      --  exit: you could return an Element_Type (which might be big and thus
      --  slow), a Stored_Element_Type (which might be an access type, and
      --  thus unsafe), or a Reference type as introduced by Ada 2012. Other
      --  variations are of course possible.

      with function To_Stored (E : Element_Type) return Stored_Type;
      with function To_Return (E : Stored_Type) return Return_Type;
      --  Converting between the types

      with procedure Release (E : in out Stored_Type) is null;
      --  Called whenever an element is removed from the container.
      --  Memory can be freed at this point, and other resources can be closed.

      with function Copy (E : Stored_Type) return Stored_Type;
      Copyable : Boolean;
      --  If True, a stored_type can be copied in memory using
      --  low-level function calls that bypass the Adjust and Finalize
      --  primitives of controlled types. This speeds things up significantly
      --  in some cases. In this case, Copy is never used.
      --  This should be set to False when the element is potentially a
      --  controlled type, to preserve correct semantics. In this case, copies
      --  will be done via  Convert_From (Convert_To (E)).
      --  It should also be set to False for most pointer types, since copying
      --  would create an alias and it would be impossible to know who the
      --  owner of the element is and when to free it.

      Movable : Boolean;
      --  If True, a stored_Element can be moved in memory. This is very
      --  similar to Copyable, but no aliasing issue occurs, so this should be
      --  safe for access types.

   package Traits is
   end Traits;

end Conts.Elements;
