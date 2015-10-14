------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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

--  Unbounded lists of unconstrained elements.
--  These elements are returned as reference types, which avoids extra copies
--  of the element, but is sometimes slightly less convenient to use.

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Indefinite_Ref;
with Conts.Lists.Generics;
with Conts.Lists.Cursors;
with Conts.Lists.Nodes.Unbounded;

generic
   type Element_Type (<>) is private;
package Conts.Lists.Indefinite_Unbounded_Ref is

   package Elements is new Conts.Elements.Indefinite_Ref
      (Element_Type, Pool => Conts.Global_Pool);
   package Nodes is new Conts.Lists.Nodes.Unbounded
      (Elements  => Elements.Traits,
       Base_Type => Ada.Finalization.Controlled,
       Pool      => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Nodes.Traits);

   subtype Ref_Type is Elements.Ref_Type;
   subtype Cursor is Lists.Cursor;
   type List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   --  ??? Should we provide a Copy function ?
   --  This cannot be provided in the generic package, since the type could
   --  be constrained and/or limited, so it has to be provided in all child
   --  packages. However, when the type is controlled it is much easier to
   --  just use the standard assignment operator.

   package Cursors is new Conts.Lists.Cursors (Lists, List);
end Conts.Lists.Indefinite_Unbounded_Ref;
