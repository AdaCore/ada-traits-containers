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

--  Bounded lists of constrained elements

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Definite;
with Conts.Lists.Cursors;
with Conts.Lists.Generics;
with Conts.Lists.Nodes.Bounded;

generic
   type Element_Type is private;
package Conts.Lists.Definite_Bounded is

   package Elements is new Conts.Elements.Definite (Element_Type);
   package Nodes is new Conts.Lists.Nodes.Bounded
      (Elements  => Elements.Traits,
       Base_Type => Ada.Finalization.Controlled);
   package Lists is new Conts.Lists.Generics (Nodes.Traits);

   subtype Cursor is Lists.Cursor;
   type List (Capacity : Count_Type) is
      new Lists.List (Capacity) with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   package Cursors is new Conts.Lists.Cursors (Lists, List);
end Conts.Lists.Definite_Bounded;
