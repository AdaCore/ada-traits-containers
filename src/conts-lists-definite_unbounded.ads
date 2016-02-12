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

--  Unbounded lists of constrained elements.
--  Compared with standard Ada containers, this is saving half of the memory
--  allocations, so much more efficient in general.

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Lists.Generics;
with Conts.Lists.Cursors;
with Conts.Lists.Nodes.Unbounded;

generic
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
package Conts.Lists.Definite_Unbounded is

   package Elements is new Conts.Elements.Definite (Element_Type);
   package Nodes is new Conts.Lists.Nodes.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Container_Base_Type,
       Pool                => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Nodes.Traits);

   subtype Cursor is Lists.Cursor;
   type List is new Lists.List with null record
     with Constant_Indexing => Constant_Reference,
          Iterable          => (First       => First_Primitive,
                                Next        => Next_Primitive,
                                Has_Element => Has_Element_Primitive,
                                Element     => Element_Primitive);

   function Constant_Reference
     (Self : List; Position : Cursor) return Element_Type
     is (Lists.Element (Self, Position)) with Inline;

   package Cursors is new Conts.Lists.Cursors (Lists);
end Conts.Lists.Definite_Unbounded;
