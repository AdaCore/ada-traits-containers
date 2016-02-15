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

--  Bounded lists of constrained elements

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Lists.Cursors;
with Conts.Lists.Generics;
with Conts.Lists.Storage.Bounded;
with Conts.Properties;

generic
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Lists.Definite_Bounded is

   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);
   package Storage is new Conts.Lists.Storage.Bounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Container_Base_Type);
   package Lists is new Conts.Lists.Generics (Storage.Traits);

   subtype Cursor is Lists.Cursor;
   type List (Capacity : Count_Type) is
      new Lists.List (Capacity) with null record
       with Constant_Indexing => Constant_Reference,
            Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   function Constant_Reference
     (Self : List; Position : Cursor) return Element_Type
     is (Lists.Element (Self, Position)) with Inline;

   function Copy (Self : List'Class) return List'Class;
   --  Return a deep copy of Self
   --  Complexity: O(n)

   package Cursors is new Conts.Lists.Cursors (Lists);
   package Element_Maps is new Conts.Properties.Read_Only_Maps
     (Lists.List'Class, Cursor, Element_Type, Lists.Element);
   package Returned_Maps renames Element_Maps;

end Conts.Lists.Definite_Bounded;
