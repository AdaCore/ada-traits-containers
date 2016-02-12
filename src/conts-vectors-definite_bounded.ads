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

--  Bounded vectors of constrained elements

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Vectors.Cursors;
with Conts.Vectors.Generics;
with Conts.Vectors.Nodes.Bounded;

generic
   type Index_Type is range <>;
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
package Conts.Vectors.Definite_Bounded is

   package Elements is new Conts.Elements.Definite (Element_Type);
   package Nodes is new Conts.Vectors.Nodes.Bounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Container_Base_Type);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Nodes.Traits);

   subtype Cursor is Vectors.Cursor;
   type Vector (Capacity : Count_Type) is
      new Vectors.Vector (Capacity) with null record
      with Constant_Indexing => Constant_Reference,
           Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   function Copy (Self : Vector'Class) return Vector'Class;
   --  Return a deep copy of Self
   --  Complexity: O(n)

   function Constant_Reference
     (Self : Vector; Position : Index_Type) return Element_Type
     is (Vectors.Element (Self, Position)) with Inline;

   package Cursors is new Conts.Vectors.Cursors (Vectors);

   function "<=" (Idx : Index_Type; Count : Count_Type) return Boolean
      renames Vectors."<=";
   procedure Swap
      (Self : in out Vectors.Vector'Class; Left, Right : Index_Type)
      renames Vectors.Swap;

end Conts.Vectors.Definite_Bounded;
