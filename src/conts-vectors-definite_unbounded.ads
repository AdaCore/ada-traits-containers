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

--  Unbounded Vectors of constrained elements.
--  Compared with standard Ada containers, this is saving half of the memory
--  allocations, so much more efficient in general.

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Vectors.Generics;
with Conts.Vectors.Cursors;
with Conts.Vectors.Storage.Unbounded;
with Conts.Properties;

generic
   type Index_Type is (<>);
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Vectors.Definite_Unbounded is

   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);
   package Storage is new Conts.Vectors.Storage.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Container_Base_Type,
       Resize_Policy       => Conts.Vectors.Resize_1_5);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Storage.Traits);

   type Vector is new Vectors.Vector with null record
      with Constant_Indexing => Constant_Reference,
           Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   function Constant_Reference
     (Self : Vector; Position : Index_Type) return Element_Type
     is (Vectors.Element (Self, Position)) with Inline;

   package Cursors is new Conts.Vectors.Cursors (Vectors);
   subtype Cursor is Vectors.Cursor;
   package Element_Maps is new Conts.Properties.Read_Only_Maps
     (Vectors.Vector'Class, Cursor, Element_Type, Vectors.Element);
   package Returned_Maps renames Element_Maps;

   function "<=" (Idx : Index_Type; Count : Count_Type) return Boolean
      renames Vectors."<=";
   procedure Swap
      (Self : in out Vectors.Vector'Class; Left, Right : Index_Type)
      renames Vectors.Swap;

end Conts.Vectors.Definite_Unbounded;
