------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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

--  Maps indexed by indefinite elements (strings for instance), containing
--  definite elements (records for instance).

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Elements.Indefinite_Ref;
with Conts.Maps.Generics;
with Conts.Maps.Cursors;
with Conts.Properties;

generic
   type Key_Type (<>) is private;
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with procedure Free (E : in out Key_Type) is null;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Maps.Indef_Def_Unbounded is

   package Keys is new Conts.Elements.Indefinite_Ref
     (Key_Type, Pool => Conts.Global_Pool, Free => Free);
   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);

   function "=" (Left : Key_Type; Right : Keys.Traits.Stored) return Boolean
     is (Left = Right.all) with Inline;

   package Maps is new Conts.Maps.Generics
     (Keys                => Keys.Traits,
      Elements            => Elements.Traits,
      Hash                => Hash,
      "="                 => "=",
      Probing             => Conts.Maps.Perturbation_Probing,
      Pool                => Conts.Global_Pool,
      Container_Base_Type => Container_Base_Type);

   subtype Cursor is Maps.Cursor;

   subtype Pair_Type is Maps.Pair_Type;
   function Key (P : Pair_Type) return Keys.Traits.Returned renames Maps.Key;
   function Value (P : Pair_Type) return Element_Type renames Maps.Value;

   type Map is new Maps.Map with null record
     with Constant_Indexing => Constant_Reference,
          Iterable => (First       => First_Primitive,
                       Next        => Next_Primitive,
                       Has_Element => Has_Element_Primitive,
                       Element     => Element_Primitive);

   function Constant_Reference
     (Self : Map; Position : Key_Type) return Element_Type
   is (Maps.Get (Self, Position)) with Inline;

   package Cursors is new Conts.Maps.Cursors (Maps);
   package Pair_Maps is new Conts.Properties.Read_Only_Maps
     (Maps.Map'Class, Cursor, Pair_Type, Maps.Pair);
   package Element_Maps is new Conts.Properties.Read_Only_Maps
     (Maps.Map'Class, Cursor, Element_Type, Maps.Element);
   package Returned_Maps renames Element_Maps;

end Conts.Maps.Indef_Def_Unbounded;
