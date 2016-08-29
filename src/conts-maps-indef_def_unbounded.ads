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
with Conts.Elements.Indefinite;
with Conts.Maps.Generics;

generic
   type Key_Type (<>) is private;
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with procedure Free (E : in out Key_Type) is null;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Maps.Indef_Def_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Keys is new Conts.Elements.Indefinite
     (Key_Type, Pool => Conts.Global_Pool, Free => Free);
   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);

   function "=" (Left : Key_Type; Right : Keys.Traits.Stored) return Boolean
     is (Left = Right.all) with Inline;

   package Impl is new Conts.Maps.Generics
     (Keys                => Keys.Traits,
      Elements            => Elements.Traits,
      Hash                => Hash,
      "="                 => "=",
      Probing             => Conts.Maps.Perturbation_Probing,
      Pool                => Conts.Global_Pool,
      Container_Base_Type => Container_Base_Type);

   subtype Cursor is Impl.Cursor;
   subtype Map is Impl.Map;

   package Cursors renames Impl.Cursors;
   package Maps renames Impl.Maps;

end Conts.Maps.Indef_Def_Unbounded;
