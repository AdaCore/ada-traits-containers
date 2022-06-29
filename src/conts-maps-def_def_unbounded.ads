--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Maps indexed by definite elements (integers for instance), containing
--  definite elements (records for instance).

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Maps.Generics;

generic
   type Key_Type is private;
   type Element_Type is private;
   type Container_Base_Type is abstract tagged limited private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with procedure Free (E : in out Key_Type) is null;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Maps.Def_Def_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Keys is new Conts.Elements.Definite
     (Key_Type, Free => Free);
   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);
   package Impl is new Conts.Maps.Generics
     (Keys                => Keys.Traits,
      Elements            => Elements.Traits,
      Hash                => Hash,
      "="                 => "=",
      Probing             => Conts.Maps.Perturbation_Probing,
      Pool                => Conts.Global_Pool,
      Container_Base_Type => Container_Base_Type);

   subtype Constant_Returned_Type is Impl.Constant_Returned_Type;
   subtype Constant_Returned_Key_Type is Impl.Constant_Returned_Key_Type;

   subtype Cursor is Impl.Cursor;
   subtype Map is Impl.Map;

   package Cursors renames Impl.Cursors;
   package Maps renames Impl.Maps;

end Conts.Maps.Def_Def_Unbounded;
