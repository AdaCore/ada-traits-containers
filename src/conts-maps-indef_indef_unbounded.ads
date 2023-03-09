--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Maps indexed by indefinite elements (strings for instance), containing
--  indefinite elements (class-wide for instance).

pragma Ada_2012;
with Conts.Elements.Indefinite;
with Conts.Maps.Generics;

generic
   type Key_Type (<>) is private;
   type Element_Type (<>) is private;
   type Container_Base_Type is abstract tagged limited private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function "=" (Left, Right : Key_Type) return Boolean is <>;
   with procedure Free (E : in out Key_Type) is null;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Maps.Indef_Indef_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Keys is new Conts.Elements.Indefinite
     (Key_Type, Pool => Conts.Global_Pool, Free => Free);
   package Elements is new Conts.Elements.Indefinite
     (Element_Type, Pool => Conts.Global_Pool, Free => Free);

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

   subtype Constant_Returned_Type is Impl.Constant_Returned_Type;
   subtype Constant_Returned_Key_Type is Impl.Constant_Returned_Key_Type;

   subtype Cursor is Impl.Cursor;
   subtype Map is Impl.Map;
   subtype Returned is Impl.Returned_Type;

   package Cursors renames Impl.Cursors;
   package Maps renames Impl.Maps;

end Conts.Maps.Indef_Indef_Unbounded;
