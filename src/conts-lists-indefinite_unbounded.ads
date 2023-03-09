--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unbounded controlled lists of unconstrained elements

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Indefinite;
with Conts.Lists.Generics;
with Conts.Lists.Storage.Unbounded;

generic
   type Element_Type (<>) is private;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Lists.Indefinite_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Indefinite
      (Element_Type, Free => Free, Pool => Conts.Global_Pool);
   package Storage is new Conts.Lists.Storage.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Ada.Finalization.Controlled,
       Pool                => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Storage.Traits);

   subtype Cursor is Lists.Cursor;
   subtype List is Lists.List;
   subtype Constant_Returned is Elements.Traits.Constant_Returned;

   package Cursors renames Lists.Cursors;
   package Maps renames Lists.Maps;

end Conts.Lists.Indefinite_Unbounded;
