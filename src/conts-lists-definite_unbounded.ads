--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Unbounded controlled lists of constrained elements.
--  Compared with standard Ada containers, this is saving half of the memory
--  allocations, so much more efficient in general.

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Definite;
with Conts.Lists.Generics;
with Conts.Lists.Storage.Unbounded;

generic
   type Element_Type is private;
package Conts.Lists.Definite_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Definite (Element_Type);
   package Storage is new Conts.Lists.Storage.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Ada.Finalization.Controlled,
       Pool                => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Storage.Traits);

   subtype Cursor is Lists.Cursor;
   subtype List is Lists.List;

   package Cursors renames Lists.Cursors;
   package Maps renames Lists.Maps;

end Conts.Lists.Definite_Unbounded;
