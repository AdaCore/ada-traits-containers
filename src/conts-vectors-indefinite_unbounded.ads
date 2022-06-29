--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Unbounded controlled vectors of unconstrained elements

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Indefinite;
with Conts.Vectors.Generics;
with Conts.Vectors.Storage.Unbounded;

generic
   type Index_Type is (<>);
   type Element_Type (<>) is private;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Vectors.Indefinite_Unbounded is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Indefinite
      (Element_Type, Free => Free, Pool => Conts.Global_Pool);
   package Storage is new Conts.Vectors.Storage.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Ada.Finalization.Controlled,
       Resize_Policy       => Conts.Vectors.Resize_1_5);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Storage.Traits);

   subtype Vector is Vectors.Vector;
   subtype Cursor is Vectors.Cursor;
   subtype Constant_Returned is Elements.Traits.Constant_Returned;
   No_Element : Cursor renames Vectors.No_Element;
   No_Index   : Index_Type renames Vectors.No_Index;

   package Cursors renames Vectors.Cursors;
   package Maps renames Vectors.Maps;

   procedure Swap
     (Self : in out Cursors.Forward.Container; Left, Right : Index_Type)
       renames Vectors.Swap;

end Conts.Vectors.Indefinite_Unbounded;
