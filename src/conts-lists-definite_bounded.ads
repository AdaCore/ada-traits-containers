--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Bounded controlled lists of constrained elements
--  This package is compatible with SPARK.

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Lists.Generics;
with Conts.Lists.Storage.Bounded_Definite;
with Conts.Properties.SPARK;

generic
   type Element_Type is private;
   with procedure Free (E : in out Element_Type) is null;
package Conts.Lists.Definite_Bounded with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Definite
     (Element_Type, Free => Free);
   package Storage is new Conts.Lists.Storage.Bounded_Definite
      (Elements => Elements);
   package Lists is new Conts.Lists.Generics (Storage.Traits);

   subtype Cursor is Lists.Cursor;
   subtype List is Lists.List;

   subtype Element_Sequence is Lists.Impl.M.Sequence with Ghost;
   subtype Cursor_Position_Map is Lists.Impl.P_Map with Ghost;

   package Cursors renames Lists.Cursors;
   package Maps renames Lists.Maps;

   package Content_Models is new Conts.Properties.SPARK.Content_Models
        (Map_Type     => Lists.Base_List'Class,
         Element_Type => Element_Type,
         Model_Type   => Element_Sequence,
         Index_Type   => Lists.Impl.M.Extended_Index,
         Model        => Lists.Impl.Model,
         Get          => Lists.Impl.M.Get,
         First        => Lists.Impl.M.First,
         Last         => Lists.Impl.M.Last);

end Conts.Lists.Definite_Bounded;
