--
--  Copyright (C) 2015-2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Bounded controlled vectors of constrained elements

pragma Ada_2012;
with Conts.Elements.Definite;
with Conts.Vectors.Generics;
with Conts.Vectors.Storage.Bounded_Definite;
with Conts.Properties.SPARK;

generic
   type Index_Type is range <>;
   type Element_Type is private;
package Conts.Vectors.Definite_Bounded with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Definite (Element_Type);
   package Storage is new Conts.Vectors.Storage.Bounded_Definite
      (Elements => Elements);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Storage.Traits);

   subtype Vector is Vectors.Vector;
   subtype Cursor is Vectors.Cursor;

   package Cursors renames Vectors.Cursors;
   package Maps renames Vectors.Maps;

   subtype Element_Sequence is Vectors.Impl.M.Sequence with Ghost;

   procedure Swap
      (Self : in out Cursors.Forward.Container; Left, Right : Index_Type)
      renames Vectors.Swap;

   package Content_Models is new Conts.Properties.SPARK.Content_Models
        (Map_Type     => Vectors.Base_Vector'Class,
         Element_Type => Element_Type,
         Model_Type   => Element_Sequence,
         Index_Type   => Vectors.Impl.M.Extended_Index,
         Model        => Vectors.Impl.Model,
         Get          => Vectors.Impl.M.Get,
         First        => Vectors.Impl.M.First,
         Last         => Vectors.Impl.M.Last);

end Conts.Vectors.Definite_Bounded;
