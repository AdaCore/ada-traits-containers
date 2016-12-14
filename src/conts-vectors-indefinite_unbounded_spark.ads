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

--  Unbounded controlled vectors of unconstrained elements

pragma Ada_2012;
with Conts.Elements.Indefinite_SPARK;
with Conts.Vectors.Generics;
with Conts.Vectors.Storage.Unbounded;
with Conts.Properties.SPARK;

generic
   type Index_Type is (<>);
   type Element_Type (<>) is private;
package Conts.Vectors.Indefinite_Unbounded_SPARK with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Indefinite_SPARK
      (Element_Type, Pool => Conts.Global_Pool);
   package Storage is new Conts.Vectors.Storage.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Limited_Base,
       Resize_Policy       => Conts.Vectors.Resize_1_5);
   package Vectors is new Conts.Vectors.Generics (Index_Type, Storage.Traits);

   subtype Vector is Vectors.Vector;
   subtype Cursor is Vectors.Cursor;
   subtype Constant_Returned is Elements.Traits.Constant_Returned;
   subtype Extended_Index is Vectors.Extended_Index;

   package Cursors renames Vectors.Cursors;
   package Maps renames Vectors.Maps;

   subtype Element_Sequence is Vectors.Impl.M.Sequence with Ghost;

   use type Element_Sequence;

   function Copy (Self : Vector'Class) return Vector'Class;
   --  Return a deep copy of Self

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

end Conts.Vectors.Indefinite_Unbounded_SPARK;
