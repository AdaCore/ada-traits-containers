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
