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

--  Unbounded lists of unconstrained elements.
--  Cursors are indexes into an array, to be able to write post-conditions
--  and for added safety

pragma Ada_2012;
with Conts.Elements.Indefinite_SPARK;
with Conts.Lists.Storage.Unbounded_SPARK;
with Conts.Lists.Generics;
with Conts.Properties.SPARK;

generic
   type Element_Type (<>) is private;
   --  Element_Type must not be a controlled type that needs to be
   --  Adjusted when it is moved in memory, since the list will use the
   --  realloc() system call.

package Conts.Lists.Indefinite_Unbounded_SPARK with SPARK_Mode is

   pragma Assertion_Policy
      (Pre => Suppressible, Ghost => Suppressible, Post => Ignore);

   package Elements is new Conts.Elements.Indefinite_SPARK
      (Element_Type, Pool => Conts.Global_Pool);
   package Storage is new Conts.Lists.Storage.Unbounded_SPARK
      (Elements            => Elements.Traits,
       Container_Base_Type => Limited_Base);
   package Lists is new Conts.Lists.Generics (Storage.Traits);
   subtype Constant_Returned is Elements.Traits.Constant_Returned;

   subtype Cursor is Lists.Cursor;
   subtype List is Lists.List;

   subtype Element_Sequence is Lists.Impl.M.Sequence with Ghost;
   subtype Cursor_Position_Map is Lists.Impl.P_Map with Ghost;

   use type Element_Sequence;

   function Copy (Self : List'Class) return List'Class;
   --  Return a deep copy of Self
   --  Complexity: O(n)

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

end Conts.Lists.Indefinite_Unbounded_SPARK;
