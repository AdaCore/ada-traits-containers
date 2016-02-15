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

--  Unbounded lists of strings.
--  A special allocation strategy is used for strings, so that small strings
--  are directly stored in the list's node, and do not require memory
--  allocation. This might make things faster in some cases, at the cost of
--  using more memory since the nodes are bigger.
--  Consider using Conts.Lists.Indefinite_Unbounded_Ref for another list
--  usable with strings.

pragma Ada_2012;
with Ada.Finalization;
with Conts.Elements.Arrays;
with Conts.Lists.Generics;
with Conts.Lists.Cursors;
with Conts.Lists.Nodes.Unbounded;
with Conts.Properties;

package Conts.Lists.Strings is

   package Elements is new Conts.Elements.Arrays
      (Positive, Character, String, Conts.Global_Pool);
   package Nodes is new Conts.Lists.Nodes.Unbounded
      (Elements            => Elements.Traits,
       Container_Base_Type => Ada.Finalization.Controlled,
       Pool                => Conts.Global_Pool);
   package Lists is new Conts.Lists.Generics (Nodes.Traits);

   subtype Cursor is Lists.Cursor;
   type List is new Lists.List with null record
      with Iterable => (First       => First_Primitive,
                        Next        => Next_Primitive,
                        Has_Element => Has_Element_Primitive,
                        Element     => Element_Primitive);

   package Cursors is new Conts.Lists.Cursors (Lists);

   function Element
     (Self : Lists.List'Class; Position : Cursor) return String
   is (Lists.Element (Self, Position).Element.all);

   package Element_Maps is new Conts.Properties.Read_Only_Maps
     (Lists.List'Class, Cursor, String, Element);
   package Return_Maps is new Conts.Properties.Read_Only_Maps
     (Lists.List'Class, Cursor, Elements.Traits.Returned, Lists.Element);

end Conts.Lists.Strings;
