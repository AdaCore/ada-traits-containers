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
