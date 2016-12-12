------------------------------------------------------------------------------
--                     Copyright (C) 2016, AdaCore                          --
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

pragma Ada_2012;
with Conts.Elements;
with Conts.Lists.Generics;
with Conts.Lists.Storage;

generic

   with package Elements is new Conts.Elements.Traits
      (Element_Type => Integer, others => <>);
   with package Storage is new Conts.Lists.Storage.Traits
      (Elements => Elements, others => <>);
   with package Lists is new Conts.Lists.Generics
      (Storage => Storage);

   with function Image (Self : Elements.Constant_Returned_Type) return String;

package Support is

   procedure Test (L1, L2 : in out Lists.List);
   --  Perform various tests.
   --  All lists should be empty on input. This is used to handle bounded
   --  lists.

end Support;
