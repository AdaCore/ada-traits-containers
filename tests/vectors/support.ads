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
with Conts.Vectors.Generics;
with Conts.Vectors.Storage;

package Support is
   subtype Index_Type is Positive;

   generic
      with package Elements is new Conts.Elements.Traits
         (Element_Type => Integer, others => <>);
      with package Storage is new Conts.Vectors.Storage.Traits
         (Elements => Elements, others => <>);
      with package Vectors is new Conts.Vectors.Generics
         (Storage => Storage, Index_Type => Index_Type);
      with function Image (Self : Elements.Constant_Returned) return String;
   procedure Test (V1, V2 : in out Vectors.Vector);
   --  Perform various tests.
   --  All vectors should be empty on input. This is used to handle bounded
   --  vectors.

end Support;
