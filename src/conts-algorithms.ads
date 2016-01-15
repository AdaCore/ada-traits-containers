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

with Conts.Cursors;

package Conts.Algorithms is

   --------------
   -- Count_If --
   --------------

   generic
      with package Cursors
         is new Conts.Cursors.Constant_Forward_Convert_Traits (<>);
   function Count_If_Convert
      (Self      : Cursors.Cursors.Container;
       Predicate : not null access function
          (E : Cursors.Element_Type) return Boolean)
      return Natural;
   --  Count the number of elements in the container that match the predicate

   generic
      with package Cursors is new Conts.Cursors.Constant_Forward_Traits (<>);
   function Count_If
      (Self      : Cursors.Container;
       Predicate : not null access function
          (E : Cursors.Return_Type) return Boolean)
      return Natural;
   --  Count the number of elements in the container that match the predicate

end Conts.Algorithms;
