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

with GNATCOLL.Asserts;   use GNATCOLL.Asserts;
with Conts;              use Conts;

package Asserts is

   type Testsuite_Reporter is new Error_Reporter with null record;
   overriding procedure On_Assertion_Failed
      (Self     : Testsuite_Reporter;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String);

   Reporter : Testsuite_Reporter;

   package Testsuite_Asserts is new GNATCOLL.Asserts.Asserts
      (Reporter, Enabled => True);
   use Testsuite_Asserts;
   package Integers is new Compare (Integer, Integer'Image);
   package Booleans is new Compare (Boolean, Boolean'Image);
   package Counts   is new Compare (Count_Type, Count_Type'Image);
end Asserts;
