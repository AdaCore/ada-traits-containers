--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

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
