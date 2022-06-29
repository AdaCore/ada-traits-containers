--
--  Copyright (C) 2016, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2012;
with GNAT.IO;   use GNAT.IO;

package body Asserts is

   -------------------------
   -- On_Assertion_Failed --
   -------------------------

   overriding procedure On_Assertion_Failed
      (Self     : Testsuite_Reporter;
       Msg      : String;
       Details  : String;
       Location : String;
       Entity   : String)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line
         ((if Msg = "" then "" else Msg & " ")
          & "(at " & Location & ", in " & Entity & ")"
          & ASCII.LF & "   " & Details);
   end On_Assertion_Failed;

end Asserts;
