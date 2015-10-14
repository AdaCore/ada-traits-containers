------------------------------------------------------------------------------
--                     Copyright (C) 2015, AdaCore                          --
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

with Ada.Text_IO;   use Ada.Text_IO;
with Perf_Support;  use Perf_Support;
with QGen;          use QGen;
with Report;        use Report;

procedure Perf is
   Ref_None : constant Performance_Counter := 1;
   Ref_Fill : constant Performance_Counter := 2;
   Ref_Loop : constant Performance_Counter := 3;

   Stdout : aliased Output;
begin
   Stdout.Setup
      (Counters_Count => 3,
       Columns        =>
          (Column_Title    => (new String'(""),         10, True,  Ref_None),
           Column_Fill     => (new String'("fill"),     6,  False, Ref_Fill),
           Column_Copy     => (new String'("copy"),     6,  True,  Ref_Fill),
           Column_Loop     => (new String'("loop"),     5,  False, Ref_Loop),
           Column_For_Of   => (new String'("for..of"),  8,  False, Ref_Loop),
           Column_Count_If => (new String'("count"),    5,  True,  Ref_Loop),
           Column_Allocate => (new String'("allocate"), 8,  False,
                               Last_Column_With_Test),
           Column_Allocs   => (new String'("allocs"),   8,  False, Ref_None),
           Column_Reallocs => (new String'("real"),     4,  False, Ref_None),
           Column_Frees    => (new String'("frees"),    8,  True,  Ref_None)));

   Put_Line ("+--------- lists of integers");
   Stdout.Print_Header;
   Test_Cpp_Int (To_Address (Stdout'Unchecked_Access));
   Test_Arrays_Int (Stdout);
   Test_Ada2012_Int (Stdout);
   Test_Ada2012_Int_No_Checks (Stdout);
   Test_Ada2012_Int_Indefinite (Stdout);
   Test_Tagged_Int (Stdout);
   Test_Lists_Int (Stdout);
   Test_Lists_Int_Indefinite (Stdout);
   Test_Lists_Int_Indefinite_SPARK (Stdout);
   Test_Lists_Bounded (Stdout);
   Test_Lists_Bounded_Limited (Stdout);
   Stdout.Reset;  ---  Stdout.Finish_Line to preserve percent

   New_Line;
   Put_Line ("+--------- lists of strings or std::string");
   Stdout.Print_Header;
   Test_Cpp_Str (To_Address (Stdout'Unchecked_Access));
   Test_Ada2012_Str (Stdout);
   Test_Ada2012_Str_No_Checks (Stdout);
   Test_Lists_Str (Stdout);
   Test_Lists_Str_Reference (Stdout);
   Stdout.Finish_Line;

   New_Line;
   Put_Line
      ("d/i: (in)definite b/u/s: (un)bounded/spark"
      & " (c/l): controlled/limited");
   Put_Line ("no: using pragma suppress(container_checks)");
   Put_Line ("(1): slower because Iterable aspect needs primitive operations");
   Put_Line ("(4): Using Reference_Type");

   Test_QGen;

   Stdout.Finalize;
end Perf;
