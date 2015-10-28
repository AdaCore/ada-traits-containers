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

with Perf_Support;    use Perf_Support;
with QGen;            use QGen;
with Report;          use Report;
with Generated_Tests; use Generated_Tests;
with Ada.Text_IO;     use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

procedure Perf is
   Stdout : aliased Output;
   S      : constant access Output'Class := Stdout'Access;
begin
   Test_Cpp_Int (Stdout'Address);
   Test_Arrays_Int (S);
   Test_Ada12_Definite_Unbounded_Integer (S);
   Test_Ada12_No_Checks_Definite_Unbounded_Integer (S);
   Test_Ada12_Indefinite_Unbounded_Integer (S);
   Test_Controlled_Indefinite_Unbounded_Integer (S);
   Test_Controlled_Definite_Unbounded_Integer (S);
   Test_Controlled_Definite_Bounded_Integer (S);
   Test_Limited_Definite_Bounded_Integer (S);
   Test_Limited_Indefinite_Spark_Unbounded_Spark_Integer (S);

   Test_Cpp_Str (Stdout'Address);
   Test_Ada12_Indefinite_Unbounded_String (S);
   Test_Ada12_No_Checks_Indefinite_Unbounded_String (S);
   Test_Controlled_Indefinite_Unbounded_String (S);
   Test_Controlled_Indefinite_Unbounded_Ref_String (S);
   Test_Controlled_Definite_Unbounded_Unbounded_String (S);
   Test_Controlled_Arrays_Unbounded_String (S);

   Test_QGen;
   Stdout.Display;

   Put_Line ("open file://" & Get_Current_Dir & "/index.html");
end Perf;
