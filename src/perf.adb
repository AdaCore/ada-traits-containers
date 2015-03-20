with Ada.Text_IO;   use Ada.Text_IO;
with Perf_Support;  use Perf_Support;

procedure Perf is
begin
   Put_Line ("+--- lists of integers ---------------------------------------------------+");
   Put_Line ("Type              | fill        |explicit loop| for..of     | count_if    |");
   Put_Line ("+-----------------+-------------+-------------+-------------+-------------+");
   Put ("Lists (definite)  |"); Test_Lists_Int;     New_Line;
   Put ("Lists (indefinite)|"); Test_Lists_Int_Indefinite;     New_Line;
   Put ("C++               |"); Test_Cpp_Int;       New_Line;
   Put ("Arrays            |"); Test_Arrays_Int;    New_Line;
   Put ("Ada2012           |"); Test_Ada2012_Int;   New_Line;
   Put ("Ada2012 (indef)   |"); Test_Ada2012_Int_Indefinite;   New_Line;
   Put ("Tagged            |"); Test_Tagged_Int;    New_Line;

   New_Line;
   Put_Line ("+--- lists of strings ----------------------------------------------------+");
   Put ("Lists             |"); Test_Lists_Str;     New_Line;
   Put ("C++               |"); Test_Cpp_Str;       New_Line;
   Put ("Ada2012           |"); Test_Ada2012_Str;   New_Line;

   New_Line;
   Put_Line ("(1): slower because Iterable aspect needs primitive operations");
   Put_Line ("(2): Iterable does not support unconstrained elements");
end Perf;
