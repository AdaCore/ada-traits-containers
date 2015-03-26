with Ada.Text_IO;   use Ada.Text_IO;
with Perf_Support;  use Perf_Support;

procedure Perf is
   procedure Fewer_Items;
   procedure Fewer_Items is
   begin
      if Small_Items_Count /= Items_Count then
         Put_Line (" fewer items");
      else
         New_Line;
      end if;
   end Fewer_Items;

begin
   Put_Line
      ("+--- lists of integers"
       & " ---------------------------------------------------+");
   Put_Line
      ("Type              "
       & "| fill        |explicit loop| for..of     | count_if    |");
   Put_Line
      ("+-----------------+-------------+-------------"
       & "+-------------+-------------+");
   Put ("Lists (u-d-c)     |"); Test_Lists_Int;     New_Line;
   Put ("Lists (u-i-c)     |"); Test_Lists_Int_Indefinite;     New_Line;
   Put ("Lists (b-d-c)     |"); Test_Lists_Bounded;         Fewer_Items;
   Put ("Lists (b-d-l)     |"); Test_Lists_Bounded_Limited; Fewer_Items;
   Put ("C++               |"); Test_Cpp_Int;       New_Line;
   Put ("Arrays            |"); Test_Arrays_Int;    Fewer_Items;
   Put ("Ada2012           |"); Test_Ada2012_Int;   New_Line;
   Put ("Ada2012 (indef)   |"); Test_Ada2012_Int_Indefinite;   New_Line;
   Put ("Tagged            |"); Test_Tagged_Int;    New_Line;

   New_Line;
   Put_Line
      ("+--- lists of strings (test if first char is s)"
       & " --------------------------+");
   Put ("Lists (u-i-c)     |"); Test_Lists_Str;        New_Line;
   Put ("Lists (u-i-c) (3) |"); Test_Lists_Str_Access; New_Line;
   Put ("C++               |"); Test_Cpp_Str;          New_Line;
   Put ("Ada2012           |"); Test_Ada2012_Str;      New_Line;

   New_Line;
   Put_Line
      ("b-/u-: (un)bounded  (d-/i-): (in)definite  (c/l): controlled/limited");
   Put_Line ("(1): slower because Iterable aspect needs primitive operations");
   Put_Line ("(2): Iterable does not support unconstrained elements");
   Put_Line ("(3): Using Stored_Element instead of Element");
end Perf;
