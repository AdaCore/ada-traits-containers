with Ada.Text_IO;   use Ada.Text_IO;
with Perf_Support;  use Perf_Support;
with QGen;          use QGen;
with Report;        use Report;

procedure Perf is
   Ref_None : constant Performance_Counter := 1;
   Ref_Fill : constant Performance_Counter := 2;
   Ref_Loop : constant Performance_Counter := 3;
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
   Test_Cpp_Int;
   Test_Arrays_Int;
   Test_Ada2012_Int;
   Test_Ada2012_Int_Indefinite;
   Test_Tagged_Int;
   Test_Lists_Int;
   Test_Lists_Int_Indefinite;
   Test_Lists_Int_Indefinite_SPARK;
   Test_Lists_Bounded;
   Test_Lists_Bounded_Limited;
   Stdout.Reset;  ---  Stdout.Finish_Line to preserve percent

   New_Line;
   Put_Line ("+--------- lists of strings or std::string");
   Stdout.Print_Header;
   Test_Cpp_Str;
   Test_Ada2012_Str;
   Test_Lists_Str;
   Test_Lists_Str_Reference;
   Test_Lists_Str_Access;
   Stdout.Finish_Line;

   New_Line;
   Put_Line
      ("d/i: (in)definite b/u/s: (un)bounded/spark"
      & " (c/l): controlled/limited");
   Put_Line ("(1): slower because Iterable aspect needs primitive operations");
   Put_Line ("(2): Iterable does not support unconstrained elements");
   Put_Line ("(3): Using Stored_Element (less safe, user can free pointer)");
   Put_Line ("(4): Using Reference_Type (unconstrained type, slower)");

   Test_QGen;

   Stdout.Finalize;
end Perf;
