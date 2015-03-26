with Ada.Text_IO;   use Ada.Text_IO;
with Perf_Support;  use Perf_Support;

procedure Perf is
begin
   Put_Line ("+--------- lists of integers");
   Stdout.Start_Line ("");
   Stdout.Print_Not_Run ("fill");
   Stdout.Print_Not_Run ("explicit loop");
   Stdout.Print_Not_Run ("for..of");
   Stdout.Print_Not_Run ("count_if");
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
   Stdout.Start_Line ("");
   Stdout.Print_Not_Run ("fill");
   Stdout.Print_Not_Run ("explicit loop");
   Stdout.Print_Not_Run ("for..of");
   Stdout.Print_Not_Run ("count_if");
   Test_Cpp_Str;
   Test_Ada2012_Str;
   Test_Lists_Str;
   Test_Lists_Str_Access;
   Stdout.Finish_Line;

   New_Line;
   Put_Line
      ("b-/u-: (un)bounded  (d-/i-): (in)definite"
      & " (c/l/s): controlled/limited/spark");
   Put_Line ("(1): slower because Iterable aspect needs primitive operations");
   Put_Line ("(2): Iterable does not support unconstrained elements");
   Put_Line ("(3): Using Stored_Element instead of Element");
end Perf;
