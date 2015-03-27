package body Memory is
   procedure Reset is
   begin
      Live := 0;
      Allocs := 0;
      Frees := 0;
      Reallocs := 0;
   end Reset;
end Memory;
