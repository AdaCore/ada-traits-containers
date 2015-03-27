package Memory is
   Live     : Natural := 0;
   Allocs   : Natural := 0;
   Frees    : Natural := 0;
   Reallocs : Natural := 0;

   procedure Reset;
end Memory;
